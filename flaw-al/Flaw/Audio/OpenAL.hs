{-|
Module: Flaw.Audio.OpenAL
Description: OpenAL audio implementation.
License: MIT
-}

{-# LANGUAGE CPP, TypeFamilies #-}

module Flaw.Audio.OpenAL
	( AlDevice(..)
	, createAlDevice
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as B
import Data.Foldable
import Data.IORef
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Audio
import Flaw.Audio.OpenAL.FFI
import Flaw.Book
import Flaw.Exception
import Flaw.Flow
import Flaw.Math

data AlDevice = AlDevice
	{ alDeviceFlow :: {-# UNPACK #-} !Flow
	-- | Operations to be executed during next tick.
	, alDeviceDeferredOperationsVar :: {-# UNPACK #-} !(TVar [IO ()])
	-- | Operations repeatedly executed during ticks.
	, alDeviceRepeatOperationsRef :: {-# UNPACK #-} !(IORef [(ALuint, IO ())])
	}

data AlSoundSource
	= AlBufferedSoundSource
		{ alSoundSourceBufferName :: {-# UNPACK #-} !ALuint
		}
	| AlStreamingSoundSource
		{ alSoundSourceStream :: !(IO ((SoundFormat, TVar BL.ByteString), IO ()))
		}

data AlStreamingState = AlStreamingState
	{ alStreamingStateBusyBuffers :: [ALuint]
	, alStreamingStateFreeBuffers :: [ALuint]
	}

instance Device AlDevice where
	data SoundId AlDevice = AlSoundId
		{ alSoundDevice :: !AlDevice
		, alSoundSource :: !AlSoundSource
		}
	data SoundPlayerId AlDevice = AlSoundPlayerId
		{ alSoundPlayerDevice :: !AlDevice
		, alSoundPlayerSourceName :: {-# UNPACK #-} !ALuint
		}

	createSound device@AlDevice
		{ alDeviceFlow = flow
		} format@SoundFormat
		{ soundFormatSamplesPerSecond = samplesPerSecond
		} bytes = describeException "failed to create OpenAL sound" $ runInFlow flow $ withSpecialBook $ \bk -> do
		-- allocate buffer name
		bufferName <- alloca $ \bufferNamePtr -> do
			alGenBuffers 1 bufferNamePtr
			peek bufferNamePtr
		alCheckErrors0 "gen buffer"
		book bk $ return ((), atomically $ alDeferredOperation device $ with bufferName $ alDeleteBuffers 1)

		-- upload data
		B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) ->
			alBufferData bufferName (alConvertFormat format) (castPtr bytesPtr) (fromIntegral bytesLen) (fromIntegral samplesPerSecond)
		alCheckErrors0 "buffer data"

		alCheckErrors1 "create sound"
		return AlSoundId
			{ alSoundDevice = device
			, alSoundSource = AlBufferedSoundSource
				{ alSoundSourceBufferName = bufferName
				}
			}

	createStreamingSound device stream = return (AlSoundId
		{ alSoundDevice = device
		, alSoundSource = AlStreamingSoundSource
			{ alSoundSourceStream = stream
			}
		}, return ())

	createSoundPlayer AlSoundId
		{ alSoundDevice = device@AlDevice
			{ alDeviceFlow = flow
			, alDeviceRepeatOperationsRef = repeatOperationsRef
			}
		, alSoundSource = soundSource
		} = describeException "failed to create OpenAL sound player" $ runInFlow flow $ withSpecialBook $ \bk -> do
		-- allocate source name
		sourceName <- alloca $ \sourceNamePtr -> do
			alGenSources 1 sourceNamePtr
			peek sourceNamePtr
		alCheckErrors0 "gen source"
		book bk $ return ((), atomically $ alDeferredOperation device $ do
			alSourceStop sourceName
			with sourceName $ alDeleteSources 1
			)

		-- setup sound source
		case soundSource of
			AlBufferedSoundSource
				{ alSoundSourceBufferName = bufferName
				} -> do
				-- for buffered sound just set buffer for the source
				alSourcei sourceName AL_BUFFER (fromIntegral bufferName)
				alCheckErrors0 "set source buffer"
			AlStreamingSoundSource
				{ alSoundSourceStream = stream
				} -> do
				-- create a few buffers for streaming
				buffers <- forM [1..2 :: Int] $ \_i -> do
					-- allocate buffer name
					bufferName <- alloca $ \bufferNamePtr -> do
						alGenBuffers 1 bufferNamePtr
						peek bufferNamePtr
					alCheckErrors0 "gen buffer"
					book bk $ return (bufferName, atomically $ alDeferredOperation device $ with bufferName $ alDeleteBuffers 1)
				-- start streaming
				(format@SoundFormat
					{ soundFormatSamplesPerSecond = samplesPerSecond
					, soundFormatSampleType = sampleType
					, soundFormatChannelsCount = channelsCount
					}, bytesVar) <- book bk stream
				-- create state ref
				stateRef <- newIORef AlStreamingState
					{ alStreamingStateFreeBuffers = buffers
					, alStreamingStateBusyBuffers = []
					}
				-- some arbitrary size of a single streaming buffer
				let bufferTime = 1 -- second
				let bufferSize = bufferTime * channelsCount * samplesPerSecond * soundSampleSize sampleType
				-- repeat operation for feeding buffers
				let feed = do
					-- get number of processed buffers
					processedCount <- alloca $ \processedCountPtr -> do
						alGetSourcei sourceName AL_BUFFERS_PROCESSED processedCountPtr
						fromIntegral <$> peek processedCountPtr
					-- if there're some, unqueue them
					when (processedCount > 0) $ do
						state@AlStreamingState
							{ alStreamingStateFreeBuffers = freeBuffers
							, alStreamingStateBusyBuffers = busyBuffers
							} <- readIORef stateRef
						let (buffersToUnqueue, restBusyBuffers) = splitAt processedCount busyBuffers
						withArray buffersToUnqueue $ alSourceUnqueueBuffers sourceName (fromIntegral processedCount)
						alCheckErrors0 "unqueue buffers"
						writeIORef stateRef state
							{ alStreamingStateFreeBuffers = freeBuffers ++ buffersToUnqueue
							, alStreamingStateBusyBuffers = restBusyBuffers
							}
					-- get number of free buffers
					state@AlStreamingState
						{ alStreamingStateFreeBuffers = freeBuffers
						, alStreamingStateBusyBuffers = busyBuffers
						} <- readIORef stateRef
					let freeBuffersCount = length freeBuffers
					-- if there're at least one free buffer, try to pull data from stream and enqueue it
					when (freeBuffersCount > 0) $ do
						-- get bytes
						(feedBytes, buffersToFeedCount) <- atomically $ do
							bytes <- readTVar bytesVar
							let bytesLength = fromIntegral $ BL.length bytes
							let buffersToFeedCount = bytesLength `quot` bufferSize
							if buffersToFeedCount > 0 then do
								let (feedBytes, restBytes) = BL.splitAt (fromIntegral $ buffersToFeedCount * bufferSize) bytes
								writeTVar bytesVar restBytes
								return (feedBytes, buffersToFeedCount)
							else return (BL.empty, 0)
						-- if there're some data
						when (buffersToFeedCount > 0) $ do
							-- put data into buffers
							let
								(freeBuffersToFeed, restFreeBuffers) = splitAt buffersToFeedCount freeBuffers
								feedBuffer (bufferName : restBufferNames) tailFeedBytes = do
									let (chunkBytes, restTailFeedBytes) = BL.splitAt (fromIntegral bufferSize) tailFeedBytes
									B.unsafeUseAsCString (BL.toStrict chunkBytes) $ \feedBytesPtr ->
										alBufferData bufferName (alConvertFormat format) (castPtr feedBytesPtr) (fromIntegral bufferSize) (fromIntegral samplesPerSecond)
									alCheckErrors0 "feed buffer"
									feedBuffer restBufferNames restTailFeedBytes
								feedBuffer [] _ = return ()
							feedBuffer freeBuffersToFeed feedBytes
							-- enqueue them
							withArray freeBuffersToFeed $ alSourceQueueBuffers sourceName (fromIntegral buffersToFeedCount)
							alCheckErrors0 "queue buffers"
							-- change state
							writeIORef stateRef state
								{ alStreamingStateFreeBuffers = restFreeBuffers
								, alStreamingStateBusyBuffers = busyBuffers ++ freeBuffersToFeed
								}
				-- register operation
				modifyIORef' repeatOperationsRef ((sourceName, feed) :)
				book bk $ return ((), atomically $ alDeferredOperation device $ modifyIORef' repeatOperationsRef $ filter ((/= sourceName) . fst))

		alCheckErrors1 "create sound player"
		return AlSoundPlayerId
			{ alSoundPlayerDevice = device
			, alSoundPlayerSourceName = sourceName
			}

	tickAudio device@AlDevice
		{ alDeviceFlow = flow
		, alDeviceRepeatOperationsRef = repeatOperationsRef
		} = runInFlow flow $ do
		alRunDeferredOperations device
		join $ sequence_ . map snd <$> readIORef repeatOperationsRef

	playSound AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		} = alDeferredOperation device $ do
		alSourcePlay sourceName
		alCheckErrors0 "play source"

	pauseSound AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		} = alDeferredOperation device $ do
		alSourcePause sourceName
		alCheckErrors0 "pause source"

	stopSound AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		} = alDeferredOperation device $ do
		alSourceStop sourceName
		alCheckErrors0 "stop source"

	setSoundPosition AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		} (Vec3 x y z) = alDeferredOperation device $ do
		alSource3f sourceName AL_POSITION (realToFrac x) (realToFrac y) (realToFrac z)
		alCheckErrors0 "set sound position"

	setSoundDirection AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		} (Vec3 x y z) = alDeferredOperation device $ do
		alSource3f sourceName AL_DIRECTION (realToFrac x) (realToFrac y) (realToFrac z)
		alCheckErrors0 "set sound direction"

	setSoundVelocity AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		} (Vec3 x y z) = alDeferredOperation device $ do
		alSource3f sourceName AL_VELOCITY (realToFrac x) (realToFrac y) (realToFrac z)
		alCheckErrors0 "set sound velocity"

createAlDevice :: IO (AlDevice, IO ())
createAlDevice = describeException "failed to create OpenAL device" $ withSpecialBook $ \bk -> do

	-- create flow for operations
	flow <- book bk newFlowOS
	-- create var for deferred operations
	deferredOperationsVar <- newTVarIO []
	-- create ref for repeat operations
	repeatOperationsRef <- newIORef []

	-- device struct
	let device = AlDevice
		{ alDeviceFlow = flow
		, alDeviceDeferredOperationsVar = deferredOperationsVar
		, alDeviceRepeatOperationsRef = repeatOperationsRef
		}

	-- open audio device
	devicePtr <- alcOpenDevice nullPtr
	when (devicePtr == nullPtr) $ throwIO $ DescribeFirstException "failed to open device"
	book bk $ return ((), do
		-- run deferred operations, as freeing of some objects might be deferred
		alRunDeferredOperations device
		-- close OpenAL device
		void $ alcCloseDevice devicePtr
		)

	runInFlow flow $ do
		-- create context
		contextPtr <- alcCreateContext devicePtr nullPtr
		when (contextPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to create context"
		book bk $ return ((), runInFlow flow $ do
			void $ alcMakeContextCurrent nullPtr
			alcDestroyContext contextPtr
			)

		-- make context current
		void $ alcMakeContextCurrent contextPtr
		alCheckErrors1 "make context current"

	return device

alConvertFormat :: SoundFormat -> ALenum
alConvertFormat format = case format of
	SoundFormat
		{ soundFormatSampleType = SoundSampleByte
		, soundFormatChannelsCount = 1
		} -> AL_FORMAT_MONO8
	SoundFormat
		{ soundFormatSampleType = SoundSampleShort
		, soundFormatChannelsCount = 1
		} -> AL_FORMAT_MONO16
	SoundFormat
		{ soundFormatSampleType = SoundSampleByte
		, soundFormatChannelsCount = 2
		} -> AL_FORMAT_STEREO8
	SoundFormat
		{ soundFormatSampleType = SoundSampleShort
		, soundFormatChannelsCount = 2
		} -> AL_FORMAT_STEREO16
	_ -> error "unsupported sound format"

alDeferredOperation :: AlDevice -> IO () -> STM ()
alDeferredOperation AlDevice
	{ alDeviceDeferredOperationsVar = deferredOperationsVar
	} operation = modifyTVar' deferredOperationsVar (operation :)

alRunDeferredOperations :: AlDevice -> IO ()
alRunDeferredOperations AlDevice
	{ alDeviceDeferredOperationsVar = deferredOperationsVar
	} = join $ atomically $ do
	deferredOperations <- readTVar deferredOperationsVar
	writeTVar deferredOperationsVar []
	return $ do
		foldrM const () deferredOperations
		alCheckErrors1 "deferred operations"

-- | Check for OpenAL errors, throw an exception if there's some.
alCheckErrors :: String -> IO ()
alCheckErrors msg = do
	firstError <- alGetError
	unless (firstError == AL_NO_ERROR) $ do
		let f restErrors = do
			nextError <- alGetError
			if nextError == AL_NO_ERROR then return restErrors
			else f $ nextError : restErrors
		errors <- f [firstError]
		throwIO $ DescribeFirstException $ show ("OpenAL error", msg, errors)

{-# INLINE alCheckErrors0 #-}
alCheckErrors0 :: String -> IO ()
#if 0 >= AL_ERROR_LEVEL
alCheckErrors0 = alCheckErrors
#else
alCheckErrors0 _ = return ()
#endif

{-# INLINE alCheckErrors1 #-}
alCheckErrors1 :: String -> IO ()
#if 1 >= AL_ERROR_LEVEL
alCheckErrors1 = alCheckErrors
#else
alCheckErrors1 _ = return ()
#endif
