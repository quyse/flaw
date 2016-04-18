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
import Data.IORef
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Audio
import Flaw.Audio.OpenAL.FFI
import Flaw.Book
import Flaw.ByteStream
import Flaw.Exception
import Flaw.Math

data AlDevice = AlDevice
	{
	-- | Operations to be executed during next tick.
	  alDeviceDeferredOperationsVar :: {-# UNPACK #-} !(TVar [IO ()])
	-- | Operations repeatedly executed during ticks.
	, alDeviceRepeatOperationsRef :: {-# UNPACK #-} !(IORef [(ALuint, IO ())])
	}

data AlSoundSource
	= AlBufferedSoundSource
		{ alSoundSourceBufferName :: {-# UNPACK #-} !ALuint
		}
	| AlStreamingSoundSource
		{ alSoundSourceCreateStream :: !(IO ((SoundFormat, ByteStream), IO ()))
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
		, alSoundPlayerPlayingRef :: {-# UNPACK #-} !(IORef Bool)
		}

	createSound device format@SoundFormat
		{ soundFormatSamplesPerSecond = samplesPerSecond
		} bytes = describeException "failed to create OpenAL sound" $ withSpecialBook $ \bk -> do
		-- allocate buffer name
		bufferName <- alloca $ \bufferNamePtr -> do
			alGenBuffers 1 bufferNamePtr
			peek bufferNamePtr
		alCheckError0 "gen buffer"
		book bk $ return ((), atomically $ alDeferredOperation device $ with bufferName $ alDeleteBuffers 1)

		-- upload data
		B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) ->
			alBufferData bufferName (alConvertFormat format) (castPtr bytesPtr) (fromIntegral bytesLen) (fromIntegral samplesPerSecond)
		alCheckError0 "buffer data"

		alCheckError1 "create sound"
		return AlSoundId
			{ alSoundDevice = device
			, alSoundSource = AlBufferedSoundSource
				{ alSoundSourceBufferName = bufferName
				}
			}

	createStreamingSound device createStream = return (AlSoundId
		{ alSoundDevice = device
		, alSoundSource = AlStreamingSoundSource
			{ alSoundSourceCreateStream = createStream
			}
		}, return ())

	createSoundPlayer AlSoundId
		{ alSoundDevice = device@AlDevice
			{ alDeviceRepeatOperationsRef = repeatOperationsRef
			}
		, alSoundSource = soundSource
		} = describeException "failed to create OpenAL sound player" $ withSpecialBook $ \bk -> do
		-- allocate source name
		sourceName <- alloca $ \sourceNamePtr -> do
			alGenSources 1 sourceNamePtr
			peek sourceNamePtr
		alCheckError0 "gen source"
		book bk $ return ((), atomically $ alDeferredOperation device $ do
			alSourceStop sourceName
			with sourceName $ alDeleteSources 1
			)

		-- flag that player is playing
		playingRef <- newIORef False

		-- setup sound source
		case soundSource of
			AlBufferedSoundSource
				{ alSoundSourceBufferName = bufferName
				} -> do
				-- for buffered sound just set buffer for the source
				alSourcei sourceName AL_BUFFER (fromIntegral bufferName)
				alCheckError0 "set source buffer"
			AlStreamingSoundSource
				{ alSoundSourceCreateStream = createStream
				} -> do
				-- create a few buffers for streaming
				buffers <- forM [1..2 :: Int] $ \_i -> do
					-- allocate buffer name
					bufferName <- alloca $ \bufferNamePtr -> do
						alGenBuffers 1 bufferNamePtr
						peek bufferNamePtr
					alCheckError0 "gen buffer"
					book bk $ return (bufferName, atomically $ alDeferredOperation device $ with bufferName $ alDeleteBuffers 1)
				-- start streaming
				(format@SoundFormat
					{ soundFormatSamplesPerSecond = samplesPerSecond
					, soundFormatSampleType = sampleType
					, soundFormatChannelsCount = channelsCount
					}, byteStream) <- book bk createStream
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
						alCheckError0 "unqueue buffers"
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
							bytesLength <- fromIntegral <$> byteStreamLength byteStream
							finished <- isByteStreamFinished byteStream
							-- allow feeding last buffer partially if stream is finished
							let buffersToFeedCount = min freeBuffersCount $ if finished then (bytesLength + bufferSize - 1) `quot` bufferSize else bytesLength `quot` bufferSize
							if buffersToFeedCount > 0 then do
								feedBytes <- pullByteStream byteStream $ fromIntegral $ buffersToFeedCount * bufferSize
								return (feedBytes, buffersToFeedCount)
							else return (BL.empty, 0)
						-- if there're some data
						when (buffersToFeedCount > 0) $ do
							-- put data into buffers
							let
								(freeBuffersToFeed, restFreeBuffers) = splitAt buffersToFeedCount freeBuffers
								feedBuffer (bufferName : restBufferNames) tailFeedBytes = do
									let (chunkBytes, restTailFeedBytes) = BL.splitAt (fromIntegral bufferSize) tailFeedBytes
									B.unsafeUseAsCStringLen (BL.toStrict chunkBytes) $ \(feedBytesPtr, feedBytesLen) ->
										alBufferData bufferName (alConvertFormat format) (castPtr feedBytesPtr) (fromIntegral feedBytesLen) (fromIntegral samplesPerSecond)
									alCheckError0 "feed buffer"
									feedBuffer restBufferNames restTailFeedBytes
								feedBuffer [] _ = return ()
							feedBuffer freeBuffersToFeed feedBytes
							-- enqueue them
							withArray freeBuffersToFeed $ alSourceQueueBuffers sourceName (fromIntegral buffersToFeedCount)
							alCheckError0 "queue buffers"
							-- change state
							writeIORef stateRef state
								{ alStreamingStateFreeBuffers = restFreeBuffers
								, alStreamingStateBusyBuffers = busyBuffers ++ freeBuffersToFeed
								}
							-- ensure it's playing if needed (it might stop playing if streaming has been interrupted)
							playing <- readIORef playingRef
							when playing $ do
								sourceState <- alloca $ \sourceStatePtr -> do
									alGetSourcei sourceName AL_SOURCE_STATE sourceStatePtr
									alCheckError0 "get source state"
									peek sourceStatePtr
								unless (sourceState == AL_PLAYING) $ do
									alSourcePlay sourceName
									alCheckError0 "restart playing"

				-- register operation
				modifyIORef' repeatOperationsRef ((sourceName, feed) :)
				book bk $ return ((), atomically $ alDeferredOperation device $ modifyIORef' repeatOperationsRef $ filter ((/= sourceName) . fst))

		alCheckError1 "create sound player"
		return AlSoundPlayerId
			{ alSoundPlayerDevice = device
			, alSoundPlayerSourceName = sourceName
			, alSoundPlayerPlayingRef = playingRef
			}

	tickAudio device@AlDevice
		{ alDeviceRepeatOperationsRef = repeatOperationsRef
		} = do
		alRunDeferredOperations device
		join $ sequence_ . map snd <$> readIORef repeatOperationsRef

	playSound AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		, alSoundPlayerPlayingRef = playingRef
		} = alDeferredOperation device $ do
		alSourcei sourceName AL_LOOPING AL_FALSE
		alSourcePlay sourceName
		alCheckError0 "play"
		writeIORef playingRef True

	playLoopSound AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		, alSoundPlayerPlayingRef = playingRef
		} = alDeferredOperation device $ do
		alSourcei sourceName AL_LOOPING AL_TRUE
		alSourcePlay sourceName
		alCheckError0 "play loop"
		writeIORef playingRef True

	pauseSound AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		, alSoundPlayerPlayingRef = playingRef
		} = alDeferredOperation device $ do
		alSourcePause sourceName
		alCheckError0 "pause source"
		writeIORef playingRef False

	stopSound AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		, alSoundPlayerPlayingRef = playingRef
		} = alDeferredOperation device $ do
		alSourceStop sourceName
		alCheckError0 "stop source"
		writeIORef playingRef False

	setSoundPosition AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		} (Vec3 x y z) = alDeferredOperation device $ do
		alSource3f sourceName AL_POSITION (realToFrac x) (realToFrac y) (realToFrac z)
		alCheckError0 "set sound position"

	setSoundDirection AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		} (Vec3 x y z) = alDeferredOperation device $ do
		alSource3f sourceName AL_DIRECTION (realToFrac x) (realToFrac y) (realToFrac z)
		alCheckError0 "set sound direction"

	setSoundVelocity AlSoundPlayerId
		{ alSoundPlayerDevice = device
		, alSoundPlayerSourceName = sourceName
		} (Vec3 x y z) = alDeferredOperation device $ do
		alSource3f sourceName AL_VELOCITY (realToFrac x) (realToFrac y) (realToFrac z)
		alCheckError0 "set sound velocity"

createAlDevice :: IO (AlDevice, IO ())
createAlDevice = describeException "failed to create OpenAL device" $ withSpecialBook $ \bk -> do

	-- create var for deferred operations
	deferredOperationsVar <- newTVarIO []
	-- create ref for repeat operations
	repeatOperationsRef <- newIORef []

	-- device struct
	let device = AlDevice
		{ alDeviceDeferredOperationsVar = deferredOperationsVar
		, alDeviceRepeatOperationsRef = repeatOperationsRef
		}

	-- open audio device
	devicePtr <- alcOpenDevice nullPtr
	when (devicePtr == nullPtr) $ throwIO $ DescribeFirstException "failed to open device"
	book bk $ return ((), void $ alcCloseDevice devicePtr)

	-- create context
	contextPtr <- alcCreateContext devicePtr nullPtr
	alcCheckError devicePtr "create context"
	when (contextPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to create context"
	book bk $ return ((), do
		alcDestroyContext contextPtr
		alcCheckError devicePtr "destroy context"
		)

	-- make context current
	void $ alcMakeContextCurrent contextPtr
	alcCheckError devicePtr "make context current"
	book bk $ return ((), do
		void $ alcMakeContextCurrent nullPtr
		alcCheckError devicePtr "reset current context"
		)

	-- freeing of some objects is implemented as deferred operation
	-- so schedule running deferred operations before destroying context
	book bk $ return ((), alRunDeferredOperations device)

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
	return $ sequence_ deferredOperations

-- | Check for OpenAL error, throw an exception if there's one.
alCheckError :: String -> IO ()
alCheckError msg = do
	err <- alGetError
	unless (err == AL_NO_ERROR) $ let
		errStr = case err of
			AL_INVALID_NAME -> "AL_INVALID_NAME"
			AL_INVALID_ENUM -> "AL_INVALID_ENUM"
			AL_INVALID_VALUE -> "AL_INVALID_VALUE"
			AL_INVALID_OPERATION -> "AL_INVALID_OPERATION"
			AL_OUT_OF_MEMORY -> "AL_OUT_OF_MEMORY"
			_ -> show err
		in throwIO $ DescribeFirstException $ "OpenAL error " ++ errStr ++ ": " ++ msg

{-# INLINE alCheckError0 #-}
alCheckError0 :: String -> IO ()
#if 0 >= AL_ERROR_LEVEL
alCheckError0 = alCheckError
#else
alCheckError0 _ = return ()
#endif

{-# INLINE alCheckError1 #-}
alCheckError1 :: String -> IO ()
#if 1 >= AL_ERROR_LEVEL
alCheckError1 = alCheckError
#else
alCheckError1 _ = return ()
#endif

-- | Check for ALC error, throw an exception if there's one.
alcCheckError :: Ptr ALCdevice -> String -> IO ()
alcCheckError devicePtr msg = do
	err <- alcGetError devicePtr
	unless (err == ALC_NO_ERROR) $ let
		errStr = case err of
			ALC_INVALID_DEVICE -> "ALC_INVALID_DEVICE"
			ALC_INVALID_CONTEXT -> "ALC_INVALID_CONTEXT"
			ALC_INVALID_ENUM -> "ALC_INVALID_ENUM"
			ALC_INVALID_VALUE -> "ALC_INVALID_VALUE"
			ALC_OUT_OF_MEMORY -> "ALC_OUT_OF_MEMORY"
			_ -> show err
		in throwIO $ DescribeFirstException $ "OpenAL ALC error " ++ errStr ++ ": " ++ msg
