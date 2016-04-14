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
import qualified Data.ByteString.Unsafe as B
import Data.Foldable
import Foreign.Marshal.Alloc
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
	, alDeviceDeferredOperationsVar :: {-# UNPACK #-} !(TVar [IO ()])
	}

instance Device AlDevice where
	data SoundId AlDevice = AlSoundId
		{ alSoundDevice :: !AlDevice
		, alSoundBufferName :: {-# UNPACK #-} !ALuint
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
			, alSoundBufferName = bufferName
			}

	createSoundPlayer AlSoundId
		{ alSoundDevice = device@AlDevice
			{ alDeviceFlow = flow
			}
		, alSoundBufferName = bufferName
		} = describeException "failed to create OpenAL sound player" $ runInFlow flow $ withSpecialBook $ \bk -> do
		-- allocate source name
		sourceName <- alloca $ \sourceNamePtr -> do
			alGenSources 1 sourceNamePtr
			peek sourceNamePtr
		alCheckErrors0 "gen source"
		book bk $ return ((), atomically $ alDeferredOperation device $ with sourceName $ alDeleteSources 1)

		-- set source
		alSourcei sourceName AL_BUFFER (fromIntegral bufferName)
		alCheckErrors0 "set source buffer"

		alCheckErrors1 "create sound player"
		return AlSoundPlayerId
			{ alSoundPlayerDevice = device
			, alSoundPlayerSourceName = sourceName
			}

	tickAudio device@AlDevice
		{ alDeviceFlow = flow
		} = runInFlow flow $ alRunDeferredOperations device

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

	-- device struct
	let device = AlDevice
		{ alDeviceFlow = flow
		, alDeviceDeferredOperationsVar = deferredOperationsVar
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
	return $ foldrM const () deferredOperations

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
