{-|
Module: Flaw.Audio.OpenAL.FFI
Description: FFI definitions for OpenAL.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}

module Flaw.Audio.OpenAL.FFI
	(
	-- * Basic types
	  ALboolean
	, ALchar
	, ALbyte
	, ALubyte
	, ALshort
	, ALushort
	, ALint
	, ALuint
	, ALsizei
	, ALenum
	, ALfloat
	, ALdouble
	-- * ALC types
	, ALCdevice
	, ALCcontext
	-- * ALC functions
	, alcOpenDevice
	, alcCloseDevice
	, alcCreateContext
	, alcDestroyContext
	, alcMakeContextCurrent
	-- * AL functions
	, alGetError
	, alGenBuffers
	, alDeleteBuffers
	, alBufferData
	, alGenSources
	, alDeleteSources
	, alSourcei
	, alSource3f
	, alGetSourcei
	, alSourcePlay
	, alSourcePause
	, alSourceStop
	, alSourceQueueBuffers
	, alSourceUnqueueBuffers
	-- * AL enums
	, pattern AL_NO_ERROR
	, pattern AL_FORMAT_MONO8
	, pattern AL_FORMAT_MONO16
	, pattern AL_FORMAT_STEREO8
	, pattern AL_FORMAT_STEREO16
	, pattern AL_FALSE
	, pattern AL_TRUE
	, pattern AL_POSITION
	, pattern AL_DIRECTION
	, pattern AL_VELOCITY
	, pattern AL_LOOPING
	, pattern AL_BUFFER
	, pattern AL_SOURCE_STATE
	, pattern AL_INITIAL
	, pattern AL_PLAYING
	, pattern AL_PAUSED
	, pattern AL_STOPPED
	, pattern AL_BUFFERS_PROCESSED
	) where

import Foreign.C.Types
import Foreign.Ptr

type ALboolean = CChar
type ALchar = CChar
type ALbyte = CSChar
type ALubyte = CUChar
type ALshort = CShort
type ALushort = CUShort
type ALint = CInt
type ALuint = CUInt
type ALsizei = CInt
type ALenum = CInt
type ALfloat = CFloat
type ALdouble = CDouble

data ALCdevice
data ALCcontext

foreign import ccall safe alcOpenDevice :: Ptr ALchar -> IO (Ptr ALCdevice)
foreign import ccall safe alcCloseDevice :: Ptr ALCdevice -> IO ALboolean

foreign import ccall safe alcCreateContext :: Ptr ALCdevice -> Ptr ALint -> IO (Ptr ALCcontext)
foreign import ccall safe alcDestroyContext :: Ptr ALCcontext -> IO ()
foreign import ccall safe alcMakeContextCurrent :: Ptr ALCcontext -> IO ALboolean

foreign import ccall unsafe alGetError :: IO ALenum

foreign import ccall unsafe alGenBuffers :: ALsizei -> Ptr ALuint -> IO ()
foreign import ccall unsafe alDeleteBuffers :: ALsizei -> Ptr ALuint -> IO ()
foreign import ccall unsafe alBufferData :: ALuint -> ALenum -> Ptr () -> ALsizei -> ALsizei -> IO ()

foreign import ccall unsafe alGenSources :: ALsizei -> Ptr ALuint -> IO ()
foreign import ccall unsafe alDeleteSources :: ALsizei -> Ptr ALuint -> IO ()
foreign import ccall unsafe alSourcei :: ALuint -> ALenum -> ALint -> IO ()
foreign import ccall unsafe alSource3f :: ALuint -> ALenum -> ALfloat -> ALfloat -> ALfloat -> IO ()
foreign import ccall unsafe alGetSourcei :: ALuint -> ALenum -> Ptr ALint -> IO ()
foreign import ccall unsafe alSourcePlay :: ALuint -> IO ()
foreign import ccall unsafe alSourcePause :: ALuint -> IO ()
foreign import ccall unsafe alSourceStop :: ALuint -> IO ()
foreign import ccall unsafe alSourceQueueBuffers :: ALuint -> ALsizei -> Ptr ALuint -> IO ()
foreign import ccall unsafe alSourceUnqueueBuffers :: ALuint -> ALsizei -> Ptr ALuint -> IO ()

pattern AL_NO_ERROR = 0

pattern AL_FORMAT_MONO8 = 0x1100
pattern AL_FORMAT_MONO16 = 0x1101
pattern AL_FORMAT_STEREO8 = 0x1102
pattern AL_FORMAT_STEREO16 = 0x1103

pattern AL_FALSE = 0
pattern AL_TRUE = 1

pattern AL_POSITION = 0x1004
pattern AL_DIRECTION = 0x1005
pattern AL_VELOCITY = 0x1006
pattern AL_LOOPING = 0x1007
pattern AL_BUFFER = 0x1009

pattern AL_SOURCE_STATE = 0x1010

pattern AL_INITIAL = 0x1011
pattern AL_PLAYING = 0x1012
pattern AL_PAUSED = 0x1013
pattern AL_STOPPED = 0x1014

pattern AL_BUFFERS_PROCESSED = 0x1016
