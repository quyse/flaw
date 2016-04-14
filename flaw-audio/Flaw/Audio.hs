{-|
Module: Flaw.Audio
Description: Audio abstraction.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Audio
	( Device(..)
	, SoundFormat(..)
	, SoundSampleType(..)
	) where

import qualified Data.ByteString as B

import Flaw.Math

class Device d where
	-- | Sound type.
	data SoundId d :: *
	-- | Type of sound player.
	data SoundPlayerId d :: *
	-- | Create buffered sound.
	createSound :: d -> SoundFormat -> B.ByteString -> IO (SoundId d, IO ())
	-- | Create player for a sound.
	createSoundPlayer :: SoundId d -> IO (SoundPlayerId d, IO ())
	-- | Start playing.
	playSound :: SoundPlayerId d -> IO ()
	-- | Pause playing.
	pauseSound :: SoundPlayerId d -> IO ()
	-- | Stop playing.
	stopSound :: SoundPlayerId d -> IO ()
	-- | Set position of sound player.
	setSoundPosition :: SoundPlayerId d -> Float3 -> IO ()
	-- | Set direction of sound player.
	setSoundDirection :: SoundPlayerId d -> Float3 -> IO ()
	-- | Set velocity of sound player.
	setSoundVelocity :: SoundPlayerId d -> Float3 -> IO ()

-- | Sound format type.
data SoundFormat = SoundFormat
	{ soundFormatSamplesPerSecond :: {-# UNPACK #-} !Int
	, soundFormatSampleType :: !SoundSampleType
	, soundFormatChannelsCount :: {-# UNPACK #-} !Int
	} deriving Show

-- | Sound sample type.
data SoundSampleType
	= SoundSampleByte
	| SoundSampleShort
	| SoundSampleInt
	| SoundSampleFloat
	| SoundSampleDouble
	deriving Show
