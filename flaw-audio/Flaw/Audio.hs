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

import Control.Concurrent.STM
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
	-- | Apply deferred updates to all sound objects simultaneously.
	tickAudio :: d -> IO ()
	-- | Start playing.
	playSound :: SoundPlayerId d -> STM ()
	-- | Pause playing.
	pauseSound :: SoundPlayerId d -> STM ()
	-- | Stop playing.
	stopSound :: SoundPlayerId d -> STM ()
	-- | Set position of sound player.
	setSoundPosition :: SoundPlayerId d -> Float3 -> STM ()
	-- | Set direction of sound player.
	setSoundDirection :: SoundPlayerId d -> Float3 -> STM ()
	-- | Set velocity of sound player.
	setSoundVelocity :: SoundPlayerId d -> Float3 -> STM ()

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
