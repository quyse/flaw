{-|
Module: Flaw.Audio.Util
Description: Audio utilities.
License: MIT
-}

module Flaw.Audio.Util
  ( convertAudioIntToShort
  , convertAudioFloatToShort
  , convertAudioDoubleToShort
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Int
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Exception

-- | Convert audio samples from S32 to S16 format.
convertAudioIntToShort :: B.ByteString -> IO B.ByteString
convertAudioIntToShort = convertSamples (fromIntegral . (`quot` 0x10000) :: Int32 -> Int16)

-- | Convert audio samples from F32 to S16 format.
convertAudioFloatToShort :: B.ByteString -> IO B.ByteString
convertAudioFloatToShort = convertSamples (floor . (* 0x7fff) . clampFloating :: Float -> Int16)

-- | Convert audio samples from F64 to S16 format.
convertAudioDoubleToShort :: B.ByteString -> IO B.ByteString
convertAudioDoubleToShort = convertSamples (floor . (* 0x7fff) . clampFloating :: Double -> Int16)

-- | Internal conversion function.
convertSamples :: (Storable a, Storable b) => (a -> b) -> B.ByteString -> IO B.ByteString
convertSamples convert bytes = B.unsafeUseAsCStringLen bytes $ \(srcPtr, srcLen) -> do
  let
    -- handy type hack
    undefinedParam :: (a -> b) -> a
    undefinedParam _ = undefined

    -- get samples cound
    (samplesCount, r) = srcLen `quotRem` sizeOf (undefinedParam convert)

  unless (r == 0) $ throwIO $ DescribeFirstException "size of audio samples sequence is not divisible by sample size"

  -- allocate memory
  destPtr <- mallocArray samplesCount
  -- perform conversion
  let
    loop i = when (i < samplesCount) $ do
      pokeElemOff destPtr i =<< convert <$> peekElemOff (castPtr srcPtr) i
      loop $ i + 1
    in loop 0
  -- make a bytestring
  B.unsafePackMallocCStringLen (castPtr destPtr, samplesCount * sizeOf (convert undefined))

clampFloating :: (Floating a, Ord a) => a -> a
clampFloating a
  | a > 1 = 1
  | a < (-1) = -1
  | otherwise = a
