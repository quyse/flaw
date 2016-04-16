{-|
Module: Flaw.Media.FFmpeg
Description: FFmpeg integration.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}

module Flaw.Media.FFmpeg
	( FFmpegAVFormatContext()
	, ffmpegInit
	, ffmpegOpenAVFormatContext
	, ffmpegDecodeSingleAudioStream
	) where

import Control.Exception
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr

import Flaw.Audio
import Flaw.Book
import Flaw.Exception

newtype FFmpegAVFormatContext = FFmpegAVFormatContext (Ptr C_AVFormatContext)

ffmpegInit :: IO ()
ffmpegInit = flaw_ffmpeg_init

-- | Open AV format context by media URL.
ffmpegOpenAVFormatContext :: T.Text -> IO (FFmpegAVFormatContext, IO ())
ffmpegOpenAVFormatContext url = describeException "failed to open FFmpeg AV format context" $ withSpecialBook $ \bk -> do

	-- open AV format context
	formatContextPtr <- runInBoundThread $ B.useAsCString (T.encodeUtf8 url) flaw_ffmpeg_openInput
	when (formatContextPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to open input"
	book bk $ return ((), with formatContextPtr avformat_close_input)

	-- find stream info
	checkAVError "avformat_find_stream_info" $ avformat_find_stream_info formatContextPtr nullPtr

	return $ FFmpegAVFormatContext formatContextPtr

ffmpegDecodeSingleAudioStream :: FFmpegAVFormatContext -> (SoundFormat -> Ptr () -> Int -> IO ()) -> IO ()
ffmpegDecodeSingleAudioStream (FFmpegAVFormatContext formatContextPtr) callback = describeException "failed to decode ffmpeg single audio stream" $ withBook $ \bk -> do

	wrappedCallback <- wrapDecodeAudioCallback $ \samplesPerSecond sampleFormat channelsCount dataPtr size -> do
		callback SoundFormat
			{ soundFormatSamplesPerSecond = fromIntegral samplesPerSecond
			, soundFormatSampleType = convertSampleFormat sampleFormat
			, soundFormatChannelsCount = fromIntegral channelsCount
			} dataPtr (fromIntegral size)
		return 0

	book bk $ return ((), freeHaskellFunPtr wrappedCallback)

	checkAVError "decode audio" $ flaw_ffmpeg_decodeSinglePackedAudioStream formatContextPtr wrappedCallback

checkAVError :: String -> IO CInt -> IO ()
checkAVError message action = do
	r <- action
	unless (r == 0) $ throwIO $ DescribeFirstException $ "AV error " <> show r <> ": " <> message

convertSampleFormat :: CInt -> SoundSampleType
convertSampleFormat sampleFormat = case sampleFormat of
	AV_SAMPLE_FMT_U8 -> SoundSampleByte
	AV_SAMPLE_FMT_S16 -> SoundSampleShort
	AV_SAMPLE_FMT_S32 -> SoundSampleInt
	AV_SAMPLE_FMT_FLT -> SoundSampleFloat
	AV_SAMPLE_FMT_DBL -> SoundSampleDouble
	_ -> error "unknown AV sample format"

-- flaw-ffmpeg helper routines

foreign import ccall safe flaw_ffmpeg_init :: IO ()
foreign import ccall safe flaw_ffmpeg_openInput :: Ptr CChar -> IO (Ptr C_AVFormatContext)
foreign import ccall safe flaw_ffmpeg_decodeSinglePackedAudioStream :: Ptr C_AVFormatContext -> FunPtr DecodeAudioCallback -> IO CInt

-- wrappers

type DecodeAudioCallback = CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt
foreign import ccall "wrapper" wrapDecodeAudioCallback :: DecodeAudioCallback -> IO (FunPtr DecodeAudioCallback)

-- ffmpeg types

data C_AVFormatContext
data C_AVDictionary

-- ffmpeg routines

foreign import ccall safe avformat_close_input
	:: Ptr (Ptr C_AVFormatContext) -> IO ()
foreign import ccall safe avformat_find_stream_info
	:: Ptr C_AVFormatContext -> Ptr (Ptr C_AVDictionary) -> IO CInt

-- ffmpeg enums

pattern AV_SAMPLE_FMT_U8 = 0
pattern AV_SAMPLE_FMT_S16 = 1
pattern AV_SAMPLE_FMT_S32 = 2
pattern AV_SAMPLE_FMT_FLT = 3
pattern AV_SAMPLE_FMT_DBL = 4
