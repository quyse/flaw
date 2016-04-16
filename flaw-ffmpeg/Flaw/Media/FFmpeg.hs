{-|
Module: Flaw.Media.FFmpeg
Description: FFmpeg integration.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}

module Flaw.Media.FFmpeg
	( FFmpegAVFormatContext()
	, ffmpegInit
	, ffmpegOpenInput
	, ffmpegOpenOutput
	, ffmpegRemux
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
import Foreign.Ptr

import Flaw.Audio
import Flaw.Book
import Flaw.Exception

newtype FFmpegAVFormatContext = FFmpegAVFormatContext (Ptr C_AVFormatContext)

ffmpegInit :: IO ()
ffmpegInit = flaw_ffmpeg_init

-- | Open input AV format context.
ffmpegOpenInput :: T.Text -> IO (FFmpegAVFormatContext, IO ())
ffmpegOpenInput url = describeException "failed to open input FFmpeg context" $ withSpecialBook $ \bk -> do
	formatContextPtr <- runInBoundThread $ B.useAsCString (T.encodeUtf8 url) flaw_ffmpeg_openInput
	when (formatContextPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to open input"
	book bk $ return ((), flaw_ffmpeg_closeInput formatContextPtr)

	return $ FFmpegAVFormatContext formatContextPtr

-- | Open output AV format context.
ffmpegOpenOutput :: T.Text -> IO (FFmpegAVFormatContext, IO ())
ffmpegOpenOutput url = describeException "failed to open output FFmpeg context" $ withSpecialBook $ \bk -> do
	formatContextPtr <- runInBoundThread $ B.useAsCString (T.encodeUtf8 url) flaw_ffmpeg_openOutput
	when (formatContextPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to open output"
	book bk $ return ((), flaw_ffmpeg_closeOutput formatContextPtr)

	return $ FFmpegAVFormatContext formatContextPtr

ffmpegRemux :: FFmpegAVFormatContext -> FFmpegAVFormatContext -> IO ()
ffmpegRemux (FFmpegAVFormatContext inputFormatContextPtr) (FFmpegAVFormatContext outputFormatContextPtr) = describeException "failed FFmpeg remuxinx" $ do
	checkAVError "remux" $ flaw_ffmpeg_remux inputFormatContextPtr outputFormatContextPtr

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
foreign import ccall safe flaw_ffmpeg_closeInput :: Ptr C_AVFormatContext -> IO ()
foreign import ccall safe flaw_ffmpeg_openOutput :: Ptr CChar -> IO (Ptr C_AVFormatContext)
foreign import ccall safe flaw_ffmpeg_closeOutput :: Ptr C_AVFormatContext -> IO ()
foreign import ccall safe flaw_ffmpeg_remux :: Ptr C_AVFormatContext -> Ptr C_AVFormatContext -> IO CInt
foreign import ccall safe flaw_ffmpeg_decodeSinglePackedAudioStream :: Ptr C_AVFormatContext -> FunPtr DecodeAudioCallback -> IO CInt

-- wrappers

type DecodeAudioCallback = CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt
foreign import ccall "wrapper" wrapDecodeAudioCallback :: DecodeAudioCallback -> IO (FunPtr DecodeAudioCallback)

-- ffmpeg types

data C_AVFormatContext

-- ffmpeg enums

pattern AV_SAMPLE_FMT_U8 = 0
pattern AV_SAMPLE_FMT_S16 = 1
pattern AV_SAMPLE_FMT_S32 = 2
pattern AV_SAMPLE_FMT_FLT = 3
pattern AV_SAMPLE_FMT_DBL = 4
