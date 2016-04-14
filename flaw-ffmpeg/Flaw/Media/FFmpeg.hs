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
	, ffmpegDecodeAudio
	) where

import Control.Exception
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

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

ffmpegDecodeAudio :: FFmpegAVFormatContext -> IO (SoundFormat, BL.ByteString)
ffmpegDecodeAudio (FFmpegAVFormatContext formatContextPtr) = describeException "failed to decode ffmpeg audio" $ do
	-- get audio stream
	streamPtr <- flaw_ffmpeg_getSingleAudioStream formatContextPtr
	when (streamPtr == nullPtr) $ throwIO $ DescribeFirstException "no single audio stream"
	-- prepare to decode
	checkAVError "prepare decoding" $ flaw_ffmpeg_prepareStreamDecoding streamPtr
	-- get format
	format <- alloca $ \samplesPerSecondPtr -> alloca $ \sampleFormatPtr -> alloca $ \channelsCountPtr -> do
		flaw_ffmpeg_getAudioStreamFormat streamPtr samplesPerSecondPtr sampleFormatPtr channelsCountPtr
		SoundFormat
			<$> (fromIntegral <$> peek samplesPerSecondPtr)
			<*> (convertSampleFormat <$> peek sampleFormatPtr)
			<*> (fromIntegral <$> peek channelsCountPtr)
	-- decode
	chunksRef <- newIORef []
	outputCallback <- wrapOutputCallback $ \chunkPtr chunkLen ->
		modifyIORef' chunksRef =<< (:) <$> B.packCStringLen (chunkPtr, fromIntegral chunkLen)
	checkAVError "decode audio" $ flaw_ffmpeg_decodeAudio formatContextPtr streamPtr outputCallback
	-- get samples data
	bytes <- BL.fromChunks . reverse <$> readIORef chunksRef
	return (format, bytes)

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
foreign import ccall safe flaw_ffmpeg_getSingleVideoStream :: Ptr C_AVFormatContext -> IO (Ptr C_AVStream)
foreign import ccall safe flaw_ffmpeg_getSingleAudioStream :: Ptr C_AVFormatContext -> IO (Ptr C_AVStream)
foreign import ccall safe flaw_ffmpeg_prepareStreamDecoding :: Ptr C_AVStream -> IO CInt
foreign import ccall unsafe flaw_ffmpeg_getAudioStreamFormat :: Ptr C_AVStream -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall safe flaw_ffmpeg_decodeAudio
	:: Ptr C_AVFormatContext -> Ptr C_AVStream -> FunPtr OutputCallback -> IO CInt

-- wrappers

type OutputCallback = Ptr CChar -> CInt -> IO ()
foreign import ccall "wrapper" wrapOutputCallback :: OutputCallback -> IO (FunPtr OutputCallback)

-- ffmpeg types

data C_AVFormatContext
data C_AVStream
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
