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
	, ffmpegSetContextOption
	, ffmpegDemux
	, ffmpegGetPacketStreamIndex
	, ffmpegDecode
	, ffmpegOpenCodec
	, ffmpegPrepareRemux
	, ffmpegRemuxPacket
	, ffmpegFinalizeOutputContext
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
import Foreign.ForeignPtr
import Foreign.Ptr

import Flaw.Audio
import Flaw.Book
import Flaw.Exception

newtype FFmpegAVFormatContext = FFmpegAVFormatContext (Ptr C_AVFormatContext)
newtype FFmpegAVPacket = FFmpegAVPacket (ForeignPtr C_AVPacket)
newtype FFmpegAVFrame = FFmpegAVFrame (ForeignPtr C_AVFrame)

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
ffmpegOpenOutput :: T.Text -> T.Text -> IO (FFmpegAVFormatContext, IO ())
ffmpegOpenOutput url format = describeException "failed to open output FFmpeg context" $ withSpecialBook $ \bk -> do
	formatContextPtr <- runInBoundThread $
		B.useAsCString (T.encodeUtf8 url) $ \urlPtr ->
		B.useAsCString (T.encodeUtf8 format) $ \formatPtr ->
		flaw_ffmpeg_openOutput urlPtr formatPtr
	when (formatContextPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to open output"
	book bk $ return ((), flaw_ffmpeg_closeOutput formatContextPtr)

	return $ FFmpegAVFormatContext formatContextPtr

-- | Set AV format context option.
ffmpegSetContextOption :: FFmpegAVFormatContext -> T.Text -> T.Text -> IO ()
ffmpegSetContextOption (FFmpegAVFormatContext formatContextPtr) option value = do
	r <- B.useAsCString (T.encodeUtf8 option) $ \optionPtr ->
		B.useAsCString (T.encodeUtf8 value) $ \valuePtr ->
		av_opt_set (castPtr formatContextPtr) optionPtr valuePtr AV_OPT_SEARCH_CHILDREN
	unless (r == 0) $ let
		desc = case r of
			AVERROR_OPTION_NOT_FOUND -> "option not found"
			AVERROR_EINVAL -> "invalid value"
			_ -> "unknown error"
		in throwIO $ DescribeFirstException $ "failed to set FFmpeg option " <> T.unpack option <> ": " <> desc

-- | Demux streams from input context.
{-# INLINABLE ffmpegDemux #-}
ffmpegDemux :: FFmpegAVFormatContext -> (FFmpegAVPacket -> IO ()) -> IO ()
ffmpegDemux (FFmpegAVFormatContext formatContextPtr) callback = withBook $ \bk -> do
	wrappedCallback <- wrapDemuxCallback $ \packetPtr -> handle (\SomeException {} -> return 1) $ do
		callback =<< FFmpegAVPacket <$> (newForeignPtr flaw_ffmpeg_freePacket =<< flaw_ffmpeg_refPacket packetPtr)
		return 0
	book bk $ return ((), freeHaskellFunPtr wrappedCallback)
	checkAVError "demux" $ flaw_ffmpeg_demux formatContextPtr wrappedCallback nullPtr

-- | Get stream index of a packet.
{-# INLINABLE ffmpegGetPacketStreamIndex #-}
ffmpegGetPacketStreamIndex :: FFmpegAVPacket -> IO Int
ffmpegGetPacketStreamIndex (FFmpegAVPacket packetFPtr) = withForeignPtr packetFPtr $ \packetPtr ->
	fromIntegral <$> flaw_ffmpeg_getPacketStreamIndex packetPtr

{-# INLINABLE ffmpegDecode #-}
ffmpegDecode :: FFmpegAVFormatContext -> FFmpegAVPacket -> (FFmpegAVFrame -> IO ()) -> IO ()
ffmpegDecode (FFmpegAVFormatContext formatContextPtr) (FFmpegAVPacket packetFPtr) callback = withBook $ \bk -> do
	wrappedCallback <- wrapDecodeCallback $ \framePtr -> handle (\SomeException {} -> return 1) $ do
		callback =<< FFmpegAVFrame <$> (newForeignPtr flaw_ffmpeg_freeFrame =<< flaw_ffmpeg_refFrame framePtr)
		return 0
	book bk $ return ((), freeHaskellFunPtr wrappedCallback)
	checkAVError "decode" $ withForeignPtr packetFPtr $ \packetPtr ->
		flaw_ffmpeg_decode formatContextPtr packetPtr wrappedCallback

ffmpegOpenCodec :: FFmpegAVFormatContext -> Int -> IO ((), IO ())
ffmpegOpenCodec (FFmpegAVFormatContext formatContextPtr) streamIndex = do
	checkAVError "open codec" $ flaw_ffmpeg_openCodec formatContextPtr (fromIntegral streamIndex)
	return ((), flaw_ffmpeg_closeCodec formatContextPtr (fromIntegral streamIndex))

ffmpegPrepareRemux :: FFmpegAVFormatContext -> FFmpegAVFormatContext -> IO ()
ffmpegPrepareRemux (FFmpegAVFormatContext inputFormatContextPtr) (FFmpegAVFormatContext outputFormatContextPtr) = do
	checkAVError "prepare remux" $ flaw_ffmpeg_prepareRemux inputFormatContextPtr outputFormatContextPtr

{-# INLINABLE ffmpegRemuxPacket #-}
ffmpegRemuxPacket :: FFmpegAVFormatContext -> FFmpegAVFormatContext -> FFmpegAVPacket -> IO ()
ffmpegRemuxPacket (FFmpegAVFormatContext inputFormatContextPtr) (FFmpegAVFormatContext outputFormatContextPtr) (FFmpegAVPacket packetFPtr) = do
	checkAVError "remux packet" $ withForeignPtr packetFPtr $ \packetPtr -> flaw_ffmpeg_remuxPacket inputFormatContextPtr outputFormatContextPtr packetPtr

ffmpegFinalizeOutputContext :: FFmpegAVFormatContext -> IO ()
ffmpegFinalizeOutputContext (FFmpegAVFormatContext formatContextPtr) = checkAVError "finalize output context" $ flaw_ffmpeg_finalizeOutputContext formatContextPtr

ffmpegDecodeSingleAudioStream :: FFmpegAVFormatContext -> (SoundFormat -> Ptr () -> Int -> IO ()) -> IO ()
ffmpegDecodeSingleAudioStream formatContext@(FFmpegAVFormatContext formatContextPtr) callback = describeException "failed to decode ffmpeg single audio stream" $ withBook $ \bk -> do

	-- find single audio stream
	streamIndex <- fromIntegral <$> flaw_ffmpeg_getSingleAudioStream formatContextPtr
	when (streamIndex < 0) $ throwIO $ DescribeFirstException "no single audio stream"

	-- open codec
	book bk $ ffmpegOpenCodec formatContext streamIndex

	-- wrap callback
	wrappedCallback <- wrapDecodeAudioCallback $
		\samplesPerSecond sampleFormat channelsCount dataPtr size -> handle (\SomeException {} -> return 1) $ do
		callback SoundFormat
			{ soundFormatSamplesPerSecond = fromIntegral samplesPerSecond
			, soundFormatSampleType = convertSampleFormat sampleFormat
			, soundFormatChannelsCount = fromIntegral channelsCount
			} dataPtr (fromIntegral size)
		return 0
	book bk $ return ((), freeHaskellFunPtr wrappedCallback)

	-- demux and decode
	ffmpegDemux formatContext $ \packet -> do
		packetStreamIndex <- ffmpegGetPacketStreamIndex packet
		when (streamIndex == packetStreamIndex) $
			ffmpegDecode formatContext packet $ \(FFmpegAVFrame frameFPtr) -> withForeignPtr frameFPtr $ \framePtr ->
				checkAVError "pack audio" $ flaw_ffmpeg_packAudioFrame framePtr wrappedCallback

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
foreign import ccall safe flaw_ffmpeg_openOutput :: Ptr CChar -> Ptr CChar -> IO (Ptr C_AVFormatContext)
foreign import ccall safe flaw_ffmpeg_closeOutput :: Ptr C_AVFormatContext -> IO ()

foreign import ccall safe flaw_ffmpeg_refPacket :: Ptr C_AVPacket -> IO (Ptr C_AVPacket)
foreign import ccall safe "&flaw_ffmpeg_freePacket" flaw_ffmpeg_freePacket :: FunPtr (Ptr C_AVPacket -> IO ())
foreign import ccall unsafe flaw_ffmpeg_getPacketStreamIndex :: Ptr C_AVPacket -> IO CInt
foreign import ccall safe flaw_ffmpeg_demux :: Ptr C_AVFormatContext -> FunPtr DemuxCallback -> Ptr () -> IO CInt

foreign import ccall safe flaw_ffmpeg_refFrame :: Ptr C_AVFrame -> IO (Ptr C_AVFrame)
foreign import ccall safe "&flaw_ffmpeg_freeFrame" flaw_ffmpeg_freeFrame :: FunPtr (Ptr C_AVFrame -> IO ())
foreign import ccall safe flaw_ffmpeg_decode :: Ptr C_AVFormatContext -> Ptr C_AVPacket -> FunPtr DecodeCallback -> IO CInt

foreign import ccall safe flaw_ffmpeg_openCodec :: Ptr C_AVFormatContext -> CInt -> IO CInt
foreign import ccall safe flaw_ffmpeg_closeCodec :: Ptr C_AVFormatContext -> CInt -> IO ()

foreign import ccall safe flaw_ffmpeg_prepareRemux :: Ptr C_AVFormatContext -> Ptr C_AVFormatContext -> IO CInt
foreign import ccall safe flaw_ffmpeg_remuxPacket :: Ptr C_AVFormatContext -> Ptr C_AVFormatContext -> Ptr C_AVPacket -> IO CInt
foreign import ccall safe flaw_ffmpeg_finalizeOutputContext :: Ptr C_AVFormatContext -> IO CInt

foreign import ccall unsafe flaw_ffmpeg_getSingleAudioStream :: Ptr C_AVFormatContext -> IO CInt
foreign import ccall safe flaw_ffmpeg_packAudioFrame :: Ptr C_AVFrame -> FunPtr DecodeAudioCallback -> IO CInt

-- wrappers

type DemuxCallback = Ptr C_AVPacket -> IO CInt
foreign import ccall "wrapper" wrapDemuxCallback :: DemuxCallback -> IO (FunPtr DemuxCallback)

type DecodeCallback = Ptr C_AVFrame -> IO CInt
foreign import ccall "wrapper" wrapDecodeCallback :: DecodeCallback -> IO (FunPtr DecodeCallback)

type DecodeAudioCallback = CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt
foreign import ccall "wrapper" wrapDecodeAudioCallback :: DecodeAudioCallback -> IO (FunPtr DecodeAudioCallback)

-- ffmpeg functions

foreign import ccall unsafe av_opt_set :: Ptr () -> Ptr CChar -> Ptr CChar -> CInt -> IO CInt

-- ffmpeg types

data C_AVFormatContext
data C_AVPacket
data C_AVFrame

-- ffmpeg enums

pattern AV_SAMPLE_FMT_U8 = 0
pattern AV_SAMPLE_FMT_S16 = 1
pattern AV_SAMPLE_FMT_S32 = 2
pattern AV_SAMPLE_FMT_FLT = 3
pattern AV_SAMPLE_FMT_DBL = 4

pattern AV_OPT_SEARCH_CHILDREN = 1
pattern AVERROR_OPTION_NOT_FOUND = (-0x54504ff8)
pattern AVERROR_EINVAL = (-22)
