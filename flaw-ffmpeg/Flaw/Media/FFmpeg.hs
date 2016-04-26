{-|
Module: Flaw.Media.FFmpeg
Description: FFmpeg integration.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}

module Flaw.Media.FFmpeg
	( FFmpegAVFormatContext()
	, FFmpegAVStream()
	, FFmpegAVPacket()
	, FFmpegAVFrame()
	, ffmpegInit
	, ffmpegOpenInput
	, ffmpegOpenOutput
	, ffmpegGetStreamsCount
	, ffmpegGetStream
	, ffmpegGetStreamIndex
	, ffmpegGetStreamType
	, ffmpegGetSingleStreamOfType
	, ffmpegSetContextOption
	, ffmpegSetStreamOption
	, ffmpegSetVideoStreamOptions
	, ffmpegNewPacket
	, ffmpegRefPacket
	, ffmpegGetPacketStreamIndex
	, ffmpegSetPacketStreamIndex
	, ffmpegDemux
	, ffmpegMux
	, ffmpegRescalePacketTime
	, ffmpegRefFrame
	, ffmpegDecode
	, ffmpegEncode
	, ffmpegOpenDecoder
	, ffmpegAddOutputStream
	, ffmpegCopyOutputStream
	, ffmpegNewScaler
	, ffmpegDecodeSingleAudioStream
	, FFmpegStreamType(..)
	, FFmpegPixFmt(..)
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
import System.IO.Unsafe

import Flaw.Audio
import Flaw.Book
import Flaw.Exception

newtype FFmpegAVFormatContext = FFmpegAVFormatContext (Ptr C_AVFormatContext)
newtype FFmpegAVStream = FFmpegAVStream (Ptr C_AVStream)
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

ffmpegGetStreamsCount :: FFmpegAVFormatContext -> IO Int
ffmpegGetStreamsCount (FFmpegAVFormatContext formatContextPtr) =
	fromIntegral <$> flaw_ffmpeg_getStreamsCount formatContextPtr

ffmpegGetStream :: FFmpegAVFormatContext -> Int -> IO FFmpegAVStream
ffmpegGetStream (FFmpegAVFormatContext formatContextPtr) streamIndex =
	FFmpegAVStream <$> flaw_ffmpeg_getStream formatContextPtr (fromIntegral streamIndex)

ffmpegGetStreamIndex :: FFmpegAVStream -> IO Int
ffmpegGetStreamIndex (FFmpegAVStream streamPtr) = fromIntegral <$> flaw_ffmpeg_getStreamIndex streamPtr

ffmpegGetStreamType :: FFmpegAVStream -> IO FFmpegStreamType
ffmpegGetStreamType (FFmpegAVStream streamPtr) =
	toEnum . fromIntegral <$> flaw_ffmpeg_getStreamType streamPtr

ffmpegGetSingleStreamOfType :: FFmpegAVFormatContext -> FFmpegStreamType -> IO FFmpegAVStream
ffmpegGetSingleStreamOfType (FFmpegAVFormatContext formatContextPtr) streamType = do
	streamPtr <- flaw_ffmpeg_getSingleStreamOfType formatContextPtr (fromIntegral $ fromEnum streamType)
	when (streamPtr == nullPtr) $ throwIO $ DescribeFirstException "no single stream of specified type"
	return $ FFmpegAVStream streamPtr

-- | Set AV format context option.
ffmpegSetContextOption :: FFmpegAVFormatContext -> T.Text -> T.Text -> IO ()
ffmpegSetContextOption (FFmpegAVFormatContext formatContextPtr) = ffmpegSetOption formatContextPtr

-- | Set AV stream option.
ffmpegSetStreamOption :: FFmpegAVStream -> T.Text -> T.Text -> IO ()
ffmpegSetStreamOption (FFmpegAVStream streamPtr) = ffmpegSetOption streamPtr

ffmpegSetOption :: Ptr a -> T.Text -> T.Text -> IO ()
ffmpegSetOption ptr option value = do
	r <- B.useAsCString (T.encodeUtf8 option) $ \optionPtr ->
		B.useAsCString (T.encodeUtf8 value) $ \valuePtr ->
		av_opt_set (castPtr ptr) optionPtr valuePtr AV_OPT_SEARCH_CHILDREN
	unless (r == 0) $ let
		desc = case r of
			AVERROR_OPTION_NOT_FOUND -> "option not found"
			AVERROR_EINVAL -> "invalid value"
			_ -> "unknown error"
		in throwIO $ DescribeFirstException $ "failed to set FFmpeg option " <> T.unpack option <> ": " <> desc

-- | Set AV stream video codec options.
ffmpegSetVideoStreamOptions :: FFmpegAVStream -> FFmpegPixFmt -> Int -> Int -> IO ()
ffmpegSetVideoStreamOptions (FFmpegAVStream streamPtr) format width height =
	flaw_ffmpeg_setVideoStreamOptions streamPtr (fromIntegral $ fromEnum format) (fromIntegral width) (fromIntegral height)

-- | Create new empty packet.
ffmpegNewPacket :: IO FFmpegAVPacket
ffmpegNewPacket = FFmpegAVPacket <$> (newForeignPtr flaw_ffmpeg_freePacket =<< flaw_ffmpeg_newPacket)

-- | Copy packet (but not data in it).
ffmpegRefPacket :: FFmpegAVPacket -> IO FFmpegAVPacket
ffmpegRefPacket (FFmpegAVPacket packetFPtr) = FFmpegAVPacket <$>
	(newForeignPtr flaw_ffmpeg_freePacket =<< withForeignPtr packetFPtr flaw_ffmpeg_refPacket)

-- | Get stream index of a packet.
ffmpegGetPacketStreamIndex :: FFmpegAVPacket -> IO Int
ffmpegGetPacketStreamIndex (FFmpegAVPacket packetFPtr) = withForeignPtr packetFPtr $ \packetPtr ->
	fromIntegral <$> flaw_ffmpeg_getPacketStreamIndex packetPtr

-- | Set stream index of a packet.
ffmpegSetPacketStreamIndex :: FFmpegAVPacket -> Int -> IO ()
ffmpegSetPacketStreamIndex (FFmpegAVPacket packetFPtr) streamIndex = withForeignPtr packetFPtr $ \packetPtr ->
	flaw_ffmpeg_setPacketStreamIndex packetPtr (fromIntegral streamIndex)

-- | Demux packets lazily from input context.
ffmpegDemux :: FFmpegAVFormatContext -> IO [FFmpegAVPacket]
ffmpegDemux (FFmpegAVFormatContext formatContextPtr) = step where
	step = unsafeInterleaveIO $ do
		FFmpegAVPacket packetFPtr <- ffmpegNewPacket
		r <- withForeignPtr packetFPtr $ flaw_ffmpeg_demux formatContextPtr
		-- return packet in any case, in order to allow single null packet in the end of stream
		(FFmpegAVPacket packetFPtr :) <$> if r == 0 then step else return []

-- | Mux packets into output context.
ffmpegMux :: FFmpegAVFormatContext -> [FFmpegAVPacket] -> IO ()
ffmpegMux (FFmpegAVFormatContext formatContextPtr) packets = do
	checkAVError "initialize output context" $ flaw_ffmpeg_initializeOutputContext formatContextPtr
	forM_ packets $ \(FFmpegAVPacket packetFPtr) ->
		checkAVError "mux packet" $ withForeignPtr packetFPtr $ flaw_ffmpeg_mux formatContextPtr
	checkAVError "finalize output context" $ flaw_ffmpeg_finalizeOutputContext formatContextPtr

-- | Rescale timestamps of packet coming from one stream, to mux into another.
ffmpegRescalePacketTime :: FFmpegAVStream -> FFmpegAVStream -> FFmpegAVPacket -> IO ()
ffmpegRescalePacketTime (FFmpegAVStream fromStreamPtr) (FFmpegAVStream toStreamPtr) (FFmpegAVPacket packetFPtr) =
	withForeignPtr packetFPtr $ flaw_ffmpeg_rescalePacketTime fromStreamPtr toStreamPtr

-- | Copy frame (but not data in it).
ffmpegRefFrame :: FFmpegAVFrame -> IO FFmpegAVFrame
ffmpegRefFrame (FFmpegAVFrame frameFPtr) = FFmpegAVFrame <$>
	(newForeignPtr flaw_ffmpeg_freeFrame =<< withForeignPtr frameFPtr flaw_ffmpeg_refFrame)

-- | Decode packet into frames lazily.
ffmpegDecode :: FFmpegAVFormatContext -> FFmpegAVPacket -> IO [FFmpegAVFrame]
ffmpegDecode (FFmpegAVFormatContext formatContextPtr) (FFmpegAVPacket packetFPtr) = step where
	step = unsafeInterleaveIO $ do
		frameFPtr <- newForeignPtr flaw_ffmpeg_freeFrame =<< flaw_ffmpeg_newFrame
		r <- withForeignPtr packetFPtr $ \packetPtr -> withForeignPtr frameFPtr $ flaw_ffmpeg_decode formatContextPtr packetPtr
		if r == 0 then (FFmpegAVFrame frameFPtr :) <$> step else return []

-- | Encode frames lazily into packets.
ffmpegEncode :: FFmpegAVFormatContext -> FFmpegAVStream -> [FFmpegAVFrame] -> IO [FFmpegAVPacket]
ffmpegEncode (FFmpegAVFormatContext formatContextPtr) stream fs = do
	streamIndex <- ffmpegGetStreamIndex stream
	let step frames = unsafeInterleaveIO $ do
		let (withFramePtr, restFrames, noPacketAction) = case frames of
			FFmpegAVFrame frameFPtr : rest -> (withForeignPtr frameFPtr, rest, step rest)
			[] -> (($ nullPtr), [], return [])
		packet@(FFmpegAVPacket packetFPtr) <- ffmpegNewPacket
		ffmpegSetPacketStreamIndex packet streamIndex
		r <- withFramePtr $ withForeignPtr packetFPtr . flaw_ffmpeg_encode formatContextPtr
		if r == 0 then (packet :) <$> step restFrames
		else if r > 0 then noPacketAction
		else throwIO $ DescribeFirstException "failed to FFmpeg encode"
	step fs

ffmpegOpenDecoder :: FFmpegAVStream -> IO ((), IO ())
ffmpegOpenDecoder (FFmpegAVStream streamPtr) = do
	checkAVError "open decoder" $ flaw_ffmpeg_openDecoder streamPtr
	return ((), flaw_ffmpeg_closeCodec streamPtr)

ffmpegAddOutputStream :: FFmpegAVFormatContext -> T.Text -> IO (FFmpegAVStream, IO ())
ffmpegAddOutputStream (FFmpegAVFormatContext formatContextPtr) codecName = do
	streamPtr <- B.useAsCString (T.encodeUtf8 codecName) $ flaw_ffmpeg_addOutputStream formatContextPtr
	when (streamPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to add FFmpeg output stream"
	return (FFmpegAVStream streamPtr, flaw_ffmpeg_closeCodec streamPtr)

ffmpegCopyOutputStream :: FFmpegAVFormatContext -> FFmpegAVStream -> IO (FFmpegAVStream, IO ())
ffmpegCopyOutputStream (FFmpegAVFormatContext formatContextPtr) (FFmpegAVStream copyFromStreamPtr) = do
	streamPtr <- flaw_ffmpeg_copyOutputStream formatContextPtr copyFromStreamPtr
	when (streamPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to copy FFmpeg output stream"
	return (FFmpegAVStream streamPtr, flaw_ffmpeg_closeCodec streamPtr)

-- | Create video scaler/converter.
-- Format, width and height arguments are all optional, missing argument means
-- use value from source (don't change format or size).
ffmpegNewScaler :: Maybe FFmpegPixFmt -> Maybe Int -> Maybe Int -> IO (FFmpegAVFrame -> IO FFmpegAVFrame)
ffmpegNewScaler outputFormat outputWidth outputHeight = do
	scalerFPtr <- newForeignPtr flaw_ffmpeg_freeScaler =<< do
		scalerPtr <- flaw_ffmpeg_newScaler
			(maybe (-1) (fromIntegral . fromEnum) outputFormat)
			(maybe (-1) fromIntegral outputWidth)
			(maybe (-1) fromIntegral outputHeight)
		when (scalerPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to create FFmpeg scaler"
		return scalerPtr
	return $ \(FFmpegAVFrame frameFPtr) -> do
		outputFramePtr <- withForeignPtr scalerFPtr $ \scalerPtr ->
			withForeignPtr frameFPtr $ flaw_ffmpeg_scaleVideoFrame scalerPtr
		when (outputFramePtr == nullPtr) $ throwIO $ DescribeFirstException "failed to scale video frame"
		FFmpegAVFrame <$> newForeignPtr flaw_ffmpeg_freeFrame outputFramePtr

ffmpegDecodeSingleAudioStream :: FFmpegAVFormatContext -> (SoundFormat -> Ptr () -> Int -> IO ()) -> IO ()
ffmpegDecodeSingleAudioStream formatContext callback = describeException "failed to decode ffmpeg single audio stream" $ withBook $ \bk -> do

	-- find single audio stream
	stream <- ffmpegGetSingleStreamOfType formatContext FFmpegStreamTypeAudio
	streamIndex <- ffmpegGetStreamIndex stream

	-- open decoder
	book bk $ ffmpegOpenDecoder stream

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
	packets <- ffmpegDemux formatContext
	forM_ packets $ \packet -> do
		packetStreamIndex <- ffmpegGetPacketStreamIndex packet
		when (streamIndex == packetStreamIndex) $ do
			frames <- ffmpegDecode formatContext packet
			forM_ frames $ \(FFmpegAVFrame frameFPtr) -> withForeignPtr frameFPtr $ \framePtr ->
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

foreign import ccall unsafe flaw_ffmpeg_getStreamsCount :: Ptr C_AVFormatContext -> IO CInt
foreign import ccall unsafe flaw_ffmpeg_getStream :: Ptr C_AVFormatContext -> CInt -> IO (Ptr C_AVStream)
foreign import ccall unsafe flaw_ffmpeg_getStreamIndex :: Ptr C_AVStream -> IO CInt
foreign import ccall unsafe flaw_ffmpeg_getStreamType :: Ptr C_AVStream -> IO CInt
foreign import ccall unsafe flaw_ffmpeg_getSingleStreamOfType :: Ptr C_AVFormatContext -> CInt -> IO (Ptr C_AVStream)

foreign import ccall unsafe flaw_ffmpeg_setVideoStreamOptions :: Ptr C_AVStream -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe flaw_ffmpeg_newPacket :: IO (Ptr C_AVPacket)
foreign import ccall safe "&flaw_ffmpeg_freePacket" flaw_ffmpeg_freePacket :: FunPtr (Ptr C_AVPacket -> IO ())
foreign import ccall safe flaw_ffmpeg_refPacket :: Ptr C_AVPacket -> IO (Ptr C_AVPacket)
foreign import ccall unsafe flaw_ffmpeg_getPacketStreamIndex :: Ptr C_AVPacket -> IO CInt
foreign import ccall unsafe flaw_ffmpeg_setPacketStreamIndex :: Ptr C_AVPacket -> CInt -> IO ()
foreign import ccall safe flaw_ffmpeg_demux :: Ptr C_AVFormatContext -> Ptr C_AVPacket -> IO CInt
foreign import ccall safe flaw_ffmpeg_mux :: Ptr C_AVFormatContext -> Ptr C_AVPacket -> IO CInt
foreign import ccall unsafe flaw_ffmpeg_rescalePacketTime :: Ptr C_AVStream -> Ptr C_AVStream -> Ptr C_AVPacket -> IO ()

foreign import ccall safe flaw_ffmpeg_newFrame :: IO (Ptr C_AVFrame)
foreign import ccall safe "&flaw_ffmpeg_freeFrame" flaw_ffmpeg_freeFrame :: FunPtr (Ptr C_AVFrame -> IO ())
foreign import ccall safe flaw_ffmpeg_refFrame :: Ptr C_AVFrame -> IO (Ptr C_AVFrame)
foreign import ccall safe flaw_ffmpeg_decode :: Ptr C_AVFormatContext -> Ptr C_AVPacket -> Ptr C_AVFrame -> IO CInt
foreign import ccall safe flaw_ffmpeg_encode :: Ptr C_AVFormatContext -> Ptr C_AVFrame -> Ptr C_AVPacket -> IO CInt

foreign import ccall safe flaw_ffmpeg_openDecoder :: Ptr C_AVStream -> IO CInt
foreign import ccall safe flaw_ffmpeg_addOutputStream :: Ptr C_AVFormatContext -> Ptr CChar -> IO (Ptr C_AVStream)
foreign import ccall safe flaw_ffmpeg_copyOutputStream :: Ptr C_AVFormatContext -> Ptr C_AVStream -> IO (Ptr C_AVStream)
foreign import ccall safe flaw_ffmpeg_closeCodec :: Ptr C_AVStream -> IO ()

foreign import ccall safe flaw_ffmpeg_initializeOutputContext :: Ptr C_AVFormatContext -> IO CInt
foreign import ccall safe flaw_ffmpeg_finalizeOutputContext :: Ptr C_AVFormatContext -> IO CInt

foreign import ccall safe flaw_ffmpeg_newScaler :: CInt -> CInt -> CInt -> IO (Ptr C_FFmpegScaler)
foreign import ccall safe "&flaw_ffmpeg_freeScaler" flaw_ffmpeg_freeScaler :: FunPtr (Ptr C_FFmpegScaler -> IO ())
foreign import ccall safe flaw_ffmpeg_scaleVideoFrame :: Ptr C_FFmpegScaler -> Ptr C_AVFrame -> IO (Ptr C_AVFrame)

foreign import ccall safe flaw_ffmpeg_packAudioFrame :: Ptr C_AVFrame -> FunPtr DecodeAudioCallback -> IO CInt

-- flaw-ffmpeg structs

data C_FFmpegScaler

-- wrappers

type DecodeAudioCallback = CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt
foreign import ccall "wrapper" wrapDecodeAudioCallback :: DecodeAudioCallback -> IO (FunPtr DecodeAudioCallback)

-- ffmpeg functions

foreign import ccall unsafe av_opt_set :: Ptr () -> Ptr CChar -> Ptr CChar -> CInt -> IO CInt

-- ffmpeg types

data C_AVFormatContext
data C_AVStream
data C_AVPacket
data C_AVFrame

-- ffmpeg enums

-- | Stream type, corresponds to AVMediaType.
data FFmpegStreamType
	= FFmpegStreamTypeVideo
	| FFmpegStreamTypeAudio
	| FFmpegStreamTypeData -- ^ Opaque data usually continuous.
	| FFmpegStreamTypeSubtitle
	| FFmpegStreamTypeAttachment -- ^ Opaque data usually sparse.
	deriving Enum

-- | Pixel format, corresponds to AV_PIX_FMT_*.
data FFmpegPixFmt
	= FFmpegPixFmtYUV420P
	| FFmpegPixFmtYUVV422
	| FFmpegPixFmtRGB24
	| FFmpegPixFmtBGR24
	deriving Enum

pattern AV_SAMPLE_FMT_U8 = 0
pattern AV_SAMPLE_FMT_S16 = 1
pattern AV_SAMPLE_FMT_S32 = 2
pattern AV_SAMPLE_FMT_FLT = 3
pattern AV_SAMPLE_FMT_DBL = 4

pattern AV_OPT_SEARCH_CHILDREN = 1
pattern AVERROR_OPTION_NOT_FOUND = (-0x54504ff8)
pattern AVERROR_EINVAL = (-22)
