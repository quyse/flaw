#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/imgutils.h>
#include <libswscale/swscale.h>
#include <signal.h>

// Hacky macros to disable alarm signals temporarily.
// FFmpeg doesn't handle syscall interruptions properly,
// and GHC runtime uses alarms a lot.
#define BEGIN_DISABLE_ALARMS() \
	sigset_t disableAlarmsMask, oldMask; \
	sigemptyset(&disableAlarmsMask); \
	sigaddset(&disableAlarmsMask, SIGALRM); \
	sigaddset(&disableAlarmsMask, SIGVTALRM); \
	pthread_sigmask(SIG_BLOCK, &disableAlarmsMask, &oldMask)
#define END_DISABLE_ALARMS() \
	pthread_sigmask(SIG_SETMASK, &oldMask, NULL)

void flaw_ffmpeg_init()
{
	av_register_all();
	avformat_network_init();
}

AVFormatContext* flaw_ffmpeg_openInput(const char* url)
{
	AVFormatContext* ctx = NULL;
	BEGIN_DISABLE_ALARMS();
	int ret = avformat_open_input(&ctx, url, NULL, NULL);
	END_DISABLE_ALARMS();
	if(ret != 0)
	{
		goto err;
	}

	if(avformat_find_stream_info(ctx, NULL) != 0)
	{
		goto err;
	}

	return ctx;

err:
	avformat_free_context(ctx);
	return NULL;
}

void flaw_ffmpeg_closeInput(AVFormatContext* ctx)
{
	avformat_close_input(&ctx);
}

AVFormatContext* flaw_ffmpeg_openOutput(const char* url, const char* format)
{
	AVFormatContext* ctx = NULL;
	int ret = avformat_alloc_output_context2(&ctx, NULL, format, url);
	if(ret != 0)
	{
		goto err;
	}

	// open file if needed
	if(!(ctx->oformat->flags & AVFMT_NOFILE))
	{
		if(avio_open(&ctx->pb, url, AVIO_FLAG_WRITE) != 0)
		{
			goto err;
		}
	}

	return ctx;

err:
	avformat_free_context(ctx);
	return NULL;
}

void flaw_ffmpeg_closeOutput(AVFormatContext* ctx)
{
	if(!ctx) return;

	// close file if it was opened by us
	if(!(ctx->oformat->flags & AVFMT_NOFILE))
	{
		avio_closep(&ctx->pb);
	}

	avformat_free_context(ctx);
}

int flaw_ffmpeg_getStreamsCount(AVFormatContext* ctx)
{
	return ctx->nb_streams;
}

int flaw_ffmpeg_getStreamType(AVFormatContext* ctx, int i)
{
	return ctx->streams[i]->codec->codec_type;
}

int flaw_ffmpeg_getSingleStreamOfType(AVFormatContext* ctx, int mediaType)
{
	int r = -1;
	for(int i = 0; i < ctx->nb_streams; ++i)
		if(ctx->streams[i]->codec->codec_type == mediaType)
		{
			if(r < 0) r = i;
			else return -1;
		}
	return r;
}

AVPacket* flaw_ffmpeg_refPacket(AVPacket* pkt)
{
	AVPacket* newpkt = av_packet_alloc();
	if(av_packet_ref(newpkt, pkt) != 0)
	{
		av_packet_free(&newpkt);
	}
	return newpkt;
}

void flaw_ffmpeg_freePacket(AVPacket* pkt)
{
	av_packet_free(&pkt);
}

int flaw_ffmpeg_getPacketStreamIndex(AVPacket* pkt)
{
	return pkt->stream_index;
}

int flaw_ffmpeg_demux(AVFormatContext* ctx, int (*callback)(AVPacket*))
{
	// init packet struct
	AVPacket* pkt = av_packet_alloc();

	// loop reading packets
	int err = 0, gotPacket;
	do
	{
		gotPacket = av_read_frame(ctx, pkt) >= 0;

		int ret = callback(pkt);
		if(ret != 0)
		{
			err = ret;
		}

		av_packet_unref(pkt);
	}
	while(err == 0 && gotPacket);

	av_packet_free(&pkt);

	return err;
}

AVFrame* flaw_ffmpeg_refFrame(AVFrame* frame)
{
	AVFrame* newFrame = av_frame_alloc();
	if(av_frame_ref(newFrame, frame) != 0)
	{
		av_frame_free(&newFrame);
	}
	return newFrame;
}

void flaw_ffmpeg_freeFrame(AVFrame* frame)
{
	av_frame_free(&frame);
}

int flaw_ffmpeg_decode(AVFormatContext* ctx, AVPacket* pktOrig, int (*callback)(AVFrame*))
{
	AVPacket pkt = *pktOrig;
	AVCodecContext* codec = ctx->streams[pkt.stream_index]->codec;
	// loop decoding frames
	AVFrame* frame = av_frame_alloc();
	int gotFrame, err = 0;
	do
	{
		// decode frame
		gotFrame = 0;
		int decoded = 0;

		switch(codec->codec_type)
		{
		case AVMEDIA_TYPE_VIDEO:
			decoded = avcodec_decode_video2(codec, frame, &gotFrame, &pkt);
			break;
		case AVMEDIA_TYPE_AUDIO:
			decoded = avcodec_decode_audio4(codec, frame, &gotFrame, &pkt);
			break;
		case AVMEDIA_TYPE_DATA:
			break;
		case AVMEDIA_TYPE_SUBTITLE:
			break;
		case AVMEDIA_TYPE_ATTACHMENT:
			break;
		}

		if(decoded < 0)
		{
			err = decoded;
		}
		else
		{
			// output frame
			if(gotFrame)
			{
				int ret = callback(frame);
				if(ret != 0)
				{
					err = ret;
				}
			}
			// advance packet
			if(pkt.data)
			{
				pkt.data += decoded;
				pkt.size -= decoded;
			}
		}

		av_frame_unref(frame);
	}
	while(err == 0 && (pkt.data ? pkt.size > 0 : gotFrame));

	av_frame_free(&frame);

	return err;
}

int flaw_ffmpeg_openCodec(AVFormatContext* ctx, int streamIndex)
{
	AVCodecContext* codec = ctx->streams[streamIndex]->codec;
	AVCodec* decoder = avcodec_find_decoder(codec->codec_id);
	AVDictionary* opts = NULL;
	av_dict_set(&opts, "refcounted_frames", "1", 0);
	int ret = decoder ? avcodec_open2(codec, decoder, &opts) : -1;
	av_dict_free(&opts);
	return ret;
}

void flaw_ffmpeg_closeCodec(AVFormatContext* ctx, int streamIndex)
{
	avcodec_close(ctx->streams[streamIndex]->codec);
}

int flaw_ffmpeg_prepareRemux(AVFormatContext* inctx, AVFormatContext* outctx)
{
	// create output streams
	for(int i = 0; i < inctx->nb_streams; ++i)
	{
		AVStream* inStream = inctx->streams[i];
		AVStream* outStream = avformat_new_stream(outctx, inStream->codec->codec);
		if(!outStream || avcodec_copy_context(outStream->codec, inStream->codec) != 0)
		{
			return -1;
		}
		outStream->codec->codec_tag = 0;
		if(outctx->oformat->flags & AVFMT_GLOBALHEADER)
		{
			outStream->codec->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;
		}
	}

	// write header
	return avformat_write_header(outctx, NULL);
}

int flaw_ffmpeg_remuxPacket(AVFormatContext* inctx, AVFormatContext* outctx, AVPacket* pkt)
{
	// skip empty packets
	if(!pkt->data) return 0;

	AVStream* inStream = inctx->streams[pkt->stream_index];
	AVStream* outStream = outctx->streams[pkt->stream_index];

	av_packet_rescale_ts(pkt, inStream->time_base, outStream->time_base);
	pkt->pos = -1;
	return av_interleaved_write_frame(outctx, pkt);
}

int flaw_ffmpeg_finalizeOutputContext(AVFormatContext* ctx)
{
	// write trailer
	return av_write_trailer(ctx);
}

struct FFmpegScaler
{
	int inputFormat, inputWidth, inputHeight;
	int outputFormat, outputWidth, outputHeight;
	struct SwsContext* context;
};

struct FFmpegScaler* flaw_ffmpeg_newScaler(int outputFormat, int outputWidth, int outputHeight)
{
	struct FFmpegScaler* scaler = (struct FFmpegScaler*)av_malloc(sizeof(struct FFmpegScaler));
	scaler->inputFormat = -1;
	scaler->inputWidth = -1;
	scaler->inputHeight = -1;
	scaler->outputFormat = outputFormat;
	scaler->outputWidth = outputWidth;
	scaler->outputHeight = outputHeight;
	scaler->context = NULL;
	return scaler;
}

void flaw_ffmpeg_freeScaler(struct FFmpegScaler* scaler)
{
	sws_freeContext(scaler->context);
	av_free(scaler);
}

AVFrame* flaw_ffmpeg_scaleVideoFrame(struct FFmpegScaler* scaler, AVFrame* frame)
{
	int outputFormat = scaler->outputFormat < 0 ? frame->format : scaler->outputFormat;
	int outputWidth = scaler->outputWidth < 0 ? frame->width : scaler->outputWidth;
	int outputHeight = scaler->outputHeight < 0 ? frame->height : scaler->outputHeight;

	if(scaler->inputFormat != frame->format || scaler->inputWidth != frame->width || scaler->inputHeight != frame->height)
	{
		sws_freeContext(scaler->context);
		scaler->inputFormat = frame->format;
		scaler->inputWidth = frame->width;
		scaler->inputHeight = frame->height;
		scaler->context = sws_getContext(
			scaler->inputWidth, scaler->inputHeight, scaler->inputFormat,
			outputWidth, outputHeight, outputFormat,
			SWS_FAST_BILINEAR, NULL, NULL, NULL);
		if(!scaler->context) return NULL;
	}

	// allocate frame
	AVFrame* outFrame = av_frame_alloc();
	outFrame->format = outputFormat;
	outFrame->width = outputWidth;
	outFrame->height = outputHeight;
	if(av_frame_get_buffer(outFrame, 16) != 0)
	{
		av_frame_free(&outFrame);
		return NULL;
	}

	sws_scale(scaler->context, (const uint8_t*const*)frame->data, frame->linesize, 0, frame->height, frame->data, frame->linesize);

	return outFrame;
}

typedef int (*DecodeAudioCallback)(int samplesPerSecond, int sampleFormat, int channelsCount, const void* data, int size);

// get audio from frame, converting to packed format if needed
int flaw_ffmpeg_packAudioFrame(AVFrame* frame, DecodeAudioCallback callback)
{
	int samplesCount = frame->nb_samples;
	int samplesPerSecond = av_frame_get_sample_rate(frame);
	int bytesPerSample = av_get_bytes_per_sample(frame->format);
	int channelsCount = av_frame_get_channels(frame);
	int bytesPerInterleavedSample = bytesPerSample * channelsCount;
	int interleavedSize = samplesCount * bytesPerInterleavedSample;

	// if frame's data is planar, convert to packed data
	if(av_sample_fmt_is_planar(frame->format))
	{
		uint8_t interleavedData[interleavedSize];
		for(int i = 0; i < channelsCount; ++i)
		{
			uint8_t* interleavedChannelData = interleavedData + i * bytesPerSample;
			const uint8_t* planarChannelData = frame->extended_data[i];
			for(int j = 0; j < samplesCount; ++j)
			{
				uint8_t* interleavedSamplePtr = interleavedChannelData + j * bytesPerInterleavedSample;
				const uint8_t* planarSamplePtr = planarChannelData + j * bytesPerSample;
				for(int k = 0; k < bytesPerSample; ++k)
					interleavedSamplePtr[k] = planarSamplePtr[k];
			}
		}
		return callback(samplesPerSecond, av_get_packed_sample_fmt(frame->format), channelsCount, interleavedData, interleavedSize);
	}
	else
	{
		return callback(samplesPerSecond, frame->format, channelsCount, frame->extended_data[0], interleavedSize);
	}
}
