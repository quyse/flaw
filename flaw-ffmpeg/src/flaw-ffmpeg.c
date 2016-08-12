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

AVStream* flaw_ffmpeg_getStream(AVFormatContext* ctx, int i)
{
	return ctx->streams[i];
}

int flaw_ffmpeg_getStreamIndex(AVStream* stream)
{
	return stream->index;
}

int flaw_ffmpeg_getStreamType(AVStream* stream)
{
	return stream->codec->codec_type;
}

AVStream* flaw_ffmpeg_getSingleStreamOfType(AVFormatContext* ctx, int mediaType)
{
	AVStream* r = NULL;
	for(int i = 0; i < ctx->nb_streams; ++i)
		if(ctx->streams[i]->codec->codec_type == mediaType)
		{
			if(!r) r = ctx->streams[i];
			else return NULL;
		}
	return r;
}

void flaw_ffmpeg_setVideoStreamOptions(AVStream* stream, int format, int width, int height)
{
	stream->codec->pix_fmt = format;
	stream->codec->width = width;
	stream->codec->height = height;
}

AVPacket* flaw_ffmpeg_newPacket()
{
	return av_packet_alloc();
}

void flaw_ffmpeg_freePacket(AVPacket* pkt)
{
	av_packet_free(&pkt);
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

int flaw_ffmpeg_isPacketEmpty(AVPacket* pkt)
{
	return !pkt->data || pkt->size <= 0;
}

int flaw_ffmpeg_getPacketStreamIndex(AVPacket* pkt)
{
	return pkt->stream_index;
}

void flaw_ffmpeg_setPacketStreamIndex(AVPacket* pkt, int streamIndex)
{
	pkt->stream_index = streamIndex;
}

void flaw_ffmpeg_setPacketTime(AVPacket* pkt, int64_t pts)
{
	pkt->pts = pts;
}

void flaw_ffmpeg_rescalePacketTime(AVStream* fromStream, AVStream* toStream, AVPacket* pkt)
{
	av_packet_rescale_ts(pkt, fromStream->time_base, toStream->time_base);
	pkt->pos = -1;
}

int flaw_ffmpeg_demux(AVFormatContext* ctx, AVPacket* pkt)
{
	return av_read_frame(ctx, pkt);
}

int flaw_ffmpeg_mux(AVFormatContext* ctx, AVPacket* pkt)
{
	// skip empty packets
	if(!pkt->data)
		return 0;
	return av_interleaved_write_frame(ctx, pkt);
}

AVFrame* flaw_ffmpeg_newFrame()
{
	return av_frame_alloc();
}

void flaw_ffmpeg_freeFrame(AVFrame* frame)
{
	av_frame_free(&frame);
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

void flaw_ffmpeg_setFrameTime(AVFrame* frame, int64_t time)
{
	frame->pts = time;
}

void flaw_ffmpeg_frameGetAudio(AVFrame* frame, int* outSamplesCount, int* outSamplesPerSecond, int* outSampleFormat, int* outChannelsCount, int* outSize)
{
	*outSamplesCount = frame->nb_samples;
	*outSamplesPerSecond = av_frame_get_sample_rate(frame);
	*outSampleFormat = av_get_packed_sample_fmt(frame->format);
	*outChannelsCount = av_frame_get_channels(frame);
	*outSize = frame->nb_samples * av_get_bytes_per_sample(frame->format) * av_frame_get_channels(frame);
}

int flaw_ffmpeg_decode(AVStream* stream, AVPacket* pkt, AVFrame* frame)
{
	AVCodecContext* codec = stream->codec;
	int gotFrame;
	do
	{
		gotFrame = 0;
		int decoded = 0;
		switch(codec->codec_type)
		{
		case AVMEDIA_TYPE_VIDEO:
			decoded = avcodec_decode_video2(codec, frame, &gotFrame, pkt);
			break;
		case AVMEDIA_TYPE_AUDIO:
			decoded = avcodec_decode_audio4(codec, frame, &gotFrame, pkt);
			break;
		}
		if(decoded < 0) return decoded;
		if(pkt->data)
		{
			pkt->data += decoded;
			pkt->size -= decoded;
		}
	}
	while(!gotFrame && pkt->data && pkt->size > 0);
	return !gotFrame;
}

int flaw_ffmpeg_encode(AVStream* stream, AVFrame* frame, AVPacket* pkt)
{
	AVCodecContext* codec = stream->codec;
	int gotPacket = 0, ret = -1;
	switch(codec->codec_type)
	{
	case AVMEDIA_TYPE_VIDEO:
		ret = avcodec_encode_video2(codec, pkt, frame, &gotPacket);
		break;
	case AVMEDIA_TYPE_AUDIO:
		ret = avcodec_encode_audio2(codec, pkt, frame, &gotPacket);
		break;
	}
	if(ret < 0) return ret;
	return !gotPacket;
}

int flaw_ffmpeg_openDecoder(AVStream* stream)
{
	AVCodecContext* codec = stream->codec;
	AVCodec* decoder = avcodec_find_decoder(codec->codec_id);
	AVDictionary* opts = NULL;
	av_dict_set(&opts, "refcounted_frames", "1", 0);
	int ret = decoder ? avcodec_open2(codec, decoder, &opts) : -1;
	av_dict_free(&opts);
	return ret;
}

AVStream* flaw_ffmpeg_addOutputStream(AVFormatContext* ctx, const char* codecName, int timeBaseNum, int timeBaseDen)
{
	AVCodec* encoder = avcodec_find_encoder_by_name(codecName);
	if(!encoder) return NULL;
	AVStream* stream = avformat_new_stream(ctx, encoder);
	if(!stream) return NULL;
	stream->time_base.num = timeBaseNum;
	stream->time_base.den = timeBaseDen;
	if(ctx->oformat->flags & AVFMT_GLOBALHEADER)
		stream->codec->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;
	return stream;
}

int flaw_ffmpeg_openEncoder(AVStream* stream)
{
	return avcodec_open2(stream->codec, NULL, NULL);
}

AVStream* flaw_ffmpeg_copyOutputStream(AVFormatContext* ctx, AVStream* copyFromStream)
{
	AVStream* stream = avformat_new_stream(ctx, copyFromStream->codec->codec);
	if(!stream) return NULL;
	AVCodecParameters* params = avcodec_parameters_alloc();
	int r =
		avcodec_parameters_from_context(params, copyFromStream->codec) >= 0 &&
		avcodec_parameters_to_context(stream->codec, params) == 0;
	avcodec_parameters_free(&params);
	stream->codec->codec_tag = 0;
	if(ctx->oformat->flags & AVFMT_GLOBALHEADER)
		stream->codec->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;
	return stream;
}

void flaw_ffmpeg_closeCodec(AVStream* stream)
{
	avcodec_close(stream->codec);
}

int flaw_ffmpeg_initializeOutputContext(AVFormatContext* ctx)
{
	return avformat_write_header(ctx, NULL);
}

int flaw_ffmpeg_finalizeOutputContext(AVFormatContext* ctx)
{
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

// get audio from frame, converting to packed format if needed
void flaw_ffmpeg_packAudioFrame(AVFrame* frame, uint8_t* buf)
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
		// convert samples to interleaved
		for(int i = 0; i < channelsCount; ++i)
		{
			uint8_t* interleavedChannelData = buf + i * bytesPerSample;
			const uint8_t* planarChannelData = frame->extended_data[i];
			for(int j = 0; j < samplesCount; ++j)
			{
				uint8_t* interleavedSamplePtr = interleavedChannelData + j * bytesPerInterleavedSample;
				const uint8_t* planarSamplePtr = planarChannelData + j * bytesPerSample;
				for(int k = 0; k < bytesPerSample; ++k)
					interleavedSamplePtr[k] = planarSamplePtr[k];
			}
		}
	}
	else
	{
		memcpy(buf, frame->extended_data[0], frame->linesize[0]);
	}
}
