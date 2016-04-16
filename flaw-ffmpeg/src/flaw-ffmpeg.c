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

AVFormatContext* flaw_ffmpeg_openOutput(const char* url)
{
	AVFormatContext* ctx = NULL;
	int ret = avformat_alloc_output_context2(&ctx, NULL, NULL, url);
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

void flaw_ffmpeg_getStreamsTypes(AVFormatContext* ctx, int* outStreamsTypes)
{
	for(int i = 0; i < ctx->nb_streams; ++i)
	{
		outStreamsTypes[i] = ctx->streams[i]->codec->codec_type;
	}
}

int flaw_ffmpeg_demux(AVFormatContext* ctx, int (*callback)(AVPacket*, void*), void* opaque)
{
	// init packet struct
	AVPacket pkt;
	av_init_packet(&pkt);
	pkt.data = NULL;
	pkt.size = 0;

	// loop reading packets
	int err = 0, gotPacket;
	do
	{
		gotPacket = av_read_frame(ctx, &pkt) >= 0;
		if(!gotPacket)
		{
			pkt.data = NULL;
			pkt.size = 0;
		}

		int ret = callback(&pkt, opaque);
		if(ret != 0)
		{
			err = ret;
		}

		// free packet
		av_packet_unref(&pkt);
	}
	while(err == 0 && gotPacket);

	return err;
}

struct DecodeState
{
	AVFormatContext* ctx;
	AVFrame* frame;
	int streamsMask;
	int (*callback)(AVFrame*, void*);
	void* callbackOpaque;
};

static int decodeCallback(AVPacket* pkt, void* opaque)
{
	struct DecodeState* state = (struct DecodeState*)opaque;

	// skip streams we don't care about
	if(!(state->streamsMask & (1 << pkt->stream_index))) return 0;

	AVCodecContext* codec = state->ctx->streams[pkt->stream_index]->codec;
	// loop decoding frames
	int gotFrame, err = 0;
	do
	{
		// decode frame
		gotFrame = 0;
		int decoded = 0;
		switch(codec->codec_type)
		{
		case AVMEDIA_TYPE_VIDEO:
			decoded = avcodec_decode_video2(codec, state->frame, &gotFrame, pkt);
			break;
		case AVMEDIA_TYPE_AUDIO:
			decoded = avcodec_decode_audio4(codec, state->frame, &gotFrame, pkt);
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
				int ret = state->callback(state->frame, state->callbackOpaque);
				if(ret != 0)
				{
					err = ret;
				}
			}
			// advance packet
			if(pkt->data)
			{
				pkt->data += decoded;
				pkt->size -= decoded;
			}
		}
	}
	while(err == 0 && (pkt->data ? pkt->size > 0 : gotFrame));

	return err;
}

// Decode streams using decode callback provided.
// If callback returns non-zero value, decoding is stopped and that value is returned.
int flaw_ffmpeg_decode(AVFormatContext* ctx, int streamsMask, int (*callback)(AVFrame*, void*), void* callbackOpaque)
{
	// open decoders for streams needed
	for(int i = 0; i < ctx->nb_streams; ++i)
	{
		if(streamsMask & (1 << i))
		{
			AVCodecContext* codec = ctx->streams[i]->codec;
			AVCodec* decoder = avcodec_find_decoder(codec->codec_id);
			int ret = decoder ? avcodec_open2(codec, decoder, NULL) : -1;
			if(ret < 0)
			{
				// close already opened decoders
				for(int j = 0; j < i; ++j)
				{
					if(streamsMask & (1 << j))
					{
						avcodec_close(ctx->streams[j]->codec);
					}
				}
				return ret;
			}
		}
	}

	// init state
	struct DecodeState state;
	state.ctx = ctx;
	state.frame = av_frame_alloc();
	state.streamsMask = streamsMask;
	state.callback = callback;
	state.callbackOpaque = callbackOpaque;

	int err = flaw_ffmpeg_demux(ctx, decodeCallback, &state);

	// close frame
	av_frame_free(&state.frame);

	// close decoders
	for(int i = 0; i < ctx->nb_streams; ++i)
	{
		if(streamsMask & (1 << i))
		{
			avcodec_close(ctx->streams[i]->codec);
		}
	}

	return err;
}

struct RemuxState
{
	AVFormatContext* inctx;
	AVFormatContext* outctx;
};

int remuxCallback(AVPacket* pkt, void* opaque)
{
	struct RemuxState* state = (struct RemuxState*)opaque;

	// skip empty packets
	if(!pkt->data) return 0;

	AVStream* inStream = state->inctx->streams[pkt->stream_index];
	AVStream* outStream = state->outctx->streams[pkt->stream_index];

	// copy packet
	av_packet_rescale_ts(pkt, inStream->time_base, outStream->time_base);
	pkt->pos = -1;

	return av_interleaved_write_frame(state->outctx, pkt);
}

int flaw_ffmpeg_remux(AVFormatContext* inctx, AVFormatContext* outctx)
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

	int ret;

	// write header
	if((ret = avformat_write_header(outctx, NULL)) != 0)
	{
		return ret;
	}

	// copy packets
	struct RemuxState state;
	state.inctx = inctx;
	state.outctx = outctx;
	if((ret = flaw_ffmpeg_demux(inctx, remuxCallback, &state)) != 0)
	{
		return ret;
	}

	// write trailer
	return av_write_trailer(outctx);
}

struct ChainedDecodeCallback
{
	void* callback;
	void* opaque;
};

typedef int (*PackAudioCallback)(AVFrame* frame, int packedFormat, void* packedData, int packedSize, void* opaque);

// ensure audio is in a packed format, and pass it further
int flaw_ffmpeg_packAudio(AVFrame* frame, void* opaque)
{
	int samplesCount = frame->nb_samples;
	int bytesPerSample = av_get_bytes_per_sample(frame->format);
	int channelsCount = av_frame_get_channels(frame);
	int bytesPerInterleavedSample = bytesPerSample * channelsCount;
	int interleavedDataSize = samplesCount * bytesPerInterleavedSample;

	PackAudioCallback callback = (PackAudioCallback)((struct ChainedDecodeCallback*)opaque)->callback;
	void* callbackOpaque = ((struct ChainedDecodeCallback*)opaque)->opaque;

	if(av_sample_fmt_is_planar(frame->format))
	{
		// convert to packed format
		uint8_t interleavedData[interleavedDataSize];
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
		// output packed data
		callback(frame, av_get_packed_sample_fmt(frame->format), interleavedData, interleavedDataSize, callbackOpaque);
	}
	else
	{
		// output packed data
		callback(frame, frame->format, frame->extended_data[0], interleavedDataSize, callbackOpaque);
	}
}

typedef int (*DecodeAudioCallback)(int samplesPerSecond, int sampleFormat, int channelsCount, const void* data, int size);

static int decodePackedAudioCallback(AVFrame* frame, int format, void* data, int size, void* opaque)
{
	return ((DecodeAudioCallback)opaque)(av_frame_get_sample_rate(frame), format, av_frame_get_channels(frame), data, size);
}

// find a single audio stream and decode it, converting from planar format if needed
int flaw_ffmpeg_decodeSinglePackedAudioStream(AVFormatContext* ctx, DecodeAudioCallback callback)
{
	// find audio stream
	int streamsCount = flaw_ffmpeg_getStreamsCount(ctx);
	int streamsTypes[streamsCount];
	flaw_ffmpeg_getStreamsTypes(ctx, streamsTypes);
	int audioStreamIndex = -1;
	for(int i = 0; i < streamsCount; ++i)
	{
		if(streamsTypes[i] == AVMEDIA_TYPE_AUDIO)
		{
			if(audioStreamIndex < 0)
			{
				audioStreamIndex = i;
			}
			else
			{
				// more than one audio stream
				return -1;
			}
		}
	}
	// no audio streams
	if(audioStreamIndex < 0)
	{
		return -1;
	}

	// decode
	struct ChainedDecodeCallback chainedCallback;
	chainedCallback.callback = &decodePackedAudioCallback;
	chainedCallback.opaque = callback;
	return flaw_ffmpeg_decode(ctx, 1 << audioStreamIndex, flaw_ffmpeg_packAudio, &chainedCallback);
}
