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
	int r = avformat_open_input(&ctx, url, NULL, NULL);
	END_DISABLE_ALARMS();
	if(r == 0) return ctx;
	avformat_free_context(ctx);
	return NULL;
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

	// init frame
	AVFrame* frame = av_frame_alloc();

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
		if(streamsMask & (1 << pkt.stream_index))
		{
			AVCodecContext* codec = ctx->streams[pkt.stream_index]->codec;
			AVPacket origPkt = pkt;
			// loop decoding frames
			int gotFrame;
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
						int ret = callback(frame, callbackOpaque);
						if(ret != 0)
						{
							err = ret;
						}
					}
					// advance packet
					if(gotPacket)
					{
						pkt.data += decoded;
						pkt.size -= decoded;
					}
				}
			}
			while(err >= 0 && (gotPacket ? pkt.size > 0 : gotFrame));
		}
		// free packet
		av_packet_unref(&pkt);
	}
	while(err >= 0 && gotPacket);

	// close frame
	av_frame_free(&frame);

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
