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

static AVStream* getSingleStreamWithType(AVFormatContext* ctx, enum AVMediaType codecType)
{
	int r = -1;
	for(int i = 0; i < ctx->nb_streams; ++i)
		if(ctx->streams[i]->codec->codec_type == codecType)
			if(r < 0) r = i;
			else return NULL;
	return r >= 0 ? ctx->streams[r] : NULL;
}

AVStream* flaw_ffmpeg_getSingleVideoStream(AVFormatContext* ctx)
{
	return getSingleStreamWithType(ctx, AVMEDIA_TYPE_VIDEO);
}

AVStream* flaw_ffmpeg_getSingleAudioStream(AVFormatContext* ctx)
{
	return getSingleStreamWithType(ctx, AVMEDIA_TYPE_AUDIO);
}

int flaw_ffmpeg_prepareStreamDecoding(AVStream* stream)
{
	int ret;
	AVCodec* decoder = avcodec_find_decoder(stream->codec->codec_id);
	if(!decoder) return -1;
	if((ret = avcodec_open2(stream->codec, decoder, NULL)) < 0) return ret;
	return 0;
}

void flaw_ffmpeg_getAudioStreamFormat(AVStream* stream, int* outSamplesPerSecond, int* outFormat, int* outChannelsCount)
{
	*outSamplesPerSecond = stream->codec->sample_rate;
	*outFormat = av_get_packed_sample_fmt(stream->codec->sample_fmt);
	*outChannelsCount = stream->codec->channels;
}

int flaw_ffmpeg_decodeAudio(AVFormatContext* ctx, AVStream* stream, void (*output)(const void*, int))
{
	// some calculations
	int channelsCount = stream->codec->channels;
	int bytesPerSample = av_get_bytes_per_sample(stream->codec->sample_fmt);
	int planar = av_sample_fmt_is_planar(stream->codec->sample_fmt);
	int bytesPerInterleavedSample = bytesPerSample * channelsCount;

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
		if(pkt.stream_index == stream->index)
		{
			AVPacket origPkt = pkt;
			// loop decoding packets
			int gotFrame;
			do
			{
				// decode audio frame
				gotFrame = 0;
				int decoded = avcodec_decode_audio4(stream->codec, frame, &gotFrame, &pkt);
				if(decoded < 0)
				{
					err = decoded;
					break;
				}
				if(gotFrame)
				{
					int samplesCount = frame->nb_samples;
					int interleavedDataSize = samplesCount * bytesPerSample * channelsCount;
					if(planar)
					{
						// convert to interleaved format
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
						// output interleaved data
						output(interleavedData, interleavedDataSize);
					}
					else
					{
						// output interleaved data
						output(frame->extended_data[0], interleavedDataSize);
					}
				}
				// advance packet
				if(gotPacket)
				{
					pkt.data += decoded;
					pkt.size -= decoded;
				}
			}
			while(err >= 0 && (gotPacket ? pkt.size > 0 : gotFrame));
			// free packet
			av_packet_unref(&origPkt);
		}
	}
	while(err >= 0 && gotPacket);

	avcodec_close(stream->codec);
	av_frame_free(&frame);

	return err;
}
