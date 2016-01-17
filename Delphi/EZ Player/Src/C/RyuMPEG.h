#ifdef BUILD_DLL
    #define DLLEXPORT __declspec(dllexport)
#else
    #define DLLEXPORT __declspec(dllimport)
#endif

#ifndef _RyuMPEG_H_
#define	_RyuMPEG_H_

#include "libavcodec/avcodec.h"
#include "libavformat/avformat.h"
#include "libswscale/swscale.h"

#define RYU_TIME_BASE (AVRational){8, AV_TIME_BASE}

#define ERROR_GENERAL -1;
#define ERROR_CAN_NOT_OPEN_FILE -2;
#define ERROR_CAN_NOT_FIND_STREAMINFO -3;
#define ERROR_CAN_NOT_OPEN_VIDEOCODEC -4;
#define ERROR_CAN_NOT_OPEN_AUDIOCODEC -5;
#define ERROR_HAS_NO_MEDIA_STREAM -6;

// PacketType
#define UNKNOWN_PACKET 0;
#define VIDEO_PACKET 1;
#define AUDIO_PACKET 2;

#pragma pack(push,1)

typedef struct _VideoInfo {
	int codecID;
	int pixelFormat;
	int width;
	int height;
	int fps;
	int bitrate;
	int frameSize;
} VideoInfo;

typedef struct _AudioInfo {
	int codecID;
	int sampleRate;
	int channels;
} AudioInfo;

typedef struct _MediaInfo {
	char videoInfoString[1024*16];
	char audioInfoString[1024*16];

	int64_t duration;

	VideoInfo videoInfo;
	AudioInfo audioInfo;
} MediaInfo;

#pragma pack(pop)

typedef struct _RyuMPEG_Handle {
	AVFormatContext *pFormatCtx;

	int isEOF;
	int isStoped;
	int isMoving;
	int isFrameFinished;

	int currentPosition;

	int videoStream;
	int audioStream;

	AVCodecContext *pVideoCtx;
	AVCodecContext *pAudioCtx;

	AVCodec *pVideoCodec;
	AVCodec *pAudioCodec;

	AVFrame *pFrame;

	AVFrame *pFrameRGB;
	uint8_t *pFrameRGB_Buffer;

	int sws_flags;
	struct SwsContext *img_convert_ctx;

	uint8_t audio_buf[AVCODEC_MAX_AUDIO_FRAME_SIZE * 2];
} RyuMPEG_Handle;

#endif /* _RyuMPEG_H_ */
