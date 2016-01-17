#include <stdio.h>
#include <stdbool.h>
#include "libavcodec/avcodec.h"
#include "libavformat/avformat.h"
#include "libswscale/swscale.h"
#include "RyuMPEG.h"


const int PICXEL_FORMAT = PIX_FMT_BGRA;
const int PICXEL_SIZE = 4;
const int READ_BUFFER_SIZE = 16 * 1024 * 1024;


RyuMPEG_Handle *createRyuMPEGHandle() {
	RyuMPEG_Handle *pHandle = malloc(sizeof(RyuMPEG_Handle));

	pHandle->isEOF = false;
	pHandle->pFormatCtx = NULL;
	pHandle->pVideoCtx = NULL;
	pHandle->pAudioCtx = NULL;
	pHandle->pVideoCodec = NULL;
	pHandle->pAudioCodec = NULL;
	pHandle->pFrame = avcodec_alloc_frame();
	pHandle->pFrameRGB = avcodec_alloc_frame();
	pHandle->pFrameRGB_Buffer = NULL;
//	pHandle->sws_flags = SWS_BICUBIC;
	pHandle->sws_flags = SWS_FAST_BILINEAR;
	pHandle->img_convert_ctx = NULL;

	return pHandle;
}

extern void closeRyuMPEG(RyuMPEG_Handle *pHandle) {
	if (pHandle == NULL) return;

	pHandle->isEOF = true;

	if (pHandle->videoStream > -1) {
		avcodec_flush_buffers(pHandle->pVideoCtx);
	}

	// 아래는 에러 나서 변경 후 다시 에러 나서 원래대로 하니 안남 ㅡ.ㅡ
	//	if (pHandle->pFormatCtx != NULL) avcodec_close(pHandle->pFormatCtx);
	if (pHandle->pAudioCtx != NULL) avcodec_close(pHandle->pAudioCtx);
	if (pHandle->pVideoCtx != NULL) avcodec_close(pHandle->pVideoCtx);

	if (pHandle->pFormatCtx != NULL) av_close_input_file(pHandle->pFormatCtx);

	if (pHandle->pFrameRGB_Buffer != NULL) av_free(pHandle->pFrameRGB_Buffer);
	if (pHandle->pFrameRGB != NULL) av_free(pHandle->pFrameRGB);

	if (pHandle->pFrame != NULL) av_free(pHandle->pFrame);

	if (pHandle->img_convert_ctx != NULL) {
		sws_freeContext(pHandle->img_convert_ctx);
		pHandle->img_convert_ctx = NULL;
	}

	free(pHandle);
}

extern RyuMPEG_Handle *openRyuMPEG(char *fileName, int *errorCode) {
	RyuMPEG_Handle *pHandle = createRyuMPEGHandle();

	av_register_all();

	*errorCode = 0;

	if (avformat_open_input(&pHandle->pFormatCtx, fileName, NULL, 0) != 0) {
		*errorCode = ERROR_CAN_NOT_OPEN_FILE;
		goto error;
	}

	if (av_find_stream_info(pHandle->pFormatCtx) < 0) {
		*errorCode = ERROR_CAN_NOT_FIND_STREAMINFO;
		goto error;
	}

	pHandle->videoStream = -1;
	pHandle->audioStream = -1;

	int i;
	for (i = 0; i < pHandle->pFormatCtx->nb_streams; i++) {
		if (pHandle->pFormatCtx->streams[i]->codec->codec_type
				== AVMEDIA_TYPE_VIDEO && pHandle->videoStream < 0) {
			pHandle->videoStream = i;
			break;
		}
	}
	for (i = 0; i < pHandle->pFormatCtx->nb_streams; i++) {
		if (pHandle->pFormatCtx->streams[i]->codec->codec_type
				== AVMEDIA_TYPE_AUDIO && pHandle->audioStream < 0) {
			pHandle->audioStream = i;
			break;
		}
	}

	if ((pHandle->videoStream == -1) && (pHandle->audioStream == -1)) {
		*errorCode = ERROR_HAS_NO_MEDIA_STREAM;
		goto error;
	}

	if (pHandle->videoStream > -1) {
		pHandle->pVideoCtx
				= pHandle->pFormatCtx->streams[pHandle->videoStream]->codec;

		pHandle->pVideoCodec = avcodec_find_decoder(pHandle->pVideoCtx->codec_id);
		if (pHandle->pVideoCodec == NULL) {
			*errorCode = ERROR_CAN_NOT_OPEN_VIDEOCODEC;
			goto error;
		}

		if (avcodec_open(pHandle->pVideoCtx, pHandle->pVideoCodec) < 0) {
			*errorCode = ERROR_CAN_NOT_OPEN_VIDEOCODEC;
			goto error;
		}

		int bytes = avpicture_get_size(PICXEL_FORMAT, pHandle->pVideoCtx->width, pHandle->pVideoCtx->height);
		pHandle->pFrameRGB_Buffer = (uint8_t *) av_malloc(bytes * sizeof(uint8_t));

		avpicture_fill((AVPicture *) pHandle->pFrameRGB, pHandle->pFrameRGB_Buffer, PICXEL_FORMAT,
				pHandle->pVideoCtx->width, pHandle->pVideoCtx->height);

		pHandle->img_convert_ctx = sws_getContext(
			pHandle->pVideoCtx->width, pHandle->pVideoCtx->height, pHandle->pVideoCtx->pix_fmt,
			pHandle->pVideoCtx->width, pHandle->pVideoCtx->height, PICXEL_FORMAT,
			pHandle->sws_flags, NULL, NULL, NULL);
	}

	if (pHandle->audioStream > -1) {
		pHandle->pAudioCtx
				= pHandle->pFormatCtx->streams[pHandle->audioStream]->codec;

		pHandle->pAudioCodec = avcodec_find_decoder(pHandle->pAudioCtx->codec_id);
		if (pHandle->pAudioCodec == NULL) {
			*errorCode = ERROR_CAN_NOT_OPEN_AUDIOCODEC;
			goto error;
		}

		if (avcodec_open(pHandle->pAudioCtx, pHandle->pAudioCodec) < 0) {
			*errorCode = ERROR_CAN_NOT_OPEN_AUDIOCODEC;
			goto error;
		}
	}

	return pHandle;

	error:
	closeRyuMPEG(pHandle);
	return NULL;
}

extern void getVideoInfo(RyuMPEG_Handle *pHandle, VideoInfo *pVideoInfo) {
	if (pHandle->videoStream == -1) {
		pVideoInfo->codecID = 0;
		pVideoInfo->pixelFormat = 0;
		pVideoInfo->width = 0;
		pVideoInfo->height = 0;
		pVideoInfo->fps = 0;
		pVideoInfo->bitrate = 0;
		pVideoInfo->frameSize = 0;
		return;
	}

	pVideoInfo->codecID = pHandle->pVideoCtx->codec_id;
	pVideoInfo->pixelFormat = pHandle->pVideoCtx->pix_fmt;
	pVideoInfo->width = pHandle->pVideoCtx->width;
	pVideoInfo->height = pHandle->pVideoCtx->height;

	pVideoInfo->fps = (int) av_q2d(pHandle->pFormatCtx->streams[pHandle->videoStream]->r_frame_rate) * 1000;

	pVideoInfo->bitrate = pHandle->pVideoCtx->bit_rate;

	pVideoInfo->frameSize = pHandle->pVideoCtx->frame_size;
	if (pVideoInfo->frameSize == 0) pVideoInfo->frameSize = avio_size(pHandle->pFormatCtx->pb);
}

extern void getAudioInfo(RyuMPEG_Handle *pHandle, AudioInfo *pAudioInfo) {
	if (pHandle->audioStream == -1) {
		pAudioInfo->codecID = 0;
		pAudioInfo->sampleRate = 0;
		pAudioInfo->channels = 0;
		return;
	}

	pAudioInfo->codecID = pHandle->pAudioCtx->codec_id;
	pAudioInfo->sampleRate = pHandle->pAudioCtx->sample_rate;
	pAudioInfo->channels = pHandle->pAudioCtx->channels;
}

extern void getMediaInfo(RyuMPEG_Handle *pHandle, MediaInfo *pMediaInfo) {
	if (pHandle->videoStream > -1) {
	  avcodec_string(pMediaInfo->videoInfoString, sizeof(pMediaInfo->videoInfoString), pHandle->pVideoCtx, 0);
	}

	if (pHandle->audioStream > -1) {
	  avcodec_string(pMediaInfo->audioInfoString, sizeof(pMediaInfo->audioInfoString), pHandle->pAudioCtx, 0);
	}

	pMediaInfo->duration = pHandle->pFormatCtx->duration * 1000 / AV_TIME_BASE;

	getVideoInfo(pHandle, &pMediaInfo->videoInfo);
	getAudioInfo(pHandle, &pMediaInfo->audioInfo);
}

extern int readData(RyuMPEG_Handle *pHandle, void *pData, int *pDataSize, int *pPacketType, int *pPosition) {
	AVPacket packet;

	 av_init_packet(&packet);

	if ((pHandle->isEOF == false) && (av_read_frame(pHandle->pFormatCtx, &packet) >= 0)) {
		if (packet.stream_index == pHandle->audioStream) {
			*pPacketType = AUDIO_PACKET;
			pHandle->currentPosition = pHandle->pFormatCtx->streams[pHandle->audioStream]->cur_dts *
					av_q2d(pHandle->pFormatCtx->streams[pHandle->audioStream]->time_base)*1000;
		} else if (packet.stream_index == pHandle->videoStream) {
			*pPacketType = VIDEO_PACKET;
		}

		*pDataSize = packet.size;

		if (packet.size <= READ_BUFFER_SIZE) {
			memcpy(pData, packet.data, packet.size);
		}

		*pPosition = pHandle->currentPosition;

		av_free_packet(&packet);

		return true;
	} else {
		pHandle->isEOF = true;

		*pDataSize = 0;
		*pPacketType = UNKNOWN_PACKET;
		*pPosition = 0;

		return false;
	}
}

extern char decodeVideoData(RyuMPEG_Handle *pHandle, void *pData, int size, char *pBuffer)
{
	AVPacket packet;
	packet.data = pData;
	packet.size = size;

	avcodec_decode_video2(
		pHandle->pVideoCtx,
		pHandle->pFrame,
		&pHandle->isFrameFinished,
		&packet
	);

	if (pHandle->isFrameFinished) {
		sws_scale(
			pHandle->img_convert_ctx,
			pHandle->pFrame->data,
			pHandle->pFrame->linesize,
			0,
			pHandle->pVideoCtx->height,
			pHandle->pFrameRGB->data,
			pHandle->pFrameRGB->linesize
		);

		if (pBuffer != NULL) {
			memcpy(
				pBuffer,
				pHandle->pFrameRGB->data[0],
				pHandle->pVideoCtx->width * pHandle->pVideoCtx->height * PICXEL_SIZE
			);
		}
	}

	return pHandle->isFrameFinished;
}

extern char decodeAudioPacket(RyuMPEG_Handle *pHandle, void *pDataIn, int sizeIn, char *pDataOut, int *pSizeOut)
{
	int data_size = 0;

	AVPacket packet;
	packet.data = pDataIn;
	packet.size = sizeIn;

	int len = 0;
	int out_size;
	while (packet.size > 0) {
		out_size = sizeof(pHandle->audio_buf);
		len = avcodec_decode_audio3(pHandle->pAudioCtx, (int16_t *) pHandle->audio_buf,
				&out_size, &packet);
		if (len < 0) {
			break;
		}

		if (out_size > 0) {
			data_size += out_size;

			memcpy(pDataOut, pHandle->audio_buf, out_size);
			pDataOut += out_size;
		}

		packet.size -= len;
		packet.data += len;
	}

	*pSizeOut = data_size;

	return (data_size > 0);
}
