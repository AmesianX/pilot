// VideoZip.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "stdlib.h"
#include "VideoZip.h"
#include "yuvTools.h"

#define interfaceEnc (vpx_codec_vp8_cx())
#define interfaceDec (vpx_codec_vp8_dx())

const vpx_img_fmt _PixelFormat = VPX_IMG_FMT_I420;
const int _PixelSize = 4;

extern "C" __declspec(dllexport) void *openEncoder(int *errorCode, int width, int height, int bitRate, int fps, int gop) {
	*errorCode = 0;

	VideoZipHandle *pHandle = (VideoZipHandle *) malloc(sizeof(VideoZipHandle));

	if (!vpx_img_alloc(&pHandle->img, _PixelFormat , width, height, 1)) {
		*errorCode = _Error_Allocate_Image;
		goto error;
	}

	if (vpx_codec_enc_config_default(interfaceEnc, &pHandle->cfgEnc, 0)) {
		*errorCode = _Error_Getting_Config;
		goto error;
	}

	pHandle->cfgEnc.g_w = width;
	pHandle->cfgEnc.g_h = height;
	pHandle->cfgEnc.kf_max_dist = gop;

	if (vpx_codec_enc_init(&pHandle->codec, interfaceEnc, &pHandle->cfgEnc, 0)) {
		*errorCode = _Error_Init_VideoCodec;
		goto error;
	}

	return pHandle;

	error:
	vpx_img_free(&pHandle->img);
	vpx_codec_destroy(&pHandle->codec);
	free(pHandle);
	return NULL;
}

extern "C" __declspec(dllexport) void closeEncoder(VideoZipHandle *pHandle) {
	vpx_img_free(&pHandle->img);
	vpx_codec_destroy(&pHandle->codec);
	free(pHandle);
}

extern "C" __declspec(dllexport) int encodeBitmap(VideoZipHandle *pHandle, void *pBitmap, void *pBuffer, int sizeOfBuffer) {
	int packet_size = 0;
	int frame_cnt = 0;
	int flags = 0;

	RGBtoYUV420((unsigned char*) pBitmap, pHandle->img.planes[0], pHandle->cfgEnc.g_w, pHandle->cfgEnc.g_h, _PixelSize);
	if (vpx_codec_encode(&pHandle->codec, &pHandle->img, frame_cnt, 1, flags, VPX_DL_REALTIME)) {
		return packet_size;
	}

	const vpx_codec_cx_pkt_t *pPacket;
	vpx_codec_iter_t iter = NULL;
	unsigned char *pFrame = (unsigned char *) pBuffer;
	int *pFrameSize;

	while ( (pPacket = (vpx_codec_get_cx_data(&pHandle->codec, &iter))) ) {
		// sizeOfBuffer를 넘어가는 프레임은 버린다.
		if ((packet_size + sizeof(int) + pPacket->data.frame.sz) >= sizeOfBuffer) return packet_size;

		switch (pPacket->kind) {
			case VPX_CODEC_CX_FRAME_PKT:  
				pFrameSize = (int *) pFrame;
				*pFrameSize = pPacket->data.frame.sz;
				pFrame = pFrame + sizeof(int);

				memcpy(pFrame, pPacket->data.frame.buf, pPacket->data.frame.sz); 
				pFrame = pFrame + pPacket->data.frame.sz;

				packet_size = packet_size + sizeof(int) + pPacket->data.frame.sz;
				break;                                               

			default: break;
		}
	}

	return packet_size;
}

extern "C" __declspec(dllexport) void *openDecoder(int *errorCode, int width, int height)
{
	*errorCode = 0;

	VideoZipHandle *pHandle = (VideoZipHandle *) malloc(sizeof(VideoZipHandle));

	pHandle->cfgDec.w = width;
	pHandle->cfgDec.h = height;
	pHandle->cfgDec.threads = 16;

	int flags = 0;

	if (vpx_codec_dec_init(&pHandle->codec, interfaceDec, &pHandle->cfgDec, flags)) {
		*errorCode = _Error_Init_VideoCodec;
		goto error;
	}

	return pHandle;

	error:
	vpx_codec_destroy(&pHandle->codec);
	free(pHandle);
	return NULL;
}

extern "C" __declspec(dllexport) void closeDecoder(VideoZipHandle *pHandle)
{
	vpx_codec_destroy(&pHandle->codec);
	free(pHandle);
}

extern "C" __declspec(dllexport) void initDecoder(VideoZipHandle *pHandle) {
	int flags = 0;
	vpx_codec_dec_init(&pHandle->codec, interfaceDec, &pHandle->cfgDec, flags);
}

extern "C" __declspec(dllexport) char decodeBitmap(VideoZipHandle *pHandle, void *pBitmap, void *pBuffer, int sizeOfBuffer)
{
	char result = 0;

	unsigned char *pFrame = (unsigned char *) pBuffer;
	int *pFrameSize;
	int frameSize = 0;
	int count = 0;
	vpx_image_t *img;
	vpx_codec_iter_t iter;

	while (count < sizeOfBuffer) {
		pFrameSize = (int *) pFrame;
		frameSize = *pFrameSize;
		pFrame = pFrame + sizeof(int);

		if (vpx_codec_decode(&pHandle->codec, (unsigned char*) pFrame, frameSize, NULL, 0)) {
			return result;
		}
		pFrame = pFrame + frameSize;

		count = count + sizeof(int) + frameSize;

		iter = NULL;
		while((img = vpx_codec_get_frame(&pHandle->codec, &iter))) {
			I420ToARGB(
				(unsigned char *) img->planes[0], img->stride[0],
				(unsigned char *) img->planes[1], img->stride[1],
				(unsigned char *) img->planes[2], img->stride[2],
				(unsigned char *) pBitmap, 
				img->d_w * _PixelSize,
				img->d_w, img->d_h
			);

			result = 1;
		}
	}

	return result;
}
