#ifndef _VideoZip_H_
#define	_VideoZip_H_

#include "vpx/vpx_codec.h"
#include "vpx/vpx_encoder.h"
#include "vpx/vpx_decoder.h"
#include "vpx/vp8cx.h"
#include "vpx/vp8dx.h"

#define _Error_General -1;
#define _Error_Allocate_Image -2
#define _Error_Getting_Config -3
#define _Error_Init_VideoCodec -4;

typedef struct _VideoZipHandle {
    struct vpx_codec_enc_cfg cfgEnc;
	struct vpx_codec_dec_cfg cfgDec;
    vpx_codec_ctx_t codec;
	vpx_image_t	img;
} VideoZipHandle;

extern "C" __declspec(dllexport) void *openEncoder(int *errorCode, int width, int height, int bitRate, int fps, int gop);
extern "C" __declspec(dllexport) void closeEncoder(VideoZipHandle *pHandle);
extern "C" __declspec(dllexport) int encodeBitmap(VideoZipHandle *pHandle, void *pBitmap, void *pBuffer, int sizeOfBuffer);

extern "C" __declspec(dllexport) void *openDecoder(int *errorCode, int width, int height);
extern "C" __declspec(dllexport) void closeDecoder(VideoZipHandle *pHandle);
extern "C" __declspec(dllexport) void initDecoder(VideoZipHandle *pHandle);
extern "C" __declspec(dllexport) char decodeBitmap(VideoZipHandle *pHandle, void *pBitmap, void *pBuffer, int sizeOfBuffer);

#endif /* _VideoZip_H_ */
