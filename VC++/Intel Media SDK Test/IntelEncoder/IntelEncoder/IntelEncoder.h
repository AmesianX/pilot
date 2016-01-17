#ifndef _IntelEncoder_H_
#define	_IntelEncoder_H_

#include "mfxvideo.h"

#define MSDK_ALIGN16(value)                      (((value + 15) >> 4) << 4) // round up to a multiple of 16
#define MSDK_ALIGN32(value)                      (((value + 31) >> 5) << 5) // round up to a multiple of 32
#define MSDK_IGNORE_MFX_STS(P, X)                {if ((X) == (P)) {P = MFX_ERR_NONE;}}

#define _Error_General -1;

typedef struct _IntelEncoderHandle {
	mfxSession session = NULL;
	mfxU16 nEncSurfNum;
	mfxU8 *pSurfaceBuffers;
	mfxFrameSurface1 **ppEncSurfaces;
	mfxBitstream mfxBS;
} IntelEncoderHandle;

extern "C" __declspec(dllexport) void *openEncoder(int *pErrorCode, int width, int height, int bitRate, int gop);
extern "C" __declspec(dllexport) void closeEncoder(IntelEncoderHandle *pHandle);
extern "C" __declspec(dllexport) int encodeBitmap(IntelEncoderHandle *pHandle, void *pBitmap, void **ppBuffer);

extern "C" __declspec(dllexport) void *openDecoder(int *pErrorCode, int width, int height);
extern "C" __declspec(dllexport) void closeDecoder(IntelEncoderHandle *pHandle);
extern "C" __declspec(dllexport) char decodeBitmap(IntelEncoderHandle *pHandle, void *pBitmap, void *pBuffer, int sizeOfBuffer);

#endif /* _IntelEncoder_H_ */
