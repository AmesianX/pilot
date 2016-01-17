// IntelEncoder.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include <WinBase.h>
#include <string.h>
#include <stdlib.h>
#include "IntelEncoder.h"


int GetFreeSurfaceIndex(mfxFrameSurface1** pSurfacesPool, mfxU16 nPoolSize)
{
	mfxU16 i;

	if (pSurfacesPool)
	for (i = 0; i < nPoolSize; i++)
	if (0 == pSurfacesPool[i]->Data.Locked)
		return i;
	return MFX_ERR_NOT_FOUND;
}

extern "C" __declspec(dllexport) void *openEncoder(int *pErrorCode, int width, int height, int bitRate, int gop)
{
	*pErrorCode = 0;

	IntelEncoderHandle *pHandle = (IntelEncoderHandle *) malloc(sizeof(IntelEncoderHandle));

	mfxStatus sts = MFX_ERR_NONE;

	mfxIMPL impl = MFX_IMPL_AUTO_ANY;

	mfxVersion ver;
	ver.Major = 1;
	ver.Minor = 0;

	sts = MFXInit(impl, &ver, &pHandle->session);
	if (MFX_ERR_NONE != sts) {
		// TODO:
		*pErrorCode = -1;
	}

	MFXQueryIMPL(pHandle->session, &impl);

	mfxVersion verTemp;
	MFXQueryVersion(pHandle->session, &verTemp);

	mfxVideoParam mfxEncParams;
	memset(&mfxEncParams, 0, sizeof(mfxEncParams));
	mfxEncParams.mfx.CodecId = MFX_CODEC_AVC;
	//  mfxEncParams.mfx.CodecProfile = MFX_PROFILE_AVC_CONSTRAINED_BASELINE;
	mfxEncParams.mfx.TargetUsage = MFX_TARGETUSAGE_BALANCED;

	if (0 == bitRate) bitRate = 128 * 8;
	mfxEncParams.mfx.TargetKbps = bitRate; 

	mfxEncParams.mfx.RateControlMethod = MFX_RATECONTROL_VBR;
	mfxEncParams.mfx.FrameInfo.FrameRateExtN = 30;
	mfxEncParams.mfx.FrameInfo.FrameRateExtD = 1;
	mfxEncParams.mfx.FrameInfo.FourCC = MFX_FOURCC_NV12;
	mfxEncParams.mfx.FrameInfo.ChromaFormat = MFX_CHROMAFORMAT_YUV420;
	mfxEncParams.mfx.FrameInfo.PicStruct = MFX_PICSTRUCT_PROGRESSIVE;
	mfxEncParams.mfx.FrameInfo.CropX = 0;
	mfxEncParams.mfx.FrameInfo.CropY = 0;
	mfxEncParams.mfx.FrameInfo.CropW = width;
	mfxEncParams.mfx.FrameInfo.CropH = height;

	// Width must be a multiple of 16
	// Height must be a multiple of 16 in case of frame picture and a multiple of 32 in case of field picture
	mfxEncParams.mfx.FrameInfo.Width = MSDK_ALIGN16(width);
	mfxEncParams.mfx.FrameInfo.Height =
		(MFX_PICSTRUCT_PROGRESSIVE == mfxEncParams.mfx.FrameInfo.PicStruct) ? MSDK_ALIGN16(width) : MSDK_ALIGN32(height);

	mfxEncParams.IOPattern = MFX_IOPATTERN_IN_SYSTEM_MEMORY;

	sts = MFXVideoENCODE_Query(pHandle->session, &mfxEncParams, &mfxEncParams);
	if (MFX_ERR_NONE != sts) {
		// TODO:
		*pErrorCode = -2;
	}

	mfxFrameAllocRequest EncRequest;
	memset(&EncRequest, 0, sizeof(EncRequest));
	sts = MFXVideoENCODE_QueryIOSurf(pHandle->session, &mfxEncParams, &EncRequest);
	if (MFX_ERR_NONE != sts) {
		// TODO:
		*pErrorCode = -3;
	}

	pHandle->nEncSurfNum = EncRequest.NumFrameSuggested;

	mfxU16 w = (mfxU16)MSDK_ALIGN32(EncRequest.Info.Width);
	mfxU16 h = (mfxU16)MSDK_ALIGN32(EncRequest.Info.Height);
	mfxU8  bitsPerPixel = 12;  // NV12 format is a 12 bits per pixel format
	mfxU32 surfaceSize = w * h * bitsPerPixel / 8;
	pHandle->pSurfaceBuffers = (mfxU8 *)malloc(surfaceSize * pHandle->nEncSurfNum * sizeof(mfxU8));
	pHandle->ppEncSurfaces = (mfxFrameSurface1 **)malloc(sizeof(mfxFrameSurface1*)* pHandle->nEncSurfNum);

	for (int i = 0; i < pHandle->nEncSurfNum; i++)
	{
		pHandle->ppEncSurfaces[i] = (mfxFrameSurface1 *)malloc(sizeof(mfxFrameSurface1));
		memset(pHandle->ppEncSurfaces[i], 0, sizeof(mfxFrameSurface1));
		memcpy(&(pHandle->ppEncSurfaces[i]->Info), &(mfxEncParams.mfx.FrameInfo), sizeof(mfxFrameInfo));
		pHandle->ppEncSurfaces[i]->Data.Y = &pHandle->pSurfaceBuffers[surfaceSize * i];
		pHandle->ppEncSurfaces[i]->Data.U = pHandle->ppEncSurfaces[i]->Data.Y + w * h;
		pHandle->ppEncSurfaces[i]->Data.V = pHandle->ppEncSurfaces[i]->Data.U + 1;
		pHandle->ppEncSurfaces[i]->Data.Pitch = w;

		// In case simulating direct access to frames we initialize the allocated surfaces with default pattern
		// - For true benchmark comparisons to async workloads all surfaces must have the same data
		memset(pHandle->ppEncSurfaces[i]->Data.Y, 100, w * h);  // Y plane
		memset(pHandle->ppEncSurfaces[i]->Data.U, 50, (w * h) / 2);  // UV plane
	}

	sts = MFXVideoENCODE_Init(pHandle->session, &mfxEncParams);
	MSDK_IGNORE_MFX_STS(sts, MFX_WRN_PARTIAL_ACCELERATION);
	if (MFX_ERR_NONE != sts) {
		// TODO:
		*pErrorCode = -4;
	}

	mfxVideoParam par;
	memset(&par, 0, sizeof(par));
	sts = MFXVideoENCODE_GetVideoParam(pHandle->session, &par);
	if (MFX_ERR_NONE != sts) {
		// TODO:
		*pErrorCode = -5;
	}

	memset(&pHandle->mfxBS, 0, sizeof(pHandle->mfxBS));
	pHandle->mfxBS.MaxLength = par.mfx.BufferSizeInKB * 1024;
	pHandle->mfxBS.Data = (mfxU8 *)malloc(sizeof(mfxU8)* pHandle->mfxBS.MaxLength);

	return pHandle;
}

extern "C" __declspec(dllexport) void closeEncoder(IntelEncoderHandle *pHandle)
{
	MFXVideoENCODE_Close(pHandle->session);

	for (int i = 0; i < pHandle->nEncSurfNum; i++) free(pHandle->ppEncSurfaces);

	free(pHandle->pSurfaceBuffers);
	free(pHandle->ppEncSurfaces);
	free(pHandle);
}

extern "C" __declspec(dllexport) int encodeBitmap(IntelEncoderHandle *pHandle, void *pBitmap, void **ppBuffer)
{
	mfxStatus sts = MFX_ERR_NONE;
	mfxSyncPoint syncp;
	int nEncSurfIdx = 0;

	// Find free frame surface
	nEncSurfIdx = GetFreeSurfaceIndex(pHandle->ppEncSurfaces,pHandle->nEncSurfNum);

	// TODO: 
	//sts = LoadRawFrame(pEncSurfaces[nEncSurfIdx], fSource, true);

	for (;;) {
		sts = MFXVideoENCODE_EncodeFrameAsync(pHandle->session, NULL, pHandle->ppEncSurfaces[nEncSurfIdx], &pHandle->mfxBS, &syncp);

		if (MFX_ERR_NONE < sts && !syncp) {  // Repeat the call if warning and no output
			if (MFX_WRN_DEVICE_BUSY == sts) Sleep(5);
		} else if (MFX_ERR_NONE < sts && syncp) {  // Ignore warnings if output is available
			sts = MFX_ERR_NONE; 
			break;
		} else if (MFX_ERR_NOT_ENOUGH_BUFFER == sts) {
			return 0;
		}
		else break;
	}

	if (MFX_ERR_NONE == sts){
		sts = MFXVideoCORE_SyncOperation(pHandle->session, syncp, 60000); // Synchronize. Wait until encoded frame is ready
		if (MFX_ERR_NONE != sts) return 0;

		*ppBuffer = pHandle->mfxBS.Data + pHandle->mfxBS.DataOffset;

		int iResult = (int) pHandle->mfxBS.DataLength;
		pHandle->mfxBS.DataLength = 0;

		return iResult;
	}
}

extern "C" __declspec(dllexport) void *openDecoder(int *pErrorCode, int width, int height)
{
	return NULL;
}

extern "C" __declspec(dllexport) void closeDecoder(IntelEncoderHandle *pHandle)
{
}

extern "C" __declspec(dllexport) void initDecoder(IntelEncoderHandle *pHandle) {
}

extern "C" __declspec(dllexport) char decodeBitmap(IntelEncoderHandle *pHandle, void *pBitmap, void *pBuffer, int sizeOfBuffer)
{
	return 0;
}
