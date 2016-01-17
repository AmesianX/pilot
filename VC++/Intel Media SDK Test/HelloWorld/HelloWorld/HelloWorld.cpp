// HelloWorld.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <string.h>
#include <stdlib.h>
#include "mfxvideo.h"

#define MSDK_ALIGN16(value)                      (((value + 15) >> 4) << 4) // round up to a multiple of 16
#define MSDK_ALIGN32(value)                      (((value + 31) >> 5) << 5) // round up to a multiple of 32


int GetFreeSurfaceIndex(mfxFrameSurface1** pSurfacesPool, mfxU16 nPoolSize)
{
	mfxU16 i;

	if (pSurfacesPool)
	for (i = 0; i < nPoolSize; i++)
	if (0 == pSurfacesPool[i]->Data.Locked)
		return i;
	return MFX_ERR_NOT_FOUND;
}

int _tmain(int argc, _TCHAR* argv[])
{
	int iWidth = 1280;
	int iHeight = 720;

	mfxStatus sts = MFX_ERR_NONE;
	mfxIMPL impl = MFX_IMPL_AUTO_ANY;
	mfxSession session = NULL;

	mfxVersion ver;
	ver.Major = 1;
	ver.Minor = 0;

	sts = MFXInit(impl, &ver, &session);
	printf("init: sts=%d session=%lx\n", sts, (unsigned long)session);

	MFXQueryIMPL(session, &impl);
	printf("implementation=%d\n", impl);

	mfxVersion verTemp;
	MFXQueryVersion(session, &verTemp);
	printf("Version on Machine:%u.%u\n", verTemp.Major, verTemp.Minor);

	mfxVideoParam mfxEncParams;
	memset(&mfxEncParams, 0, sizeof(mfxEncParams));
	mfxEncParams.mfx.CodecId = MFX_CODEC_AVC;
	mfxEncParams.mfx.TargetUsage = MFX_TARGETUSAGE_BALANCED;
	mfxEncParams.mfx.TargetKbps = 128 * 8;  // √ ¥Á 128 KB
	mfxEncParams.mfx.RateControlMethod = MFX_RATECONTROL_VBR;
	mfxEncParams.mfx.FrameInfo.FrameRateExtN = 30;
	mfxEncParams.mfx.FrameInfo.FrameRateExtD = 1;
	mfxEncParams.mfx.FrameInfo.FourCC = MFX_FOURCC_NV12;
	mfxEncParams.mfx.FrameInfo.ChromaFormat = MFX_CHROMAFORMAT_YUV420;
	mfxEncParams.mfx.FrameInfo.PicStruct = MFX_PICSTRUCT_PROGRESSIVE;
	mfxEncParams.mfx.FrameInfo.CropX = 0;
	mfxEncParams.mfx.FrameInfo.CropY = 0;
	mfxEncParams.mfx.FrameInfo.CropW = iWidth;
	mfxEncParams.mfx.FrameInfo.CropH = iHeight;

	// Width must be a multiple of 16
	// Height must be a multiple of 16 in case of frame picture and a multiple of 32 in case of field picture
	mfxEncParams.mfx.FrameInfo.Width = MSDK_ALIGN16(iWidth);
	mfxEncParams.mfx.FrameInfo.Height = 
		(MFX_PICSTRUCT_PROGRESSIVE == mfxEncParams.mfx.FrameInfo.PicStruct) ? MSDK_ALIGN16(iWidth) : MSDK_ALIGN32(iHeight);

	mfxEncParams.IOPattern = MFX_IOPATTERN_IN_SYSTEM_MEMORY;

	sts = MFXVideoENCODE_Query(session, &mfxEncParams, &mfxEncParams);
	printf("MFXVideoENCODE_Query:sts=%d\n", sts);

	mfxFrameAllocRequest EncRequest;
	memset(&EncRequest, 0, sizeof(EncRequest));
	sts = MFXVideoENCODE_QueryIOSurf(session, &mfxEncParams, &EncRequest);
	printf("MFXVideoENCODE_QueryIOSurf:sts=%d\n", sts);

	mfxU16 nEncSurfNum = EncRequest.NumFrameSuggested;
	printf("Number of frames=%u\n", EncRequest.NumFrameSuggested);

	mfxU16 width  = (mfxU16) MSDK_ALIGN32(EncRequest.Info.Width);
	mfxU16 height = (mfxU16) MSDK_ALIGN32(EncRequest.Info.Height);

	printf("width: %d, height: %d \n", width, height);

	mfxU8  bitsPerPixel = 12;  // NV12 format is a 12 bits per pixel format
	mfxU32 surfaceSize = width * height * bitsPerPixel / 8;
	mfxU8 *surfaceBuffers = (mfxU8 *) malloc(surfaceSize * nEncSurfNum * sizeof(mfxU8));
	mfxFrameSurface1 **pEncSurfaces = (mfxFrameSurface1 **) malloc(sizeof(mfxFrameSurface1*)* nEncSurfNum);

	for (int i = 0; i < nEncSurfNum; i++)
	{
		pEncSurfaces[i] = (mfxFrameSurface1 *) malloc(sizeof(mfxFrameSurface1));
		memset(pEncSurfaces[i], 0, sizeof(mfxFrameSurface1));
		memcpy(&(pEncSurfaces[i]->Info), &(mfxEncParams.mfx.FrameInfo), sizeof(mfxFrameInfo));
		pEncSurfaces[i]->Data.Y = &surfaceBuffers[surfaceSize * i];
		pEncSurfaces[i]->Data.U = pEncSurfaces[i]->Data.Y + width * height;
		pEncSurfaces[i]->Data.V = pEncSurfaces[i]->Data.U + 1;
		pEncSurfaces[i]->Data.Pitch = width;

		// In case simulating direct access to frames we initialize the allocated surfaces with default pattern
		// - For true benchmark comparisons to async workloads all surfaces must have the same data
		memset(pEncSurfaces[i]->Data.Y, 100, width * height);  // Y plane
		memset(pEncSurfaces[i]->Data.U, 50, (width * height) / 2);  // UV plane
	}

	sts = MFXVideoENCODE_Init(session, &mfxEncParams);
	printf("MFXVideoENCODE_Init:sts=%d\n", sts);

	mfxVideoParam par;
	memset(&par, 0, sizeof(par));
	sts = MFXVideoENCODE_GetVideoParam(session, &par);
	printf("MFXVideoENCODE_GetVideoParam:sts=%d\n", sts);

	// Prepare Media SDK bit stream buffer
	mfxBitstream mfxBS;
	memset(&mfxBS, 0, sizeof(mfxBS));
	mfxBS.MaxLength = par.mfx.BufferSizeInKB * 1024;
	mfxBS.Data = (mfxU8 *) malloc(sizeof(mfxU8)* mfxBS.MaxLength);

	int nEncSurfIdx = 0;
	mfxSyncPoint syncp;
	mfxU32 nFrame = 0;

	while (MFX_ERR_NONE <= sts || MFX_ERR_MORE_DATA == sts) {
		nEncSurfIdx = GetFreeSurfaceIndex(pEncSurfaces, nEncSurfNum); // Find free frame surface

		// TODO: 
		//sts = LoadRawFrame(pEncSurfaces[nEncSurfIdx], fSource, true);

		for (;;) {
			sts = MFXVideoENCODE_EncodeFrameAsync(session, NULL, pEncSurfaces[nEncSurfIdx], &mfxBS, &syncp);

			if (MFX_ERR_NONE < sts && !syncp) // Repeat the call if warning and no output
			{
				if (MFX_WRN_DEVICE_BUSY == sts)
					Sleep(1); // Wait if device is busy, then repeat the same call            
			}
			else if (MFX_ERR_NONE < sts && syncp)
			{
				sts = MFX_ERR_NONE; // Ignore warnings if output is available                                    
				break;
			}
			else if (MFX_ERR_NOT_ENOUGH_BUFFER == sts)
			{
				// Allocate more bitstream buffer memory here if needed...
				break;
			}
			else
				break;
		}

		if (MFX_ERR_NONE == sts){
			sts = MFXVideoCORE_SyncOperation(session, syncp, 60000); // Synchronize. Wait until encoded frame is ready

			if (MFX_ERR_NONE != sts)
				printf("Error - MFX_ERR_NONE != sts, %d\n", sts);

			// TODO:
			//sts = WriteBitStreamFrame(&mfxBS);

			++nFrame;

			int iResult = mfxBS.DataLength;

			if (iResult <= 0) {
			//if (mfxBS.DataLength <= 0) {
				printf("Frame number: %d, mfxBS.DataLength=%d\n", nFrame, iResult);
			}
			else {
				printf("Frame number: %d, mfxBS.DataLength=%d\r", nFrame, iResult);
			}

			mfxBS.DataLength = 0;
		}
		else {
			printf("....... nFrame: %d, %d\n", nFrame, sts);
		}
	}

	MFXVideoENCODE_Close(session);
}

