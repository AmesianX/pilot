//
//               INTEL CORPORATION PROPRIETARY INFORMATION
//  This software is supplied under the terms of a license agreement or
//  nondisclosure agreement with Intel Corporation and may not be copied
//  or disclosed except in accordance with the terms of that agreement.
//        Copyright (c) 2005-2013 Intel Corporation. All Rights Reserved.
//

#define ENABLE_OUTPUT  // Disabling this flag removes all YUV file writing
#define ENABLE_BENCHMARK

#include "common_utils.h"

// Get free raw frame surface
int GetFreeSurfaceIndex(mfxFrameSurface1** pSurfacesPool, mfxU16 nPoolSize)
{    
    if (pSurfacesPool)
        for (mfxU16 i = 0; i < nPoolSize; i++)
            if (0 == pSurfacesPool[i]->Data.Locked)
                return i;
    return MFX_ERR_NOT_FOUND;
}


int main()
{
    mfxStatus sts = MFX_ERR_NONE;

    // =====================================================================
    // Intel Media SDK decode pipeline setup
    // - In this example we are decoding an AVC (H.264) stream
    // - For simplistic memory management, system memory surfaces are used to store the decoded frames
    //   (Note that when using HW acceleration D3D surfaces are prefered, for better performance)
    //

	   // Create output YUV file
    FILE* fSink;
    fopen_s(&fSink, "dectest.yuv", "wb");
    MSDK_CHECK_POINTER(fSink, MFX_ERR_NULL_PTR);

    // Open input H.264 elementary stream (ES) file
    FILE* fSource;
    fopen_s(&fSource, "mpeg4.h264", "rb");
    MSDK_CHECK_POINTER(fSource, MFX_ERR_NULL_PTR);
	mfxPayload dec_payload;
    mfxU64 ts;
    dec_payload.Data = new mfxU8[1024];
    dec_payload.BufSize =1024;
 

    // Initialize Intel Media SDK session
    // - MFX_IMPL_AUTO_ANY selects HW accelaration if available (on any adapter)
    // - Version 1.0 is selected for greatest backwards compatibility.
    //   If more recent API features are needed, change the version accordingly
    mfxIMPL impl = MFX_IMPL_AUTO_ANY;
    mfxVersion ver = {8, 1};
    MFXVideoSession mfxSession;
    sts = mfxSession.Init(impl, &ver);
    MSDK_CHECK_RESULT(sts, MFX_ERR_NONE, sts);

    // Create Media SDK decoder
    MFXVideoDECODE mfxDEC(mfxSession);

    // Set required video parameters for decode
    mfxVideoParam mfxVideoParams;
    memset(&mfxVideoParams, 0, sizeof(mfxVideoParams));
    mfxVideoParams.mfx.CodecId = MFX_CODEC_AVC;
    mfxVideoParams.IOPattern = MFX_IOPATTERN_OUT_SYSTEM_MEMORY;
    
    // Prepare Media SDK bit stream buffer
    // - Arbitrary buffer size for this example
    mfxBitstream mfxBS; 
    memset(&mfxBS, 0, sizeof(mfxBS));
    mfxBS.MaxLength = 1024 * 1024;
    mfxBS.Data = new mfxU8[mfxBS.MaxLength];
    MSDK_CHECK_POINTER(mfxBS.Data, MFX_ERR_MEMORY_ALLOC);

    // Read a chunk of data from stream file into bit stream buffer
    // - Parse bit stream, searching for header and fill video parameters structure
    // - Abort if bit stream header is not found in the first bit stream buffer chunk
    sts = ReadBitStreamData(&mfxBS, fSource);
    MSDK_CHECK_RESULT(sts, MFX_ERR_NONE, sts);
    
    sts = mfxDEC.DecodeHeader(&mfxBS, &mfxVideoParams);
    MSDK_IGNORE_MFX_STS(sts, MFX_WRN_PARTIAL_ACCELERATION);
    MSDK_CHECK_RESULT(sts, MFX_ERR_NONE, sts);

    // Validate video decode parameters (optional)
    // sts = mfxDEC.Query(&mfxVideoParams, &mfxVideoParams);  

    // Query number of required surfaces for decoder
    mfxFrameAllocRequest Request;
    memset(&Request, 0, sizeof(Request));
    sts = mfxDEC.QueryIOSurf(&mfxVideoParams, &Request);
    MSDK_IGNORE_MFX_STS(sts, MFX_WRN_PARTIAL_ACCELERATION);
    MSDK_CHECK_RESULT(sts, MFX_ERR_NONE, sts);

    mfxU16 numSurfaces = Request.NumFrameSuggested;

	printf("Surfaces :%d\n",numSurfaces);
    // Allocate surfaces for decoder
    // - Width and height of buffer must be aligned, a multiple of 32 
    // - Frame surface array keeps pointers all surface planes and general frame info
    mfxU16 width = (mfxU16)MSDK_ALIGN32(Request.Info.Width);
    mfxU16 height = (mfxU16)MSDK_ALIGN32(Request.Info.Height);
    mfxU8  bitsPerPixel = 12;  // NV12 format is a 12 bits per pixel format
    mfxU32 surfaceSize = width * height * bitsPerPixel / 8;
    mfxU8* surfaceBuffers = (mfxU8 *)new mfxU8[surfaceSize * numSurfaces];
    
    mfxFrameSurface1** pmfxSurfaces = new mfxFrameSurface1*[numSurfaces];
    MSDK_CHECK_POINTER(pmfxSurfaces, MFX_ERR_MEMORY_ALLOC);       
    for (int i = 0; i < numSurfaces; i++)
    {       
        pmfxSurfaces[i] = new mfxFrameSurface1;
        memset(pmfxSurfaces[i], 0, sizeof(mfxFrameSurface1));
        memcpy(&(pmfxSurfaces[i]->Info), &(mfxVideoParams.mfx.FrameInfo), sizeof(mfxFrameInfo));
        pmfxSurfaces[i]->Data.Y = &surfaceBuffers[surfaceSize * i];
        pmfxSurfaces[i]->Data.U = pmfxSurfaces[i]->Data.Y + width * height;
        pmfxSurfaces[i]->Data.V = pmfxSurfaces[i]->Data.U + 1;
        pmfxSurfaces[i]->Data.Pitch = width;
    }  

    // Initialize the Media SDK decoder
    sts = mfxDEC.Init(&mfxVideoParams);
    MSDK_IGNORE_MFX_STS(sts, MFX_WRN_PARTIAL_ACCELERATION);
    MSDK_CHECK_RESULT(sts, MFX_ERR_NONE, sts);



			 dec_payload.NumBit = 100;
            while(dec_payload.NumBit !=0){
            mfxStatus mfx_param_flag = mfxDEC.GetPayload(&ts,&dec_payload);
            if((mfx_param_flag == MFX_ERR_NONE) && (dec_payload.Type == 4))
                    printf("");
            dec_payload.Type =0;
            } 


    // ===============================================================
    // Start decoding the frames from the stream
    //

#ifdef ENABLE_BENCHMARK
    LARGE_INTEGER tStart, tEnd;
    QueryPerformanceFrequency(&tStart);
    double freq = (double)tStart.QuadPart;
    QueryPerformanceCounter(&tStart);
#endif

    mfxSyncPoint syncp;
    mfxFrameSurface1* pmfxOutSurface = NULL;
    int nIndex = 0;  
    mfxU32 nFrame = 0;

    //
    // Stage 1: Main decoding loop
    //
    while ((MFX_ERR_NONE <= sts || MFX_ERR_MORE_DATA == sts || MFX_ERR_MORE_SURFACE == sts ))// && (nFrame <=200))          
    {
        if (MFX_WRN_DEVICE_BUSY == sts)
            Sleep(1); // Wait if device is busy, then repeat the same call to DecodeFrameAsync

        if (MFX_ERR_MORE_DATA == sts)
        {
            sts = ReadBitStreamData(&mfxBS, fSource); // Read more data into input bit stream
            MSDK_BREAK_ON_ERROR(sts);            
        }

        if (MFX_ERR_MORE_SURFACE == sts || MFX_ERR_NONE == sts || MFX_WRN_DEVICE_BUSY == sts || MFX_WRN_IN_EXECUTION == sts)
        {
            nIndex = GetFreeSurfaceIndex(pmfxSurfaces, numSurfaces); // Find free frame surface 
            MSDK_CHECK_ERROR(MFX_ERR_NOT_FOUND, nIndex, MFX_ERR_MEMORY_ALLOC);
			//printf("%d:%d\n",nFrame,nIndex);
        }
        
        // Decode a frame asychronously (returns immediately)
        //  - If input bitstream contains multiple frames DecodeFrameAsync will start decoding multiple frames, and remove them from bitstream
        sts = mfxDEC.DecodeFrameAsync(&mfxBS, pmfxSurfaces[nIndex], &pmfxOutSurface, &syncp);
		
		    dec_payload.NumBit = 100;
            while(dec_payload.NumBit !=0){
            mfxStatus mfx_param_flag = mfxDEC.GetPayload(&ts,&dec_payload);
            if((mfx_param_flag == MFX_ERR_NONE) && (dec_payload.Type == 4))
                    printf("");
            dec_payload.Type =0;
            }       


		//printf("%d:%d\n",nFrame,sts);

        // Ignore warnings if output is available, 
        // if no output and no action required just repeat the DecodeFrameAsync call
        if (MFX_ERR_NONE < sts && syncp) 
            sts = MFX_ERR_NONE;

        if (MFX_ERR_NONE == sts)
            sts = mfxSession.SyncOperation(syncp, 6000); // Synchronize. Wait until decoded frame is ready
        
        if (MFX_ERR_NONE == sts)
        {
            ++nFrame;
#ifdef ENABLE_OUTPUT
            sts = WriteRawFrame(pmfxOutSurface, fSink);
            MSDK_BREAK_ON_ERROR(sts);

            printf("Frame number: %d\r", nFrame);
#endif
        }            
    }   

    // MFX_ERR_MORE_DATA means that file has ended, need to go to buffering loop, exit in case of other errors
    MSDK_IGNORE_MFX_STS(sts, MFX_ERR_MORE_DATA);
    MSDK_CHECK_RESULT(sts, MFX_ERR_NONE, sts);          
      
    //
    // Stage 2: Retrieve the buffered decoded frames
    //
    while (MFX_ERR_NONE <= sts || MFX_ERR_MORE_SURFACE == sts)        
    {        
        if (MFX_WRN_DEVICE_BUSY == sts)
            Sleep(1); // Wait if device is busy, then repeat the same call to DecodeFrameAsync

        nIndex = GetFreeSurfaceIndex(pmfxSurfaces, numSurfaces); // Find free frame surface
        MSDK_CHECK_ERROR(MFX_ERR_NOT_FOUND, nIndex, MFX_ERR_MEMORY_ALLOC);          

        // Decode a frame asychronously (returns immediately)
        sts = mfxDEC.DecodeFrameAsync(NULL, pmfxSurfaces[nIndex], &pmfxOutSurface, &syncp);

        // Ignore warnings if output is available, 
        // if no output and no action required just repeat the DecodeFrameAsync call        
        if (MFX_ERR_NONE < sts && syncp) 
            sts = MFX_ERR_NONE;

        if (MFX_ERR_NONE == sts)
            sts = mfxSession.SyncOperation(syncp, 6000); // Synchronize. Waits until decoded frame is ready

        if (MFX_ERR_NONE == sts)
        {
            ++nFrame;
#ifdef ENABLE_OUTPUT
            sts = WriteRawFrame(pmfxOutSurface, fSink);
            MSDK_BREAK_ON_ERROR(sts);

            printf("Frame number: %d\r", nFrame);   
#endif
        }
    }

    // MFX_ERR_MORE_DATA indicates that all buffers has been fetched, exit in case of other errors
    MSDK_IGNORE_MFX_STS(sts, MFX_ERR_MORE_DATA);
    MSDK_CHECK_RESULT(sts, MFX_ERR_NONE, sts);

#ifdef ENABLE_BENCHMARK
    QueryPerformanceCounter(&tEnd);
    double duration = ((double)tEnd.QuadPart - (double)tStart.QuadPart)  / freq;
    printf("\nExecution time: %3.2fs (%3.2ffps)\n", duration, nFrame/duration);
#endif

    // ===================================================================
    // Clean up resources
    //  - It is recommended to close Media SDK components first, before releasing allocated surfaces, since
    //    some surfaces may still be locked by internal Media SDK resources.
    
    mfxDEC.Close();
    // mfxSession closed automatically on destruction

    for (int i = 0; i < numSurfaces; i++)
        delete pmfxSurfaces[i];
    MSDK_SAFE_DELETE_ARRAY(pmfxSurfaces);
    MSDK_SAFE_DELETE_ARRAY(surfaceBuffers);
    MSDK_SAFE_DELETE_ARRAY(mfxBS.Data);

    fclose(fSource);
    fclose(fSink);

    return 0;
}
