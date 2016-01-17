// WMPEncDll.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "WMPEncDll.h"
#include <WMPGlue.h>
#include <time.h>

// This is an example of an exported variable
//WMPENCDLL_API int nWMPEncDll=0;

void SetParam(CWMIStrCodecParam* Param, PKPixelFormatGUID* MyPixelFormat, int Quility, int PixelFormatIndex)
{
	#pragma region PixelFormat
	static const PKPixelFormatGUID* pixelFormat[] =
	{
		&GUID_PKPixelFormat24bppBGR,

		&GUID_PKPixelFormatBlackWhite,

		&GUID_PKPixelFormat8bppGray,
		&GUID_PKPixelFormat16bppGray,
		&GUID_PKPixelFormat16bppGrayFixedPoint,
		&GUID_PKPixelFormat16bppGrayHalf,
		&GUID_PKPixelFormat32bppGray,
		&GUID_PKPixelFormat32bppGrayFixedPoint,
		&GUID_PKPixelFormat32bppGrayFloat,

		&GUID_PKPixelFormat24bppRGB,
		&GUID_PKPixelFormat48bppRGB,
		&GUID_PKPixelFormat48bppRGBFixedPoint,
		&GUID_PKPixelFormat48bppRGBHalf,
		&GUID_PKPixelFormat96bppRGB,
		&GUID_PKPixelFormat96bppRGBFixedPoint,
		&GUID_PKPixelFormat128bppRGBFloat,

		&GUID_PKPixelFormat32bppRGBE,
		&GUID_PKPixelFormat32bppCMYK,
		&GUID_PKPixelFormat64bppCMYK,

		&GUID_PKPixelFormat12bppYUV420, 
		&GUID_PKPixelFormat16bppYUV422,
		&GUID_PKPixelFormat24bppYUV444,

		//&GUID_PKPixelFormat32bppRGBA,
		&GUID_PKPixelFormat32bppBGRA,
		&GUID_PKPixelFormat64bppRGBA,
		&GUID_PKPixelFormat64bppRGBAFixedPoint,
		&GUID_PKPixelFormat64bppRGBAHalf,
		&GUID_PKPixelFormat128bppRGBA,
		&GUID_PKPixelFormat128bppRGBAFixedPoint,
		&GUID_PKPixelFormat128bppRGBAFloat,

		//&GUID_PKPixelFormat32bppPBGRA
		&GUID_PKPixelFormat16bppRGB555,
		&GUID_PKPixelFormat16bppRGB565,
		&GUID_PKPixelFormat32bppRGB101010,
		&GUID_PKPixelFormat40bppCMYKAlpha,
		&GUID_PKPixelFormat80bppCMYKAlpha,
		&GUID_PKPixelFormat32bppBGR,
		&GUID_PKPixelFormat32bppPBGRA,
		&GUID_PKPixelFormat64bppPRGBA,
		&GUID_PKPixelFormat128bppPRGBAFloat,
	};
#pragma endregion
    memset(Param, 0, sizeof(*Param));
    //args->guidPixFormat = GUID_PKPixelFormatDontCare;

    Param->bVerbose = FALSE;
    Param->cfColorFormat = YUV_444;
//    args->bFlagRGB_BGR = FALSE; //default BGR
    Param->bdBitDepth = BD_LONG;
    Param->bfBitstreamFormat = SPATIAL;
    Param->olOverlap = OL_ONE;
    Param->cNumOfSliceMinus1H = Param->cNumOfSliceMinus1V = 0;
    Param->sbSubband = SB_ALL;
    Param->uAlphaMode = 0;
    Param->nLenMantissaOrShift = 0;
    Param->nExpBias = 0;
    Param->bBlackWhite = 0;
    Param->uiTrimFlexBits = 0;
    Param->uiDefaultQPIndex = 1;
    //args->uiDefaultPlanarAlphaQPIndex = 1;
    //args->bAlphaQPSet = 0;
	*MyPixelFormat = *(pixelFormat[PixelFormatIndex]);
	Param->uiDefaultQPIndex = (U8)Quility;
}


ERR PKCodecFactory_CreateDecoderFromMemory(void* Data, int Size, PKImageDecode** ppDecoder) //Bitmap Only
{
    ERR err = WMP_errSuccess;

    char *pExt = NULL;
    PKIID* pIID = NULL;

    struct WMPStream* pStream = NULL;
    PKImageDecode* pDecoder = NULL;

    // get file extension
    pExt = strrchr((char*)".bmp", '.');
    FailIf(NULL == pExt, WMP_errUnsupportedFormat);

    // get decode PKIID
    Call(GetImageDecodeIID(pExt, (const PKIID**)&pIID));

    // create stream
	Call(CreateWS_Memory(&pStream, Data, Size));

    // Create decoder
    Call(PKCodecFactory_CreateCodec(pIID, (void**)ppDecoder));
    pDecoder = *ppDecoder;

    // attach stream to decoder
    Call(pDecoder->Initialize(pDecoder, pStream));
    pDecoder->fStreamOwner = !0;

Cleanup:
    return err;
}

WMPENCDLL_API int BitmapStreamToXR(void* Data, int Size, int Quility, int PixelFormat, void** Output, int* OutputSize)
{
	if(Quility < 1) Quility = 1;
	if(Quility > 255) Quility = 255;

	PKFactory* pFactory = NULL;
    struct WMPStream* pEncodeStream = NULL;
    PKCodecFactory* pCodecFactory = NULL;
    PKImageEncode* pEncoder = NULL;

	PKPixelFormatGUID MyPixelFormat;

	CWMIStrCodecParam CodecParam;
	SetParam(&CodecParam, &MyPixelFormat, Quility, 0);	 //PixelFormat Type 0;
	CodecParam.uiDefaultQPIndex = (U8)Quility;


	unsigned char* Mem = (unsigned char*)malloc(Size);
	memset(Mem, 0, Size);

	ERR err = PKCreateFactory(&pFactory, PK_SDK_VERSION);
	pFactory->CreateStreamFromMemory(&pEncodeStream, Mem, Size);
	
	PKCreateCodecFactory(&pCodecFactory, WMP_SDK_VERSION);
	pCodecFactory->CreateCodec(&IID_PKImageWmpEncode, (void**)&pEncoder);
 
	pEncoder->Initialize(pEncoder, pEncodeStream, &CodecParam, sizeof(CodecParam));
	{
        PKImageDecode* pDecoder = NULL;
        PKFormatConverter* pConverter = NULL;

        Float rX = 0.0, rY = 0.0;
        PKRect rect = {0, 0, 0, 0};

        //================================
		Call(PKCodecFactory_CreateDecoderFromMemory(Data, Size, &pDecoder));

        //================================
        Call(pCodecFactory->CreateFormatConverter(&pConverter));
        Call(pConverter->Initialize(pConverter, pDecoder, NULL, MyPixelFormat));

        //================================
        Call(pEncoder->SetPixelFormat(pEncoder, MyPixelFormat));

        Call(pDecoder->GetSize(pDecoder, &rect.Width, &rect.Height));
        Call(pEncoder->SetSize(pEncoder, rect.Width, rect.Height));

        Call(pDecoder->GetResolution(pDecoder, &rX, &rY));
        Call(pEncoder->SetResolution(pEncoder, rX, rY));

        //================================
        // re-encode the input source to the output
        //
		pEncoder->WriteSource = PKImageEncode_WriteSource;
		pEncoder->WriteSource(pEncoder, pConverter, &rect);
		
		*OutputSize = *(int*)(Mem + 0x72) + (pEncoder->pStream->state.buf.cbCur);
		*Output = Mem;		

        pConverter->Release(&pConverter);
        pDecoder->Release(&pDecoder);
    }
    pEncoder->Release(&pEncoder);
Cleanup:
	return 0;
}
WMPENCDLL_API void FreeXR(void* Data)
{
	free(Data);
}

// This is the constructor of a class that has been exported.
// see WMPEncDll.h for the class definition