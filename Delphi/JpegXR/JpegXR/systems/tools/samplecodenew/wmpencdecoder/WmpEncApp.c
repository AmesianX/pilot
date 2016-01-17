//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
#include <WMPGlue.h>
#include <time.h>


//================================================================
// Command line argument support
//================================================================
typedef struct tagWMPENCAPPARGS
{
    char* szInputFile;
    char* szOutputFile;
    
    PKPixelFormatGUID guidPixFormat;
//    Bool bFlagRGB_BGR;

    CWMIStrCodecParam wmiSCP;
    U8 uiDefaultPlanarAlphaQPIndex; 
    Bool bAlphaQPSet;
} WMPENCAPPARGS;

//----------------------------------------------------------------
void WmpEncAppUsage(const char* szExe)
{
    printf(CRLF);
    printf("Windows Media Photo Encoder Utility" CRLF);
    printf("Copyright 2004-2006 Microsoft Corporation - All Rights Reserved" CRLF); 
    printf(CRLF);
    printf("%s [options]..." CRLF, szExe);
    printf(CRLF);
    printf("  -i input.bmp/tif/hdr         Input image file name" CRLF);
    printf("                               bmp: <=8bpc, BGR" CRLF);
    printf("                               tif: >=8bpc, RGB" CRLF);
    printf("                               hdr: 24bppRGBE only" CRLF);
    printf(CRLF);

    printf("  -o output.wdp                Output HD Photo file name" CRLF);
    printf(CRLF);

    printf("  -q quality                   [1 - 255] Default = 1, lossless" CRLF);
    printf(CRLF);

    printf("  -c format                    Required to define uncompressed source pixel format" CRLF);
    printf("                                0: 24bppBGR" CRLF);
    printf("                                1: 1bppBlackWhite" CRLF);
    printf("                                2: 8bppGray" CRLF);
    printf("                                3: 16bppGray" CRLF);
    printf("                                4: 16bppGrayFixedPoint" CRLF);
    printf("                                5: 16bppGrayHalf" CRLF);
//    printf("                               6: 32bppGray" CRLF);
    printf("                                7: 32bppGrayFixedPoint" CRLF);
    printf("                                8: 32bppGrayFloat" CRLF);

    printf("                                9: 24bppRGB" CRLF);
    printf("                               10: 48bppRGB" CRLF);
    printf("                               11: 48bppRGBFixedPoint" CRLF);
    printf("                               12: 48bppRGBHalf" CRLF);
//    printf("                               13: 96bppRGB" CRLF);
    printf("                               14: 96bppRGBFixedPoint" CRLF);
    printf("                               15: 128bppRGBFloat" CRLF);

    printf("                               16: 32bppRGBE" CRLF);

    printf("                               17: 32bppCMYK" CRLF);
    printf("                               18: 64bppCMYK" CRLF);

/*
    printf("                               19 - YUV 420" CRLF);
    printf("                               20 - YUV 422" CRLF);
    printf("                               21 - YUV 444" CRLF);
*/
    printf("                               22: 32bppBGRA" CRLF);
    printf("                               23: 64bppRGBA" CRLF);
    printf("                               24: 64bppRGBAFixedPoint" CRLF);
    printf("                               25: 64bppRGBAHalf" CRLF);
//    printf("                               26 - 128bpp RGBA" CRLF);
    printf("                               27: 128bppRGBAFixedPoint" CRLF);
    printf("                               28: 128bppRGBAFloat" CRLF);

    printf("                               29: 16bppBGR555" CRLF);
    printf("                               30: 16bppBGR565" CRLF);
    printf("                               31: 32bppBGR101010" CRLF);
    //printf("                               101..116 - 1..16 channel 8bpp" CRLF);
    printf("                               32: 40bppCMYKA" CRLF);
    printf("                               33: 80bppCMYKA" CRLF);

    printf("                               34: 32bppBGR" CRLF);
/*
    printf("                               35:  32bppPBGRA" CRLF);
    printf("                               36:  64bppPRGBA" CRLF);
    printf("                               37: 128bppPRGBA Float" CRLF);
*/
    printf(CRLF);

    printf("  -d chroma sub-sampling       0: Y-only" CRLF);
    printf("                               1: YCoCg 4:2:0" CRLF);
    printf("                               2: YCoCg 4:2:2" CRLF);
    printf("                               3: YCoCg 4:4:4 (default)" CRLF);
    printf(CRLF);

    printf("  -l overlapping               0: No overlapping" CRLF);
    printf("                               1: One level overlapping (default)" CRLF);
    printf("                               2: Two level overlapping" CRLF);
    printf(CRLF);

    printf("  -f                           Frequency order bit stream (default is spatial)" CRLF);
    printf(CRLF);    
    printf("  -t                           Display timing information" CRLF);
    printf(CRLF);
    printf("  -v                           Display verbose encoder information" CRLF);
    printf(CRLF);
    printf("  -V tile_wd0 tile_wd1 ...     Macro block rows per tile " CRLF);
    printf(CRLF);
    printf("  -H tile_ht0 tile_ht1 ...     Macro block columns per tile" CRLF);
    printf(CRLF);
    printf("  -U num_h_tiles num_v_tiles   Horiz & vert tile count for uniform tiling" CRLF);
    printf(CRLF);

    printf("  -b Black/White               Applies to 1bpp black/white images" CRLF);
    printf("                               0: 0 = black (default)" CRLF);
    printf("                               1: 0 = white" CRLF);
    printf(CRLF);

    printf("  -a alpha channel format      Required for any pixel format with an alpha channel" CRLF);
    printf("                               2: Planar alpha (default)" CRLF);
    printf("                               3: Interleaved alpha" CRLF);
    printf("                               Other: Reserved, do not use" CRLF);
    printf(CRLF);

    printf("  -Q quality for planar alpha  [1 - 255] Default: same as image quality" CRLF);
    printf(CRLF);

    printf("  -F trimmed flexbits          [0 - 15]  0: no trimming (default)" CRLF);    
    printf("                                        15: trim all" CRLF);    
    printf(CRLF);
    printf("  -s skip subbands             0: All subbands included (default)" CRLF);    
    printf("                               1: Skip flexbits" CRLF);    
    printf("                               2: Skip highpass" CRLF);    
    printf("                               3: Skip highpass & lowpass (DC only)" CRLF);    
    printf(CRLF);
    printf("Eg: %s -i input.bmp -o output.wdp -q 10" CRLF, szExe);
}

void WmpEncAppShowArgs(WMPENCAPPARGS* args)
{
    const char *szCF[] = {"Y_ONLY", "YUV_420", "YUV_422", "YUV_444", "CMYK"};
    
    printf("================================" CRLF);
    printf("Input file:   %s" CRLF, args->szInputFile);
    printf("Output file:  %s" CRLF, args->szOutputFile);
    printf("Color format: %x" CRLF, args->guidPixFormat);
    printf("Internal cf:  %s" CRLF, szCF[args->wmiSCP.cfColorFormat]);
    printf("Overlap:      %s" CRLF, 0 < args->wmiSCP.olOverlap ? "yes" : "no");
    printf("DCOverlap:    %s" CRLF, 1 < args->wmiSCP.olOverlap ? "yes" : "no");
    printf("Alpha:        %s" CRLF, 1 < args->wmiSCP.uAlphaMode ? "yes" : "no");
    printf("================================" CRLF);
}

//----------------------------------------------------------------
void WmpEncAppInitDefaultArgs(WMPENCAPPARGS* args)
{
    memset(args, 0, sizeof(*args));

    args->guidPixFormat = GUID_PKPixelFormatDontCare;

    args->wmiSCP.bVerbose = FALSE;
    args->wmiSCP.cfColorFormat = YUV_444;
//    args->bFlagRGB_BGR = FALSE; //default BGR
    args->wmiSCP.bdBitDepth = BD_LONG;
    args->wmiSCP.bfBitstreamFormat = SPATIAL;
    args->wmiSCP.olOverlap = OL_ONE;
    args->wmiSCP.cNumOfSliceMinus1H = args->wmiSCP.cNumOfSliceMinus1V = 0;
    args->wmiSCP.sbSubband = SB_ALL;
    args->wmiSCP.uAlphaMode = 0;
    args->wmiSCP.nLenMantissaOrShift = 0;
    args->wmiSCP.nExpBias = 0;
    args->wmiSCP.bBlackWhite = 0;
    args->wmiSCP.uiTrimFlexBits = 0;
    args->wmiSCP.uiDefaultQPIndex = 1;
    args->uiDefaultPlanarAlphaQPIndex = 1;
    args->bAlphaQPSet = 0;
}

ERR WmpEncAppValidateArgs(WMPENCAPPARGS* args)
{
    ERR err = WMP_errSuccess;

    Test(NULL != args->szInputFile, WMP_errInvalidParameter);
    Test(NULL != args->szOutputFile, WMP_errInvalidParameter);

Cleanup:
    return err;
}

ERR WmpEncAppParseArgs(int argc, char* argv[], WMPENCAPPARGS* args)
{
    ERR err = WMP_errSuccess;

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

    size_t InvalidPF[9] = {6, 13, 19, 20, 21, 26, 35, 36, 37};
    size_t AlphaPF[8] = {22, 23, 24, 25, 27, 28, 32, 33};

    int i = 1, j = 0, k;
    char c;
    size_t idxPF = 0;

    WmpEncAppInitDefaultArgs(args);

    while (i < argc && argv[i][0] == '-')
    {
        switch ((c = argv[i][1])) {
            /* the no-argument switches */
            case 't':
                // NOOP - now we always print timing info
                break;

            case 'v':
                args->wmiSCP.bVerbose = !FALSE;
                break;
                
            /* simple flag argument */
            case 'f':
                args->wmiSCP.bfBitstreamFormat = FREQUENCY;
                break;
            
            default:
                i ++;
                if (i == argc || argv[i][0] == '-') // need more info
                    Call(WMP_errInvalidArgument);

                switch (c)
                {
                    case 'i':
                        args->szInputFile = argv[i];
                        break;

                    case 'o':
                        args->szOutputFile = argv[i];
                        break;

                    case 'q':
                        args->wmiSCP.uiDefaultQPIndex = (U8)(atoi(argv[i]));
                        if(0 == args->bAlphaQPSet)//user didn't set -Q
                            args->uiDefaultPlanarAlphaQPIndex = args->wmiSCP.uiDefaultQPIndex;
                        break;

                    case 'Q':
                        args->bAlphaQPSet = 1; 
                        args->uiDefaultPlanarAlphaQPIndex = (U8)(atoi(argv[i]));
                        break;

                    case 's':
                        args->wmiSCP.sbSubband = (SUBBAND)(atoi(argv[i]));
                        break;

                    case 'c':
                        idxPF = (size_t)atol(argv[i]);
                        for (k = 0; k < 9; k++)
                        {
                            if (InvalidPF[k] == idxPF)
                            {
                                printf("Unsupported format in Vista.\n");
                                Call(WMP_errInvalidArgument);
                            }
                        }
                        break;

                    case 'a': 
                        args->wmiSCP.uAlphaMode = (U8)atoi(argv[i]);
                        break;

/*                    case 'R':
                        args->bFlagRGB_BGR = (Bool)atoi(argv[i]);
                        break;
*/                
                    case 'l':
                        args->wmiSCP.olOverlap = (OVERLAP)atoi(argv[i]);
                        break;

                    case 'd':
                        args->wmiSCP.cfColorFormat = (COLORFORMAT)atoi(argv[i]);
                        break;
                    
                    case 'H': // horizontal tiling
                        for(j = 0;;i ++, j ++){
                            args->wmiSCP.uiTileY[j] = atoi(argv[i]);
                            if(i + 1 == argc || argv[i + 1][0] == '-' || j >= 255)
                                break;
                        }
                        args->wmiSCP.cNumOfSliceMinus1H = (U8)j;

                        break;

                    case 'V': // vertical tiling
                        for(j = 0;;i ++, j ++){
                            args->wmiSCP.uiTileX[j] = atoi(argv[i]);
                            if(i + 1 == argc || argv[i + 1][0] == '-' || j >= 255)
                                break;
                        }
                        args->wmiSCP.cNumOfSliceMinus1V = (U8)j;
                        break;

                    case 'U': // uniform tiling
                        if(i + 1 < argc && argv[i + 1][0] != '-'){
                            if(atoi(argv[i]) > 0 && atoi(argv[i + 1]) > 0){
                                args->wmiSCP.cNumOfSliceMinus1H = atoi(argv[i]) - 1;
                                args->wmiSCP.cNumOfSliceMinus1V = atoi(argv[i + 1]) - 1;
                            }
                            i ++;
                        }
                        break;

                    case 'm':
                        args->wmiSCP.nLenMantissaOrShift = (U8)atoi(argv[i]);
                        break;

                    case 'C':
                        args->wmiSCP.nExpBias = (I8) atoi(argv[i]) + 128; // rollover arithmetic
                        break;

                    case 'b':
                        args->wmiSCP.bBlackWhite = (Bool)atoi(argv[i]);
                        break;
                        
                    case 'F':
                        args->wmiSCP.uiTrimFlexBits = (U8)atoi(argv[i]);
                        if (args->wmiSCP.uiTrimFlexBits > 15)
                            args->wmiSCP.uiTrimFlexBits = 15;
                        break;

                    default:
                        Call(WMP_errInvalidArgument);
                }
        }

        i ++;
    }

    FailIf(sizeof2(pixelFormat) <= idxPF, WMP_errUnsupportedFormat);
    args->guidPixFormat = *pixelFormat[idxPF];

    if ((idxPF >= 1) && (idxPF <= 8))
        args->wmiSCP.cfColorFormat = Y_ONLY;
    else if ((idxPF == 17) || (idxPF == 18) || (idxPF == 32) || (idxPF == 33))
        args->wmiSCP.cfColorFormat = CMYK;

    for (k = 0; k < 8; k++)
    {
        if (AlphaPF[k] == idxPF) 
        {
            if(0 == args->wmiSCP.uAlphaMode)//with Alpha and no default, set default as Planar
            {
                args->wmiSCP.uAlphaMode = 2;
            }
            break;
        }
    }

    //================================
    Call(WmpEncAppValidateArgs(args));

Cleanup:
    return err;
}


//================================================================
// main function
//================================================================
int 
#ifndef __ANSI__
__cdecl 
#endif // __ANSI__
main(int argc, char* argv[])
{
    ERR err = WMP_errSuccess;

    PKFactory* pFactory = NULL;
    struct WMPStream* pEncodeStream = NULL;
    PKCodecFactory* pCodecFactory = NULL;
    PKImageEncode* pEncoder = NULL;

    clock_t start = 0, finish = 0;
    WMPENCAPPARGS args;

    //================================
    // parse command line parameters
    if (1 == argc)
    {
        WmpEncAppUsage(argv[0]);
        return 0;
    }

    Call(WmpEncAppParseArgs(argc, argv, &args));
    if (args.wmiSCP.bVerbose)
    {
        WmpEncAppShowArgs(&args);
    }

    //================================
    Call(PKCreateFactory(&pFactory, PK_SDK_VERSION));
    Call(pFactory->CreateStreamFromFilename(&pEncodeStream, args.szOutputFile, "wb"));

    //================================
    Call(PKCreateCodecFactory(&pCodecFactory, WMP_SDK_VERSION));
    Call(pCodecFactory->CreateCodec(&IID_PKImageWmpEncode, &pEncoder));

    Call(pEncoder->Initialize(pEncoder, pEncodeStream, &args.wmiSCP, sizeof(args.wmiSCP)));

    if(pEncoder->WMP.wmiSCP.uAlphaMode == 2)
        pEncoder->WMP.wmiSCP_Alpha.uiDefaultQPIndex = args.uiDefaultPlanarAlphaQPIndex;

    //----------------------------------------------------------------
    //
    // go through each image
    //
    //for (i = 0; ; ++i)
    {
        PKImageDecode* pDecoder = NULL;
        PKFormatConverter* pConverter = NULL;

        Float rX = 0.0, rY = 0.0;
        PKRect rect = {0, 0, 0, 0};

        //================================
        Call(pCodecFactory->CreateDecoderFromFile(args.szInputFile, &pDecoder));

        //================================
        Call(pCodecFactory->CreateFormatConverter(&pConverter));
        Call(pConverter->Initialize(pConverter, pDecoder, NULL, args.guidPixFormat));

        //================================
        Call(pEncoder->SetPixelFormat(pEncoder, args.guidPixFormat));

        Call(pDecoder->GetSize(pDecoder, &rect.Width, &rect.Height));
        Call(pEncoder->SetSize(pEncoder, rect.Width, rect.Height));

        Call(pDecoder->GetResolution(pDecoder, &rX, &rY));
        Call(pEncoder->SetResolution(pEncoder, rX, rY));

        //================================
        // re-encode the input source to the output
        //
		pEncoder->WriteSource = PKImageEncode_WriteSource;
        Call(pEncoder->WriteSource(pEncoder, pConverter, &rect));

        pConverter->Release(&pConverter);
        pDecoder->Release(&pDecoder);

        //if (i + 1 == 5)
        //{
        //    break;
        //}

        // multi-frame support NYI
        //Call(pEncoder->CreateNewFrame(pEncoder, &wmiSCP, sizeof(wmiSCP)));
    }

//    Call(pEncoder->Terminate(pEncoder));
    pEncoder->Release(&pEncoder);

Cleanup:
    if (WMP_errSuccess != err)
    {
        WmpEncAppUsage(argv[0]);
    }
    
    return (int)err;
}


//================================================================
#ifdef _WIN32_WCE
/*
#==============
# comments
#==============
WMPEncApp.exe -i \Temp\test.bmp -o \Temp\test.wdp -q 1
WMPDecApp.exe -i \Temp\test.wdp -o \Temp\test.bmp -c 6
*/

#define DEFDIR "\\Temp\\"

int GetArguments(int* pc, char** ppv[])
{
    size_t i = 0;
    static char line[132];
    static char* args[20] =
    {
        "WMPEncApp.exe",
        "-i",
        DEFDIR "test.bmp",
        "-o",
        DEFDIR "test.wdp", 
        "-q",
        "1",
        "-v",
        "-t",
        NULL,
    };

    FILE* pfIn = fopen(DEFDIR "WMPIni.txt", "r");

    // default arguments to return
    *pc = 9;
    *ppv = args;

    while (fgets(line, sizeof2(line), pfIn))
    {
        // search for the matching line
        if (0 == strcmp(strtok(line, " \t"), args[0]))
        {
            // extract each argument from the matching line
            for (i = 1; i < sizeof2(args) - 1 && (args[i] = strtok(NULL, " \t\n\r")); ++i);
            *pc = i;

            printf("Args from WMPIni.txt" CRLF);
            break;
        }
    }

    fclose(pfIn);

    for (i = 0; i < (size_t)*pc; printf("%s ", args[i++]));
    puts("");
    return 0;
}

//================================================================
// Windows CE App entry
//================================================================
int WINAPI WinMain(HINSTANCE    hInstance,
                   HINSTANCE    hPrevInstance,
                   LPTSTR       lpCmdLine,
                   int          nCmdShow)
{
    int argc = 0;
    char** argv = NULL;
    
    //================================
    // redirect stdou and stderr
    fclose(stdout);
    fopen(DEFDIR "stdout.txt", "wb");

    fclose(stderr);
    fopen(DEFDIR "stderr.txt", "wb");

    //================================
    GetArguments(&argc, &argv);

    //================================
    return main(argc, argv);
}
#endif

