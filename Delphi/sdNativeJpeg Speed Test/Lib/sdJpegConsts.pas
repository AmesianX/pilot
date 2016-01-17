//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegConsts;

interface

type
  TsdMarkerSet = set of byte;

const

  // Jpeg markers defined in Table B.1
  
  mkSOF0  = $c0; // Baseline DCT + Huffman encoding
  mkSOF1  = $c1; // Extended Sequential DCT + Huffman encoding
  mkSOF2  = $c2; // Progressive DCT + Huffman encoding
  mkSOF3  = $c3; // Lossless (sequential) + Huffman encoding

  mkSOF5  = $c5; // Differential Sequential DCT + Huffman encoding
  mkSOF6  = $c6; // Differential Progressive DCT + Huffman encoding
  mkSOF7  = $c7; // Differential Lossless (sequential) + Huffman encoding

  mkJPG   = $c8; // Reserved for Jpeg extensions
  mkSOF9  = $c9; // Extended Sequential DCT + Arithmetic encoding
  mkSOF10 = $ca; // Progressive DCT + Arithmetic encoding
  mkSOF11 = $cb; // Lossless (sequential) + Arithmetic encoding

  mkSOF13 = $cd; // Differential Sequential DCT + Arithmetic encoding
  mkSOF14 = $ce; // Differential Progressive DCT + Arithmetic encoding
  mkSOF15 = $cf; // Differential Lossless (sequential) + Arithmetic encoding

  mkDHT   = $c4; // Define Huffman Table

  mkDAC   = $cc; // Define Arithmetic Coding

  mkRST0  = $d0; // Restart markers
  mkRST1  = $d1;
  mkRST2  = $d2;
  mkRST3  = $d3;
  mkRST4  = $d4;
  mkRST5  = $d5;
  mkRST6  = $d6;
  mkRST7  = $d7;

  mkSOI   = $d8; // Start of Image
  mkEOI   = $d9; // End of Image
  mkSOS   = $da; // Start of Scan
  mkDQT   = $db; // Define Quantization Table
  mkDNL   = $dc; // Define Number of Lines
  mkDRI   = $dd; // Define Restart Interval
  mkDHP   = $de; // Define Hierarchical Progression
  mkEXP   = $df; // Expand reference components

  // For APPn markers see:
  // http://www.ozhiker.com/electronics/pjmt/jpeg_info/app_segments.html

  mkAPP0  = $e0; // APPn markers - APP0 = JFIF
  mkAPP1  = $e1; //                APP1 = EXIF or XMP
  mkAPP2  = $e2; //                ICC colour profile
  mkAPP3  = $e3;
  mkAPP4  = $e4;
  mkAPP5  = $e5;
  mkAPP6  = $e6;
  mkAPP7  = $e7;
  mkAPP8  = $e8;
  mkAPP9  = $e9;
  mkAPP10 = $ea;
  mkAPP11 = $eb;
  mkAPP12 = $ec;
  mkAPP13 = $ed; //                APP13 = IPTC or Adobe IRB
  mkAPP14 = $ee; //                APP14 = Adobe
  mkAPP15 = $ef;

  mkJPG0  = $f0; // JPGn markers - reserved for JPEG extensions
  mkJPG13 = $fd;
  mkCOM   = $fe; // Comment

  mkTEM   = $01; // Reserved for temporary use

type

  TsdZigZagArray = array[0..63 + 16] of byte;
  PsdZigZagArray = ^TsdZigZagArray;
  TsdIntArray64 = array[0..63] of integer;


const
  // This matrix maps zigzag position to the left/right
  // top/down normal position inside the 8x8 block.

  cJpegInverseZigZag1x1: TsdZigZagArray =
    ( 0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegInverseZigZag2x2: TsdZigZagArray =
    ( 0,  1,  2,  0,  3,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegInverseZigZag4x4: TsdZigZagArray =
    ( 0,  1,  4,  8,  5,  2,  3,  6,
      9, 12,  0, 13, 10,  7,  0,  0,
      0, 11, 14,  0,  0,  0,  0,  0,
     15,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegInverseZigZag8x8: TsdZigZagArray =
    ( 0,  1,  8, 16,  9,  2,  3, 10,
     17, 24, 32, 25, 18, 11,  4,  5,
     12, 19, 26, 33, 40, 48, 41, 34,
     27, 20, 13,  6,  7, 14, 21, 28,
     35, 42, 49, 56, 57, 50, 43, 36,
     29, 22, 15, 23, 30, 37, 44, 51,
     58, 59, 52, 45, 38, 31, 39, 46,
     53, 60, 61, 54, 47, 55, 62, 63,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegForwardZigZag8x8: TsdZigZagArray =
    ( 0,  1,  5,  6, 14, 15, 27, 28,
      2,  4,  7, 13, 16, 26, 29, 42,
      3,  8, 12, 17, 25, 30, 41, 43,
      9, 11, 18, 24, 31, 40, 44, 53,
     10, 19, 23, 32, 39, 45, 52, 54,
     20, 22, 33, 38, 46, 51, 55, 60,
     21, 34, 37, 47, 50, 56, 59, 61,
     35, 36, 48, 49, 57, 58, 62, 63,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  cJpegNaturalZigZag8x8: TsdZigZagArray =
    ( 0,  1,  2,  3,  4,  5,  6,  7,
      8,  9, 10, 11, 12, 13, 14, 15,
     16, 17, 18, 19, 20, 21, 22, 23,
     24, 25, 26, 27, 28, 29, 30, 31,
     32, 33, 34, 35, 36, 37, 38, 39,
     40, 41, 42, 43, 44, 45, 46, 47,
     48, 49, 50, 51, 52, 53, 54, 55,
     56, 57, 58, 59, 60, 61, 62, 63,
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0);

  // entry n equals 1 shl (n-1)
  cExtendTest: array[0..15] of integer =
    ($0000, $0001, $0002, $0004, $0008, $0010, $0020, $0040,
     $0080, $0100, $0200, $0400, $0800, $1000, $2000, $4000);

  // entry n equals (-1 shl n) + 1
  cExtendOffset: array[0..15] of integer =
   (0, ((-1) shl 1 ) + 1, ((-1) shl 2 ) + 1, ((-1) shl 3 ) + 1, ((-1) shl 4 ) + 1,
       ((-1) shl 5 ) + 1, ((-1) shl 6 ) + 1, ((-1) shl 7 ) + 1, ((-1) shl 8 ) + 1,
       ((-1) shl 9 ) + 1, ((-1) shl 10) + 1, ((-1) shl 11) + 1, ((-1) shl 12) + 1,
       ((-1) shl 13) + 1, ((-1) shl 14) + 1, ((-1) shl 15) + 1);

  // These are the sample quantization tables given in JPEG spec section K.1.
  // The spec says that the values given produce "good" quality, and
  // when divided by 2, "very good" quality.

  cStdLuminanceQuantTbl: TsdIntArray64 =
   (16,  11,  10,  16,  24,  40,  51,  61,
    12,  12,  14,  19,  26,  58,  60,  55,
    14,  13,  16,  24,  40,  57,  69,  56,
    14,  17,  22,  29,  51,  87,  80,  62,
    18,  22,  37,  56,  68, 109, 103,  77,
    24,  35,  55,  64,  81, 104, 113,  92,
    49,  64,  78,  87, 103, 121, 120, 101,
    72,  92,  95,  98, 112, 100, 103,  99);

  cStdChrominanceQuantTbl: TsdIntArray64 =
   (17,  18,  24,  47,  99,  99,  99,  99,
    18,  21,  26,  66,  99,  99,  99,  99,
    24,  26,  56,  99,  99,  99,  99,  99,
    47,  66,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99,
    99,  99,  99,  99,  99,  99,  99,  99);

  // These are standard Huffman tables for general use

  cHuffmanBitsDcLum: array[0..15] of byte =
    (0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0);
  cHuffmanValDCLum: array[0..11] of byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

  cHuffmanBitsDCChrom: array[0..15] of byte =
    (0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0);
  cHuffmanValDCChrom: array[0..11] of byte =
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );

  const cHuffmanBitsACLum: array[0..15] of byte =
    (0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, $7d);
  const cHuffmanValACLum: array[0..161] of byte =
    ( $01, $02, $03, $00, $04, $11, $05, $12,
      $21, $31, $41, $06, $13, $51, $61, $07,
      $22, $71, $14, $32, $81, $91, $a1, $08,
      $23, $42, $b1, $c1, $15, $52, $d1, $f0,
      $24, $33, $62, $72, $82, $09, $0a, $16,
      $17, $18, $19, $1a, $25, $26, $27, $28,
      $29, $2a, $34, $35, $36, $37, $38, $39,
      $3a, $43, $44, $45, $46, $47, $48, $49,
      $4a, $53, $54, $55, $56, $57, $58, $59,
      $5a, $63, $64, $65, $66, $67, $68, $69,
      $6a, $73, $74, $75, $76, $77, $78, $79,
      $7a, $83, $84, $85, $86, $87, $88, $89,
      $8a, $92, $93, $94, $95, $96, $97, $98,
      $99, $9a, $a2, $a3, $a4, $a5, $a6, $a7,
      $a8, $a9, $aa, $b2, $b3, $b4, $b5, $b6,
      $b7, $b8, $b9, $ba, $c2, $c3, $c4, $c5,
      $c6, $c7, $c8, $c9, $ca, $d2, $d3, $d4,
      $d5, $d6, $d7, $d8, $d9, $da, $e1, $e2,
      $e3, $e4, $e5, $e6, $e7, $e8, $e9, $ea,
      $f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );

  cHuffmanBitsACChrom: array[0..15] of byte =
    (0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, $77);
  cHuffmanValACChrom: array[0..161] of byte =
    ( $00, $01, $02, $03, $11, $04, $05, $21,
      $31, $06, $12, $41, $51, $07, $61, $71,
      $13, $22, $32, $81, $08, $14, $42, $91,
      $a1, $b1, $c1, $09, $23, $33, $52, $f0,
      $15, $62, $72, $d1, $0a, $16, $24, $34,
      $e1, $25, $f1, $17, $18, $19, $1a, $26,
      $27, $28, $29, $2a, $35, $36, $37, $38,
      $39, $3a, $43, $44, $45, $46, $47, $48,
      $49, $4a, $53, $54, $55, $56, $57, $58,
      $59, $5a, $63, $64, $65, $66, $67, $68,
      $69, $6a, $73, $74, $75, $76, $77, $78,
      $79, $7a, $82, $83, $84, $85, $86, $87,
      $88, $89, $8a, $92, $93, $94, $95, $96,
      $97, $98, $99, $9a, $a2, $a3, $a4, $a5,
      $a6, $a7, $a8, $a9, $aa, $b2, $b3, $b4,
      $b5, $b6, $b7, $b8, $b9, $ba, $c2, $c3,
      $c4, $c5, $c6, $c7, $c8, $c9, $ca, $d2,
      $d3, $d4, $d5, $d6, $d7, $d8, $d9, $da,
      $e2, $e3, $e4, $e5, $e6, $e7, $e8, $e9,
      $ea, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
      $f9, $fa );

resourcestring

  sInternalError                   = 'Internal error';
  sUnsupportedEncoding             = 'Unsupported encoding: SOF%d';
  sMarkerExpected                  = 'Jpeg Marker expected';
  sUnsupportedBitsPerSample        = 'Unsupported bits per sample';
  sInvalidTableClass               = 'Invalid table class in DHT marker';
  sInputStreamChopped              = 'Input stream prematurely chopped';
  sUnexpectedMarkerInEncodedStream = 'Unexpected marker in encoded stream';
  sInvalidFrameRef                 = 'Invalid frame reference in scan component';
  sNoColorTransformation           = 'No color transformation available for current settings';
  sNoDCTCoefficentsAvailable       = 'No DCT coefficients available (load first)';
  sOperationOnlyFor8x8             = 'Operation can only be performed with LoadScale = lsFull';
  sBitmapIsEmptyCannotSave         = 'Bitmap is empty; cannot save';
  sInvalidFormatForSelectedCS      = 'Invalid bitmap format for selected color space';
  sCommentCannotBeSet              = 'Comment cannot be set before assigning bitmap';
  sDNLMarkerExpected               = 'DNL marker expected';
  sUnsupportedColorSpace           = 'Unsupported color space';
  sOnProvideStripMustBeAssigned    = 'OnProvideStrip must be assigned for SaveToStreamStripByStrip';
  sCannotUseTileMode               = 'Cannot use tilemode with progressive jpeg';
  sRangeErrorInTileLoading         = 'Range error in tiled loading: make sure to select tilemode'; 

implementation

end.
