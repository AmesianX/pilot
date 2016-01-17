//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegColors;

interface

uses
  Classes, Windows, Graphics;

type

  TsdColorTransform = class(TPersistent)
  public
    constructor Create; virtual;
    // Transform Count colors from Source to Dest
    procedure Transform(Source, Dest: pointer; Count: integer); virtual; abstract;
  end;

  // Transform class
  TsdColorTransformClass = class of TsdColorTransform;

  // Null transform: 8bit/pixel direct copy.
  TsdNullTransform8bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Null transform: 16bit/pixel direct copy.
  TsdNullTransform16bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Null transform: 24bit/pixel direct copy.
  TsdNullTransform24bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Null transform: 32bit/pixel direct copy.
  TsdNullTransform32bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Invert gray values from 0->255 and 255->0
  TsdTransformInverseGray8bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;


  // Inversion transform: invert colour triplets RGB->BGR, can be used for any
  // 24bit triplet of colours that needs to be inverted in order.
  TsdTransformInvertTriplet24bit = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Y-Cb-Cr (24bit) to BGR (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does).
  TsdTransformYCbCrToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Y-Cb-Cr (24bit) to BGRA (32bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channel A is set to $FF.
  TsdTransformYCbCrToBGRA = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Y-Cb-Cr (24bit) to gray (8it) colour transform. The Y channel is used
  // as grayscale directly, Cb and Cr channels are not used.
  TsdTransformYCbCrToGray = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Y-Cb-Cr-A (32bit) to BGR (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The input
  // alpha (A) channel is ignored.
  TsdTransformYCbCrAToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // YCbCrK to BGR. YCbCr is first converted to CMY, then CMYK is converted to RGB
  TsdTransformYCbCrKToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Y-Cb-Cr-A (32bit) to BGRA (32bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channels are copied.
  TsdTransformYCbCrAToBGRA = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // CMYK (32bit) to BGR (24bit) colour transform.
  TsdTransformCMYKToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // CMYK (32bit) to BGR (24bit) colour transform, Adobe specific.
  TsdTransformCMYKToBGR_Adobe = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // YCCK (32bit) to BGR (24bit) colour transform. The CMY channels are coded as
  // Y-Cb-Cr, thus first unencoded to CMY, then combined with K to do CMYK to RGB.
  TsdTransformYCCKToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // YCCK to BGR colour transform, as Adobe does it. Experimental status!
  TsdTransformYCCKToBGR_Adobe = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Gray (8bit) to BGR (24bit) transform. The R, G and B channels are all set
  // to the gray value.
  TsdTransformGrayToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Gray + Alpha (16bit) to BGR (24bit) transform. The R, G and B channels are all set
  // to the gray value. The Alpha channel is ignored.
  TsdTransformGrayAToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // Gray + Alpha (16bit) to BGRA (32bit) transform. The R, G and B channels are all set
  // to the gray value. The Alpha channels are copied.
  TsdTransformGrayAToBGRA = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // RGB (24bit) to BGRA (32bit) transform. The output alpha (A) channel is
  // set to $FF.
  TsdTransformRGBToBGRA = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // RGBA (32bit) to BGR (24bit) transform. The input alpha (A) channel is ignored.
  TsdTransformRGBAToBGR = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // BGRA (32bit) to BGR (24bit) transform. This transform uses a parameter,
  // BkColor (TColor) that is used to fill the background, while the colors are
  // blended using the "over" operator. The background color by default is clWhite.
  // This routine assumes alpha pre-multiplied colors. 
  TsdTransformBGRAToBGR = class(TsdColorTransform)
  private
    FBkColor: cardinal;
    procedure SetBkColor(const Value: TColor);
    function GetBkColor: TColor;
  public
    constructor Create; override;
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    property BkColor: TColor read GetBkColor write SetBkColor;
  end;

  // RGB (24bit) to Y-Cb-Cr (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does).
  TsdTransformBGRToYCbCr = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // RGB (24bit) to Gray (8bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). It uses
  // the same formula to find Gray as in RGB->YCbCr
  TsdTransformBGRToGray = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // RGBA (32bit) to Y-Cb-Cr-A (32bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channels are copied.
  TsdTransformBGRAToYCbCrA = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // RGBA (32bit) to Y-Cb-Cr (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channel is ignored.
  TsdTransformBGRAToYCbCr = class(TsdColorTransform)
  public
    procedure Transform(Source, Dest: pointer; Count: integer); override;
  end;

  // CIE L*a*b* (24bit) to BGR (24bit), using parameters
  TsdTransformCIELabToBGR = class(TsdColorTransform)
  private
    FXw: double;
    FYw: double;
    FZw: double;
    FAmin: double;
    FAmax: double;
    FBmin: double;
    FBmax: double;
    FAofs: integer;
    FBofs: integer;
  public
    constructor Create; override;
    procedure Transform(Source, Dest: pointer; Count: integer); override;
    // White point
    property Xw: double read FXw write FXw;
    property Yw: double read FYw write FYw;
    property Zw: double read FZw write FZw;
    // Range
    property Amin: double read FAmin write FAmin;
    property Amax: double read FAmax write FAmax;
    property Bmin: double read FBmin write FBmin;
    property Bmax: double read FBmax write FBmax;
    // Offset
    property Aofs: integer read FAofs write FAofs;
    property Bofs: integer read FBofs write FBofs;
  end;

  // ITU CIE L*a*b* (24bit) to BGR (24bit), with canned parameters, which are
  // set in the constructor
  TsdTransformITUCIELabToBGR = class(TsdTransformCIELabToBGR)
  public
    constructor Create; override;
  end;

  // to do
  TsdTransformRGBAToGray = class(TsdColorTransform);
  TsdTransformRGBToCMYK = class(TsdColorTransform);
  TsdTransformRGBToYCCK = class(TsdColorTransform);
  TsdTransformCMYKToYCCK = class(TsdColorTransform);

type

  T32bitPaletteEntry = packed record
    case integer of
    0: (R, G, B, A: byte);
    1: (Color: longint);
  end;

  T8bitPaletteArray = array[0..255] of T32bitPaletteEntry;

procedure SetBitmap8bitGrayscale(Bitmap: TBitmap);

implementation

uses
  Math;

procedure SetBitmap8bitPalette(const APalette: T8bitPaletteArray; ABitmap: TBitmap);
// Add a 256-color palette to a bitmap
var
  i: integer;
  pal: PLogPalette;
  hpal: HPALETTE;
begin
  if not assigned(ABitmap) then
    exit;
  // 8 bits per pixel
  ABitmap.PixelFormat := pf8bit;

  // Create a gradient palette between foreground and background color
  GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 256);
  try
    pal.palVersion := $300;
    pal.palNumEntries := 256;
    for i := 0 to 255 do
    begin
      pal.palPalEntry[i].peRed   := APalette[i].R;
      pal.palPalEntry[i].peGreen := APalette[i].G;
      pal.palPalEntry[i].peBlue  := APalette[i].B;
    end;
    hpal := CreatePalette(pal^);
    if hpal <> 0 then
      ABitmap.Palette := hpal;
  finally
    FreeMem(pal);
  end;
end;

procedure SetBitmap8bitGrayscale(Bitmap: TBitmap);
var
  i: integer;
  Pal: T8bitPaletteArray;
begin
  // Create grayscale palette
  for i := 0 to 255 do
  begin
    Pal[i].R := i;
    Pal[i].G := i;
    Pal[i].B := i;
  end;
  // Set it for the bitmap
  SetBitmap8bitPalette(Pal, Bitmap);
end;

{ TsdColorTransform }

constructor TsdColorTransform.Create;
// We need a virtual constructor so it can be overridden
begin
  inherited Create;
end;

{ TsdNullTransform8bit }

procedure TsdNullTransform8bit.Transform(Source, Dest: pointer; Count: integer);
begin
  Move(Source^, Dest^, Count);
end;

{ TsdNullTransform16bit }

procedure TsdNullTransform16bit.Transform(Source, Dest: pointer; Count: integer);
begin
  Move(Source^, Dest^, Count * 2);
end;

{ TsdNullTransform24bit }

procedure TsdNullTransform24bit.Transform(Source, Dest: pointer; Count: integer);
begin
  Move(Source^, Dest^, Count * 3);
end;

{ TsdNullTransform32bit }

procedure TsdNullTransform32bit.Transform(Source, Dest: pointer; Count: integer);
begin
  Move(Source^, Dest^, Count * 4);
end;

{ TsdTransformInverseGray8bit }

procedure TsdTransformInverseGray8bit.Transform(Source, Dest: pointer; Count: integer);
var
  G, IG: PByte;
begin
  G := Source;
  IG := Dest;
  while Count > 0 do
  begin
    IG^ := G^ xor $FF;
    inc(G);
    inc(IG);
    dec(Count);
  end;
end;

{ TsdTransformInvertTriplet24bit }

procedure TsdTransformInvertTriplet24bit.Transform(Source, Dest: pointer; Count: integer);
var
  T: byte;
  X1S, X2S, X3S: PByte;
  X1D, X2D, X3D: PByte;
begin
  // Source pointers straightforward
  X1S := Source;
  X2S := Source; inc(X2S);
  X3S := Source; inc(X3S, 2);
  // Dest pointers layed out inverted
  X3D := Dest;
  X2D := Dest; inc(X2D);
  X1D := Dest; inc(X1D, 2);

  // Check if Src = Dst
  if Source = Dest then
  begin

    // Repeat Count times
    while Count > 0 do
    begin
      T    := X1S^;
      X1S^ := X3S^;
      X3S^ := T;

      inc(X1S, 3); inc(X3S, 3);
      dec(Count);
    end;

  end else
  begin

    // Repeat Count times
    while Count > 0 do
    begin
      X1D^ := X1S^;
      X2D^ := X2S^;
      X3D^ := X3S^;
      inc(X1S, 3); inc(X2S, 3); inc(X3S, 3);
      inc(X1D, 3); inc(X2D, 3); inc(X3D, 3);
      dec(Count);
    end;

  end;
end;

{ YCbCr to RGB conversion: These constants come from JFIF spec

  R = Y                      + 1.402 (Cr-128)
  G = Y - 0.34414 (Cb-128) - 0.71414 (Cr-128)
  B = Y + 1.772 (Cb-128)

  or

  R = Y                + 1.402 Cr - 179.456
  G = Y - 0.34414 Cb - 0.71414 Cr + 135.53664
  B = Y +   1.772 Cb              - 226.816
}
const
  cColorConvScale = 1 shl 10;
  c__toR = Round(-179.456   * cColorConvScale);
  c__toG = Round( 135.53664 * cColorConvScale);
  c__toB = Round(-226.816   * cColorConvScale);

var
  cY_toRT: array[0..255] of integer;
  cCrtoRT: array[0..255] of integer;
  cCbtoGT: array[0..255] of integer;
  cCrtoGT: array[0..255] of integer;
  cCbtoBT: array[0..255] of integer;

procedure InitYCbCrToRGBTables;
var
  i: integer;
begin
  for i := 0 to 255 do
  begin
    cY_toRT[i] := Round(  1       * cColorConvScale * i);
    cCrtoRT[i] := Round(  1.402   * cColorConvScale * i);
    cCbtoGT[i] := Round( -0.34414 * cColorConvScale * i);
    cCrtoGT[i] := Round( -0.71414 * cColorConvScale * i);
    cCbtoBT[i] := Round(  1.772   * cColorConvScale * i);
  end;
end;

{ TsdTransformYCbCrToRGB }

function RangeLimitDescale(A: integer): integer;
begin
  Result := A div cColorConvScale;
  if Result < 0 then
    Result := 0
  else
    if Result > 255 then
      Result := 255;
end;

procedure TsdTransformYCbCrToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Yi, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := cY_toRT[Y^];
    Ri := Yi +                cCrtoRT[Cr^] + c__toR;
    Gi := Yi + cCbToGT[Cb^] + cCrtoGT[Cr^] + c__toG;
    Bi := Yi + cCbtoBT[Cb^]                + c__toB;
    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);
    // Advance pointers
    inc(Y, 3); inc(Cb, 3); inc(Cr, 3);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrToRGBA }

procedure TsdTransformYCbCrToBGRA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Y, Cb, Cr: PByte;
  Yi, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);
  A := Dest; inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := cY_toRT[Y^];
    Ri := Yi +                cCrtoRT[Cr^] + c__toR;
    Gi := Yi + cCbToGT[Cb^] + cCrtoGT[Cr^] + c__toG;
    Bi := Yi + cCbtoBT[Cb^]                + c__toB;
    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);
    A^ := $FF;
    // Advance pointers
    inc(Y, 3); inc(Cb, 3); inc(Cr, 3);
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrToGray }

procedure TsdTransformYCbCrToGray.Transform(Source, Dest: pointer; Count: integer);
var
  G, Y: PByte;
begin
  Y  := Source;
  G := Dest;
  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    G^ := Y^;
    // Advance pointers
    inc(Y, 3);
    inc(G);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrAToRGB }

procedure TsdTransformYCbCrAToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Yi, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := cY_toRT[Y^];
    Ri := Yi +                cCrtoRT[Cr^] + c__toR;
    Gi := Yi + cCbToGT[Cb^] + cCrtoGT[Cr^] + c__toG;
    Bi := Yi + cCbtoBT[Cb^]                + c__toB;
    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);
    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrAToRGBA }

procedure TsdTransformYCbCrAToBGRA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Y, Cb, Cr, YA: PByte;
  Yi, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);
  YA := Source; inc(YA, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);
  A := Dest; inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := cY_toRT[Y^];
    Ri := Yi +                cCrtoRT[Cr^] + c__toR;
    Gi := Yi + cCbToGT[Cb^] + cCrtoGT[Cr^] + c__toG;
    Bi := Yi + cCbtoBT[Cb^]                + c__toB;
    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);
    A^ := YA^;
    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(YA, 4);
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    dec(Count);
  end;
end;

{ TsdTransformYCbCrKToRGB }

procedure TsdTransformYCbCrKToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr, K: PByte;
  Ci, Mi, Yi, Ki, Ii, Ri, Gi, Bi: integer;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);
  K  := Source; inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ii := cY_toRT[Y^];

    Ci := Ii +                cCrtoRT[Cr^] + c__toR;// cyan
    Mi := Ii + cCbToGT[Cb^] + cCrtoGT[Cr^] + c__toG;// magenta
    Yi := Ii + cCbtoBT[Cb^]                + c__toB;// yellow
    Ki := 255 * cColorConvScale - cY_toRT[K^];      // black

    // In YCbCrK, the CMYK values must be converted to produce RGB
    // Do the conversion in int
    Ri := 255 * cColorConvScale - Ci - Ki;
    Gi := 255 * cColorConvScale - Mi - Ki;
    Bi := 255 * cColorConvScale - Yi - Ki;

    R^ := RangeLimitDescale(Ri);
    G^ := RangeLimitDescale(Gi);
    B^ := RangeLimitDescale(Bi);

    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformCMYKToRGB }

procedure TsdTransformCMYKToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, C, M, Y, K: PByte;
  Ri, Gi, Bi: integer;
  function RangeLimit(A: integer): integer;
  begin
    Result := A;
    if Result < 0 then
      Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;
begin
  C := Source;
  M := Source; inc(M);
  Y := Source; inc(Y, 2);
  K := Source; inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ri := 255 - C^ - K^;
    Gi := 255 - M^ - K^;
    Bi := 255 - Y^ - K^;
    R^ := RangeLimit(Ri);
    G^ := RangeLimit(Gi);
    B^ := RangeLimit(Bi);
    // Advance pointers
    inc(C, 4); inc(M, 4); inc(Y, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformCMYKToBGR_Adobe }

procedure TsdTransformCMYKToBGR_Adobe.Transform(Source, Dest: pointer; Count: integer);
// When all in range [0..1]
//    CMY -> CMYK                         | CMYK -> CMY
//    Black=minimum(Cyan,Magenta,Yellow)  | Cyan=minimum(1,Cyan*(1-Black)+Black)
//    Cyan=(Cyan-Black)/(1-Black)         | Magenta=minimum(1,Magenta*(1-Black)+Black)
//    Magenta=(Magenta-Black)/(1-Black)   | Yellow=minimum(1,Yellow*(1-Black)+Black)
//    Yellow=(Yellow-Black)/(1-Black)     |
//    RGB -> CMYK                         | CMYK -> RGB
//    Black=minimum(1-Red,1-Green,1-Blue) | Red=1-minimum(1,Cyan*(1-Black)+Black)
//    Cyan=(1-Red-Black)/(1-Black)        | Green=1-minimum(1,Magenta*(1-Black)+Black)
//    Magenta=(1-Green-Black)/(1-Black)   | Blue=1-minimum(1,Yellow*(1-Black)+Black)
//    Yellow=(1-Blue-Black)/(1-Black)     |
var
  R, G, B, C, M, Y, K: PByte;
  Ck, Mk, Yk, Cu, Mu, Yu, Ku: integer;
  Ri, Gi, Bi: integer;
  function RangeLimit(A: integer): integer;
  begin
    Result := A;
    if Result < 0 then
      Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;
begin
  // CMYK layout
  C := Source;
  M := Source; inc(M);
  Y := Source; inc(Y, 2);
  K := Source; inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Original colour channels are inverted: uninvert them here
    Ku := 255 - K^;
    Cu := 255 - C^;
    Mu := 255 - M^;
    Yu := 255 - Y^;

    // CMYK -> CMY
    Ck := (Cu * K^) div 255;
    Mk := (Mu * K^) div 255;
    Yk := (Yu * K^) div 255;

    //CMY -> RGB
    Ri := 255 - (Ck + Ku);
    Gi := 255 - (Mk + Ku);
    Bi := 255 - (Yk + Ku);

    // Range limit
    R^ := RangeLimit(Ri);
    G^ := RangeLimit(Gi);
    B^ := RangeLimit(Bi);

    // Advance pointers
    inc(C, 4); inc(M, 4); inc(Y, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformYCCKToBGR }

procedure TsdTransformYCCKToBGR.Transform(Source, Dest: pointer; Count: integer);
// YCCK is a colorspace where the CMY part of CMYK is first converted to RGB, then
// transformed to YCbCr as usual. The K part is appended without any changes.
// To transform back, we do the YCbCr -> RGB transform, then add K
var
  R, G, B, Y, Cb, Cr, K: PByte;
  Yi, Cu, Mu, Yu, Ko, Kk: integer;
  function RangeLimit(V: integer): byte;
  begin
    if V < 0 then
      Result := 0
    else
      if V > 255 then
        Result := 255
      else
        Result := V;
  end;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);
  K  := Source; inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := cY_toRT[Y^];
    Ko := K^; // Inverse of K (K seems to be inverted in the file)
    Kk := (255 - Ko) * cColorConvScale; // Real K, with fixed precision

    // YCbCr converted back to CMY part of CMYK
    Cu := (Yi                + cCrtoRT[Cr^] + c__toR); //=original C of CMYK
    Mu := (Yi + cCbToGT[Cb^] + cCrtoGT[Cr^] + c__toG); //=original M of CMYK
    Yu := (Yi + cCbtoBT[Cb^]                + c__toB); //=original Y of CMYK

    // CMYK->RGB
    R^ := RangeLimitDescale(255 * cColorConvScale - (Cu * Ko) div 255 - Kk);
    G^ := RangeLimitDescale(255 * cColorConvScale - (Mu * Ko) div 255 - Kk);
    B^ := RangeLimitDescale(255 * cColorConvScale - (Yu * Ko) div 255 - Kk);

    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformYCCKToBGR_Adobe }

procedure TsdTransformYCCKToBGR_Adobe.Transform(Source, Dest: pointer;
  Count: integer);
// YCCK to RGB for Adobe images is different. First, the Y, Cr and Cb are inverted,
// and k* = 220 - K. The normal YCbCr to RGB is then applied. As a last step,
// the values are scaled by 0.65 around 128
{float k = 220 - K[i], y = 255 - Y[i], cb = 255 - Cb[i], cr = 255 - Cr[i];

double val = y + 1.402 * (cr - 128) - k;
val = (val - 128) * .65f + 128;
R = val < 0.0 ? (byte) 0 : val > 255.0 ? (byte) 0xff : (byte) (val + 0.5);

val = y - 0.34414 * (cb - 128) - 0.71414 * (cr - 128) - k;
val = (val - 128) * .65f + 128;
G = val < 0.0 ? (byte) 0 : val > 255.0 ? (byte) 0xff : (byte) (val + 0.5);

val = y + 1.772 * (cb - 128) - k;
val = (val - 128) * .65f + 128;
B = val < 0.0 ? (byte) 0 : val > 255.0 ? (byte) 0xff : (byte) (val + 0.5);

X* = (X - 128) * 0.65 + 128 <=>
X* = X * 0.65 + 128 - 128 * 0.65 <=>
X* = X * 0.65 + 44.8
}
const
  c0_65: integer = round(0.65 * cColorConvScale);
  c44_8: integer = round(44.8 * cColorConvScale);// 128 - 0.65 * 128
var
  R, G, B, Y, Cb, Cr, K: PByte;
  Yi, Ki, Ri, Gi, Bi, Cbi, Cri: integer;
  function ScaleAndRangeLimit(A: integer): integer;
  begin
    // First the scaling
    A := (A * c0_65) div cColorConvScale + c44_8;
    // Undo fixed precision and range limit
    Result := A div cColorConvScale;
    if Result < 0 then
      Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;
begin
  Y  := Source;
  Cb := Source; inc(Cb);
  Cr := Source; inc(Cr, 2);
  K  := Source; inc(K, 3);
  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);
  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Yi := cY_toRT[255 - Y^];
    Cbi := 255 - Cb^;
    Cri := 255 - Cr^;
    Ki := (220 - K^) * cColorConvScale;
    Ri := Yi                + cCrtoRT[Cri] + c__toR - Ki;
    Gi := Yi + cCbToGT[Cbi] + cCrtoGT[Cri] + c__toG - Ki;
    Bi := Yi + cCbtoBT[Cbi]                + c__toB - Ki;
    R^ := ScaleAndRangeLimit(Ri);
    G^ := ScaleAndRangeLimit(Gi);
    B^ := ScaleAndRangeLimit(Bi);
    // Advance pointers
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(K, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformGrayToBGR }

procedure TsdTransformGrayToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y: PByte;
begin
  Y := Source;

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Y^;
    G^ := Y^;
    B^ := Y^;
    // Advance pointers
    inc(Y, 1);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformGrayAToBGR }

procedure TsdTransformGrayAToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y: PByte;
begin
  Y := Source;

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Y^;
    G^ := Y^;
    B^ := Y^;
    // Advance pointers
    inc(Y, 2);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

{ TsdTransformGrayAToBGRA }

procedure TsdTransformGrayAToBGRA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Y, YA: PByte;
begin
  Y := Source;
  YA := Source; inc(YA);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);
  A := Dest; inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Y^;
    G^ := Y^;
    B^ := Y^;
    A^ := YA^;
    // Advance pointers
    inc(Y, 2); inc(Ya, 2);
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    dec(Count);
  end;
end;

{ TsdTransformRGBToBGRA }

procedure TsdTransformRGBToBGRA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Rs, Gs, Bs: PByte;
begin
  Rs := Source;
  Gs := Source; inc(Gs);
  Bs := Source; inc(Bs, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);
  A := Dest; inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Rs^;
    G^ := Gs^;
    B^ := Bs^;
    A^ := $FF;
    // Advance pointers
    inc(Rs, 3); inc(Gs, 3); inc(Bs, 3);
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    dec(Count);
  end;
end;

{ TsdTransformRGBAToBGR }

procedure TsdTransformRGBAToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Rs, Gs, Bs: PByte;
begin
  Rs := Source;
  Gs := Source; inc(Gs);
  Bs := Source; inc(Bs, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest; inc(G);
  R := Dest; inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    R^ := Rs^;
    G^ := Gs^;
    B^ := Bs^;
    // Advance pointers
    inc(Rs, 4); inc(Gs, 4); inc(Bs, 4);
    inc(R, 3); inc(G, 3); inc(B, 3);
    dec(Count);
  end;
end;

constructor TsdTransformBGRAToBGR.Create;
begin
  inherited;
  FBkColor := $FFFFFFFF;
end;

function TsdTransformBGRAToBGR.GetBkColor: TColor;
begin
  Result := TColor(FBkColor and $00FFFFFF);
end;

procedure TsdTransformBGRAToBGR.SetBkColor(const Value: TColor);
begin
  FBkColor := cardinal(ColorToRGB(Value)) or $FF000000;
end;

procedure TsdTransformBGRAToBGR.Transform(Source, Dest: pointer; Count: integer);
var
  t: integer;
  R, G, B, A, Rd, Gd, Bd: PByte;
  Rb, Gb, Bb: byte;
begin
  // ARGB source is layed out in memory as BGRA
  B := Source;
  G := Source; inc(G);
  R := Source; inc(R, 2);
  A := Source; inc(A, 3);

  // RGB dest is layed out in memory as BGR
  Bd := Dest;
  Gd := Dest; inc(Gd);
  Rd := Dest; inc(Rd, 2);

  // Background colors
  Rb :=  FBkColor and $000000FF;
  Gb := (FBkColor and $0000FF00) shr 8;
  Bb := (FBkColor and $00FF0000) shr 16;

  // Repeat Count times..
  while Count > 0 do
  begin
    if A^ = 0 then
    begin

      // Fully transparent: background color
      Rd^ := Rb;
      Gd^ := Gb;
      Bd^ := Bb;

    end else
    begin
      if A^ = 255 then
      begin

        // Fully opaque: foreground color
        Rd^ := R^;
        Gd^ := G^;
        Bd^ := B^;

      end else
      begin

        // Semi-transparent: "Src over Dst" operator (Porter-Duff),
        // for pre-multiplied colors, unrolled for speed

        t := A^ * Rb + $80;
        Rd^ := R^ + Rb - (t shr 8 + t) shr 8;

        t := A^ * Gb + $80;
        Gd^ := G^ + Gb - (t shr 8 + t) shr 8;

        t := A^ * Bb + $80;
        Bd^ := B^ + Bb - (t shr 8 + t) shr 8;

      end;
    end;

    // Advance pointers
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    inc(Rd, 3); inc(Gd, 3); inc(Bd, 3);
    dec(Count);
  end;
end;

{  RGB to YCbCr conversion: These constants come from JFIF spec

  Y =    0.299  R + 0.587  G + 0.114  B
  Cb = - 0.1687 R - 0.3313 G + 0.5    B + 128
  Cr =   0.5    R - 0.4187 G - 0.0813 B + 128
}

const
  c__toCb = Round(128 * cColorConvScale);
  c__toCr = Round(128 * cColorConvScale);

var
  cRtoY_: array[0..255] of integer;
  cGtoY_: array[0..255] of integer;
  cBtoY_: array[0..255] of integer;
  cRtoCb: array[0..255] of integer;
  cGtoCb: array[0..255] of integer;
  cBtoCb: array[0..255] of integer;
  cRtoCr: array[0..255] of integer;
  cGtoCr: array[0..255] of integer;
  cBtoCr: array[0..255] of integer;

procedure InitRGBToYCbCrTables;
var
  i: integer;
begin
  for i := 0 to 255 do
  begin
    cRtoY_[i] := Round( 0.299  * cColorConvScale * i);
    cGtoY_[i] := Round( 0.587  * cColorConvScale * i);
    cBtoY_[i] := Round( 0.114  * cColorConvScale * i);
    cRtoCb[i] := Round(-0.1687 * cColorConvScale * i);
    cGtoCb[i] := Round(-0.3313 * cColorConvScale * i);
    cBtoCb[i] := Round( 0.5    * cColorConvScale * i);
    cRtoCr[i] := Round( 0.5    * cColorConvScale * i);
    cGtoCr[i] := Round(-0.4187 * cColorConvScale * i);
    cBtoCr[i] := Round(-0.0813 * cColorConvScale * i);
  end;
end;

{ TsdTransformBGRToYCbCr }

procedure TsdTransformBGRToYCbCr.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Ri, Gi, Bi: integer;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source; inc(G);
  R := Source; inc(R, 2);

  Y  := Dest;
  Cb := Dest; inc(Cb);
  Cr := Dest; inc(Cr, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ri := R^;
    Gi := G^;
    Bi := B^;
    Y^  := RangeLimitDescale(cRtoY_[Ri] + cGtoY_[Gi] + cBtoY_[Bi]          );
    Cb^ := RangeLimitDescale(cRtoCb[Ri] + cGtoCb[Gi] + cBtoCb[Bi] + C__toCb);
    Cr^ := RangeLimitDescale(cRtoCr[Ri] + cGtoCr[Gi] + cBtoCr[Bi] + C__toCr);
    // Advance pointers
    inc(R, 3); inc(G, 3); inc(B, 3);
    inc(Y, 3); inc(Cb, 3); inc(Cr, 3);
    dec(Count);
  end;
end;

{ TsdTransformBGRToGray }

procedure TsdTransformBGRToGray.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y: PByte;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source; inc(G);
  R := Source; inc(R, 2);

  Y  := Dest;
  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Y^  := RangeLimitDescale(cRtoY_[R^] + cGtoY_[G^] + cBtoY_[B^]);
    // Advance pointers
    inc(R, 3); inc(G, 3); inc(B, 3);
    inc(Y, 1);
    dec(Count);
  end;
end;

{ TsdTransformBGRAToYCbCrA }

procedure TsdTransformBGRAToYCbCrA.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, A, Y, Cb, Cr, Ay: PByte;
  Ri, Gi, Bi: integer;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source; inc(G);
  R := Source; inc(R, 2);
  A := Source; inc(A, 3);

  Y  := Dest;
  Cb := Dest; inc(Cb);
  Cr := Dest; inc(Cr, 2);
  Ay := Dest; inc(Ay, 3);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ri := R^;
    Gi := G^;
    Bi := B^;
    Y^  := RangeLimitDescale(cRtoY_[Ri] + cGtoY_[Gi] + cBtoY_[Bi]          );
    Cb^ := RangeLimitDescale(cRtoCb[Ri] + cGtoCb[Gi] + cBtoCb[Bi] + C__toCb);
    Cr^ := RangeLimitDescale(cRtoCr[Ri] + cGtoCr[Gi] + cBtoCr[Bi] + C__toCr);
    Ay^ := A^;
    // Advance pointers
    inc(R, 4); inc(G, 4); inc(B, 4); inc(A, 4);
    inc(Y, 4); inc(Cb, 4); inc(Cr, 4); inc(Ay, 4);
    dec(Count);
  end;
end;

{ TsdTransformBGRAToYCbCr }

procedure TsdTransformBGRAToYCbCr.Transform(Source, Dest: pointer; Count: integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Ri, Gi, Bi: integer;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source; inc(G);
  R := Source; inc(R, 2);

  Y  := Dest;
  Cb := Dest; inc(Cb);
  Cr := Dest; inc(Cr, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // Do the conversion in int
    Ri := R^;
    Gi := G^;
    Bi := B^;
    Y^  := RangeLimitDescale(cRtoY_[Ri] + cGtoY_[Gi] + cBtoY_[Bi]          );
    Cb^ := RangeLimitDescale(cRtoCb[Ri] + cGtoCb[Gi] + cBtoCb[Bi] + C__toCb);
    Cr^ := RangeLimitDescale(cRtoCr[Ri] + cGtoCr[Gi] + cBtoCr[Bi] + C__toCr);
    // Advance pointers
    inc(R, 4); inc(G, 4); inc(B, 4);
    inc(Y, 3); inc(Cb, 3); inc(Cr, 3);
    dec(Count);
  end;
end;

{ TsdTransformCIELabToBGR }

constructor TsdTransformCIELabToBGR.Create;
begin
  inherited;
  // White point: Defaults to D65 (as recommended by CCIR XA/11)
  FXw := 0.9505;
  FYw := 1.0;
  FZw := 1.0890;
  // Range
  FAmin := -100;
  FAmax :=  100;
  FBmin := -100;
  FBmax :=  100;
  // Offset
  FAofs := 128;
  FBofs := 128;
end;

procedure TsdTransformCIELabToBGR.Transform(Source, Dest: pointer;
  Count: integer);
var
  Lb, Ab, Bb: PByte;
  Ld, Ad, Bd, L, M, N, X, Y, Z, R, G, B: double;
  Rf, Gf, Bf: PByte;
  // Limit to interval [0..1]
  function Limit(X: double): double;
  begin
    Result := X;
    if Result < 0 then
      Result := 0
    else if Result > 1 then
      Result := 1;
  end;
  function RangeLimitDescale(X: double): integer;
  begin
    Result := round(X * 255);
    if Result < 0 then
      Result := 0
    else if Result > 255 then
      Result := 255;
  end;
  function GFunc(X: double): double;
  // See PDF spec, section 4.5
  begin
    if X >= 6/29 then
      Result := X * X * X
    else
      Result := (108/841) * (X - (4/29));
  end;
  // sRGB gamma function
  function Gamma(X: double): double;
  begin
    if X < 0.0031308 then
      Result := 12.92 * X
    else
      Result := 1.055 * Power(X, 1.0/2.4) - 0.055;
  end;
begin
  // CIE Lab
  Lb := Source;
  Ab := Source; inc(Ab);
  Bb := Source; inc(Bb, 2);

  // RGB is layed out in memory as BGR
  Bf := Dest;
  Gf := Dest; inc(Gf);
  Rf := Dest; inc(Rf, 2);

  // Repeat Count times..
  while Count > 0 do
  begin
    // First stage: adjust range
    Ld := Lb^ * (100/255);

    Ad := Ab^ - FAofs;
{    if Ad < FAmin then
      Ad := FAmin
    else
      if Ad > FAmax then
        Ad := FAmax;}

    Bd := Bb^ - FBofs;
{    if Bd < FBmin then
      Bd := FBmin
    else
      if Bd > FBmax then
        Bd := FBmax;}

    // Second stage: calculate LMN
    L := (Ld + 16) / 116 + Ad / 500;
    M := (Ld + 16) / 116;
    N := (Ld + 16) / 116 - Bd / 200;

    // Third stage: calculate XYZ
    X := FXw * GFunc(L);
    Y := FYw * GFunc(M);
    Z := FZw * GFunc(N);
{   X := Limit(X);
    Y := Limit(Y);
    Z := Limit(Z);}

    // Fourth stage: calculate sRGB:
    // XYZ to RGB matrix for sRGB, D65.. see
    // http://www.brucelindbloom.com/index.html?ColorCalculator.html

    R :=  3.24071   * X +  -1.53726  * Y + -0.498571  * Z;
    G := -0.969258  * X +   1.87599  * Y +  0.0415557 * Z;
    B :=  0.0556352 * X +  -0.203996 * Y +  1.05707   * Z;

    // Correct to sRGB
    R := Gamma(R);
    G := Gamma(G);
    B := Gamma(B);

    // Final stage: convert to RGB and limit
    Rf^ := RangeLimitDescale(R);
    Gf^ := RangeLimitDescale(G);
    Bf^ := RangeLimitDescale(B);

    // Advance pointers
    inc(Lb, 3); inc(Ab, 3); inc(Bb, 3);
    inc(Rf, 3); inc(Gf, 3); inc(Bf, 3);
    dec(Count);
  end;
end;

{ TsdTransformITUCIELabToBGR }

constructor TsdTransformITUCIELabToBGR.Create;
begin
  inherited;
  // Range
{  FAmin := -21760/255;
  FAmax :=  21590/255;
  FBmin := -19200/255;
  FBmax :=  31800/255;}
  // Offset
  FAofs := 128;
  FBofs :=  96;
end;

initialization

  InitYCbCrToRGBTables;
  InitRGBToYCbCrTables;

end.
