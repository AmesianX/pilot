//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegBlockCoder;

interface

uses
  Windows, SysUtils, sdJpegFormat, sdJpegTypes, sdMapIterator, sdJpegColors;

type

  // Common ancestor for blockbased jpeg codecs like baseline and progressive. It
  // contains a list of blockmaps, which contain dct coefficients and raw samples
  // for each frame component in the image. We do not reuse the coefficient memory
  // for the samples, so we can still do operations on the coefficients after
  // doing the IDCT.
  TsdJpegBlockCoder = class(TsdJpegCoder)
  private
    FMaps: TsdBlockMapList;
    FBuffer: array of byte;
    FBufferCellStride: integer;
    FBufferScanStride: integer;
  protected
    FBlockStride: integer;
    procedure CorrectBlockStride;
    function BlockstrideForScale(AScale: TsdJpegScale): integer; virtual;
    procedure GetBlockstrideParams(ABlockstride: integer;
      var ABlockWidth, AMcuWidth, AMcuHeight: integer);
    procedure McuRowFromBuffer(McuY: integer; ABlockWidth: integer);
    procedure McuRowToBuffer(McuY: integer; ABlockWidth: integer);
    procedure SetupMaps(SpecialSize: boolean; AHorzMcuCount, AVertMcuCount: integer);
  public
    constructor Create(AInfo: TsdJpegCodingInfo); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SamplesFromImage(AImage: TsdMapIterator; ATransform: TsdColorTransform); override;
    procedure SamplesToImage(AImage: TsdMapIterator; ATransform: TsdColorTransform); override;
    procedure ForwardDCT; override;
    procedure InverseDCT; override;
    property Maps: TsdBlockMapList read FMaps;
    property BlockStride: integer read FBlockStride;
  end;

implementation

uses
  sdJpegDCT;

{ TsdJpegBlockCoder }

function TsdJpegBlockCoder.BlockstrideForScale(AScale: TsdJpegScale): integer;
begin
  case AScale of
  jsFull: Result := 64;
  jsDiv2: Result := 16;
  jsDiv4: Result := 4;
  jsDiv8: Result := 1;
  else
    Result := 0;
  end;
end;

procedure TsdJpegBlockCoder.Clear;
var
  i: integer;
begin
  inherited;
  // We only clear the backup coefficents, not the data itself, so that any new
  // SetSize may reuse already allocated memory
  for i := 0 to Maps.Count - 1 do
    Maps[i].ClearCoefBackup;
end;

procedure TsdJpegBlockCoder.CorrectBlockStride;
var
  i, NewSize: integer;
begin
  if (FBlockStride = 64) and (FScale <> jsFull) then
  begin
    // We must reduce the map blockstrides first
    NewSize := 0;
    case FScale of
    jsDiv2: NewSize := 4;
    jsDiv4: NewSize := 2;
    jsDiv8: NewSize := 1;
    end;
    for i := 0 to FMaps.Count - 1 do
      FMaps[i].ReduceBlockSize(NewSize);
    FBlockStride := NewSize * NewSize;
  end;
end;

constructor TsdJpegBlockCoder.Create(AInfo: TsdJpegCodingInfo);
begin
  inherited;
  FMaps := TsdBlockMapList.Create;
end;

destructor TsdJpegBlockCoder.Destroy;
begin
  FreeAndNil(FMaps);
  inherited;
end;

procedure TsdJpegBlockCoder.ForwardDCT;
var
  i: integer;
  FDCT: TsdJpegFDCT;
begin
  FDCT := TsdJpegFDCT.Create;
  try
    for i := 0 to FInfo.FrameCount - 1 do
    begin
      FDCT.Map := FMaps[i];
      FDCT.PerformFDCT(FInfo.QuantizationTables[FInfo.Frames[i].QTable]);
    end;
  finally
    FDCT.Free;
  end;
end;

procedure TsdJpegBlockCoder.GetBlockstrideParams(ABlockstride: integer;
  var ABlockWidth, AMcuWidth, AMcuHeight: integer);
begin
  case ABlockStride of
  64:
    begin
      ABlockWidth := 8;
      AMcuWidth := FInfo.McuWidth;
      AMcuHeight := FInfo.McuHeight;
    end;
  16:
    begin
      ABlockWidth := 4;
      AMcuWidth := FInfo.McuWidth div 2;
      AMcuHeight := FInfo.McuHeight div 2;
    end;
   4:
    begin
      ABlockWidth := 2;
      AMcuWidth := FInfo.McuWidth div 4;
      AMcuHeight := FInfo.McuHeight div 4;
    end;
   1:
    begin
      ABlockWidth := 1;
      AMcuWidth := FInfo.McuWidth div 8;
      AMcuHeight := FInfo.McuHeight div 8;
    end;
  else
    ABlockWidth := 0; // avoid warnings
    AMcuWidth := 0;
    AMcuHeight := 0;
  end;
end;

procedure TsdJpegBlockCoder.InverseDCT;
var
  i: integer;
  IDCT: TsdJpegIDCT;
begin
  IDCT := TsdJpegIDCT.Create;
  try
    IDCT.Method := FMethod;
    for i := 0 to FInfo.FrameCount - 1 do
    begin
      IDCT.Map := FMaps[i];
      IDCT.BuildQuantTableFrom(FInfo.QuantizationTables[FInfo.Frames[i].QTable]);
      IDCT.PerformIDCT;
    end;
  finally
    IDCT.Free;
  end;
end;

procedure TsdJpegBlockCoder.McuRowFromBuffer(McuY, ABlockWidth: integer);
var
  i, j, row, col, xblock, yblock, yi, m, V: integer;
  XRepeat, YRepeat, XYArea: integer;
  PixBlockStride: integer;
  Map: TsdJpegBlockMap;
  Frame: TsdFrameComponent;
  PFirst, PScan, PBlock, PPixel, PCopy, PValue: Pbyte;
begin
  PFirst := @FBuffer[0];
  // Loop through all maps
  for m := 0 to FInfo.FrameCount - 1 do
  begin
    // Process Map
    Map := FMaps[m];
    Frame := FInfo.Frames[m];
    PScan := PFirst;
    XRepeat := FInfo.HorzSamplingMax div Frame.HorzSampling;
    YRepeat := FInfo.VertSamplingMax div Frame.VertSampling;
    XYArea := XRepeat * YRepeat;
    PixBlockStride := ABlockWidth * XRepeat * FBufferCellStride;
    // We process VertSampling rows
    for yi := 0 to Frame.VertSampling - 1 do
    begin
      // y is the block row-index into the map
      yblock := McuY * Frame.VertSampling + yi;
      // Reset the block pointer to the start of the scanline
      PBlock := PScan;
      // We process a row of DCT blocks
      for xblock := 0 to Map.HorzBlockCount - 1 do
      begin
        // Pointer to the samples in this block
        PValue := Map.GetSamplePointer(xblock, yblock);
        // Reset the pixel pointer to the start of the block
        PPixel := PBlock;
        // Rows of block
        for row := 0 to ABlockWidth - 1 do
        begin
          // Check for optimized version
          if (XRepeat = 1) and (YRepeat = 1) then
          begin
            // Optimized version for no repeats
            // Columns of block
            for col := 0 to ABlockWidth - 1 do
            begin
              // Copy pixel to value
              PValue^ := PPixel^;
              inc(PPixel, FBufferCellStride);
              inc(PValue);
            end;
          end else
          begin
            // Repeats in at least one direction
            for col := 0 to ABlockWidth - 1 do
            begin
              // Copy pixel(s) to value and average
              V := 0;
              for i := 0 to XRepeat - 1 do
              begin
                inc(V, PPixel^);
                // vertical repeats?
                PCopy := PPixel;
                for j := 1 to YRepeat - 1 do
                begin
                  inc(PCopy, FBufferScanStride);
                  inc(V, PCopy^);
                end;
                inc(PPixel, FBufferCellStride);
              end;
              PValue^ := V div XYArea;
              inc(PValue);
            end;
          end;
          // Go to the next row in the block. Since we ran through the row, we
          // must also undo the blockstride
          inc(PPixel, FBufferScanStride * YRepeat - PixBlockStride);
        end;
        //
        inc(PBlock, PixBlockStride);
      end;
      inc(PScan, FBufferScanStride * ABlockWidth * YRepeat);
    end;
    inc(PFirst);
  end;
end;

procedure TsdJpegBlockCoder.McuRowToBuffer(McuY: integer; ABlockWidth: integer);
var
  i, j, row, col, xblock, yblock, yi, m: integer;
  XRepeat, YRepeat: integer;
  PixBlockStride: integer;
  Map: TsdJpegBlockMap;
  Frame: TsdFrameComponent;
  PFirst, PScan, PBlock, PPixel, PCopy, PValue: Pbyte;
begin
  PFirst := @FBuffer[0];
  // Loop through all maps
  for m := 0 to FInfo.FrameCount - 1 do
  begin
    // Process Map
    Map := FMaps[m];
    Frame := FInfo.Frames[m];
    PScan := PFirst;
    XRepeat := FInfo.HorzSamplingMax div Frame.HorzSampling;
    YRepeat := FInfo.VertSamplingMax div Frame.VertSampling;
    PixBlockStride := ABlockWidth * XRepeat * FBufferCellStride;
    // We process VertSampling rows
    for yi := 0 to Frame.VertSampling - 1 do
    begin
      // y is the block row-index into the map
      yblock := McuY * Frame.VertSampling + yi;
      // Reset the block pointer to the start of the scanline
      PBlock := PScan;
      // We process a row of DCT blocks
      for xblock := 0 to Map.HorzBlockCount - 1 do
      begin
        // Pointer to the samples in this block
        PValue := Map.GetSamplePointer(xblock, yblock);
        // Reset the pixel pointer to the start of the block
        PPixel := PBlock;
        // Rows of block
        for row := 0 to ABlockWidth - 1 do
        begin
          // Check for optimized version
          if (XRepeat = 1) and (YRepeat = 1) then
          begin
            // Optimized version for no repeats
            // Columns of block
            for col := 0 to ABlockWidth - 1 do
            begin
              // Copy value to pixel
              PPixel^ := PValue^;
              inc(PPixel, FBufferCellStride);
              inc(PValue);
            end;
          end else
          begin
            // Repeats in at least one direction
            for col := 0 to ABlockWidth - 1 do
            begin
              // Copy value to pixel(s)
              for i := 0 to XRepeat - 1 do
              begin
                PPixel^ := PValue^;
                // vertical repeats?
                PCopy := PPixel;
                for j := 1 to YRepeat - 1 do
                begin
                  inc(PCopy, FBufferScanStride);
                  PCopy^ := PValue^;
                end;
                inc(PPixel, FBufferCellStride);
              end;
              inc(PValue);
            end;
          end;
          // Go to the next row in the block. Since we ran through the row, we
          // must also undo the blockstride
          inc(PPixel, FBufferScanStride * YRepeat - PixBlockStride);
        end;
        //
        inc(PBlock, PixBlockStride);
      end;
      inc(PScan, FBufferScanStride * ABlockWidth * YRepeat);
    end;
    inc(PFirst);
  end;
end;

procedure TsdJpegBlockCoder.SamplesFromImage(AImage: TsdMapIterator; ATransform: TsdColorTransform);
var
  y, yi, BufPos, HorzCount: integer;
  BlockWidth, McuWidth, McuHeight: integer;
  PImage: Pbyte;
begin
  GetBlockstrideParams(FBlockstride, BlockWidth, McuWidth, McuHeight);
  // Create a buffer of McuHeight scanlines
  HorzCount := FInfo.HorzMcuCount * McuWidth;
  FBufferCellStride := FInfo.FrameCount;
  FBufferScanStride := HorzCount * FBufferCellStride;
  AImage.Method := imColByCol;
  PImage := AImage.First;
  if (FBufferScanStride = 0) or not assigned(PImage) then exit;
  SetLength(FBuffer, FBufferScanStride * McuHeight);
  for y := 0 to FInfo.VertMcuCount - 1 do
  begin
    // Color convert and put data in buffer
    BufPos := 0;
    for yi := 0 to McuHeight - 1 do
    begin
      ATransform.Transform(PImage, @FBuffer[BufPos], AImage.Width);
      PImage := AImage.Next;
      inc(BufPos, FBufferScanStride);
      if not assigned(PImage) then break;
    end;
    // Combine buffer into jpeg sample maps
    McuRowFromBuffer(y, BlockWidth);
    // In case the image is too small (should normally not happen with images
    // dimensioned for this jpeg)
    if not assigned(PImage) then break;
  end;
end;

procedure TsdJpegBlockCoder.SamplesToImage(AImage: TsdMapIterator; ATransform: TsdColorTransform);
var
  y, yi, BufPos, HorzCount: integer;
  BlockWidth, McuWidth, McuHeight: integer;
  PImage: Pbyte;
begin
  GetBlockstrideParams(FBlockstride, BlockWidth, McuWidth, McuHeight);
  // Create a buffer of McuHeight scanlines
  if FTileMode then
    HorzCount := FInfo.TileWidth
  else
    HorzCount := FInfo.HorzMcuCount * McuWidth;
  FBufferCellStride := FInfo.FrameCount;
  FBufferScanStride := HorzCount * FBufferCellStride;
  // We only do the first col 0, thus this iterator loops through all the rows,
  // col 0.
  AImage.Method := imColByCol;
  PImage := AImage.First;
  if (FBufferScanStride = 0) or not assigned(PImage) then exit;
  SetLength(FBuffer, FBufferScanStride * McuHeight);
  for y := 0 to FInfo.VertMcuCount - 1 do
  begin
    // Combine jpeg sample maps into buffer of width x mcu height
    McuRowToBuffer(y, BlockWidth);
    // Color convert and put data in image
    BufPos := 0;
    for yi := 0 to McuHeight - 1 do
    begin
      // Transform one pixel row of colors at the time
      ATransform.Transform(@FBuffer[BufPos], PImage, AImage.Width);
      PImage := AImage.Next;
      inc(BufPos, FBufferScanStride);
      // In case the image is too small (which often is the case for last
      // MCU row)
      if not assigned(PImage) then break;
    end;
    // In case the image is too small (should normally not happen with images
    // dimensioned for this jpeg)
    if not assigned(PImage) then break;
  end;
end;

procedure TsdJpegBlockCoder.SetupMaps(SpecialSize: boolean; AHorzMcuCount, AVertMcuCount: integer);
var
  i, H, V: integer;
begin
  // Calculate Hmax, Vmax
  for i := 0 to FInfo.FrameCount - 1 do
  begin
    H := FInfo.Frames[i].HorzSampling;
    if H > Finfo.HorzSamplingMax then
      FInfo.HorzSamplingMax := H;
    V := FInfo.Frames[i].VertSampling;
    if V > Finfo.VertSamplingMax then
      FInfo.VertSamplingMax := V;
  end;

  // MCU size in pixels
  FInfo.McuWidth := FInfo.HorzSamplingMax * 8;
  FInfo.McuHeight := FInfo.VertSamplingMax * 8;

  // MCU count
  FInfo.HorzMcuCount :=  (FInfo.Width  + FInfo.McuWidth  - 1) div FInfo.McuWidth;
  FInfo.VertMcuCount :=  (FInfo.Height + FInfo.McuHeight - 1) div FInfo.McuHeight;

  // create maps with given counts
  if SpecialSize then
    for i := 0 to FInfo.FrameCount - 1 do
      FMaps[i].SetSize(AHorzMcuCount, AVertMcuCount, FInfo.Frames[i], FBlockStride)
  else
    for i := 0 to FInfo.FrameCount - 1 do
      FMaps[i].SetSize(FInfo.HorzMcuCount, FInfo.VertMcuCount, FInfo.Frames[i], FBlockStride)

end;

end.
