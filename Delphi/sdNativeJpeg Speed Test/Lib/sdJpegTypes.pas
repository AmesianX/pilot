//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegTypes;

interface

uses
  Windows, Classes, SysUtils, Contnrs, Math, sdJpegConsts;

type

  TsdJpegCodingInfo = class;

  TsdJpegScale = (
    jsFull,  // Read the complete image (DC + AC 1..63)
    jsDiv2,  // Read only 1/2 of the image (DC + AC 1..15)
    jsDiv4,  // Read only 1/4 of the image (DC + AC 1..3)
    jsDiv8   // Read only 1/8 of the image (DC only)
  );

  TsdJpegColorSpace = (
    jcAutoDetect,   // Auto-detect the colorspace from the file
    jcGray,         // 1-Channel grayscale
    jcGrayA,        // 1-Channel grayscale with Alpha channel
    jcRGB,          // (standard) RGB
    jcRGBA,         // (standard) RGB with Alpha channel
    jcYCbCr,        // Jpeg Y-Cb-Cr
    jcYCbCrA,       // Jpeg Y-Cb-Cr with Alpha channel
    jcCMYK,         // CMYK
    jcYCbCrK,       // CMYK represented in 4 channels as YCbCrK
    jcYCCK,         // YCCK
    jcPhotoYCC,     // Photo YCC
    jcPhotoYCCA,    // Photo YCCA
    jcITUCieLAB     // ITU G3FAX CieLAB (for use in colour faxes)
  );

  TsdJpegDCTCodingMethod = (
    dmFast,
    dmAccurate
  );

  // Supported encoding methods in this implementation
  TsdJpegEncodingMethod = (
    emUnspecified,
    emBaselineDCT,
    emExtendedDCT,
    emProgressiveDCT
  );

  TsdQuantizationPrecision = (
    qp8bit,
    qp16bit
  );

  TsdJpegQuality = 1..100;

  TsdCoefBlock = array[0..63] of smallint;
  PsdCoefBlock = ^TsdCoefBlock;
  TsdSampleBlock = array[0..63] of byte;
  PsdSampleBlock = ^TsdSampleBlock;

  // Minimum Coded Unit block (MCU)
  TsdMCUBlock = record
    Values: PsdCoefBlock;
    PPred: Psmallint;
    DCTable: integer;
    ACTable: integer;
    BlockIdx: integer;
    MapIdx: integer;
  end;
  PsdMCUBlock = ^TsdMCUBlock;

  // Quantization table specified in DQT marker
  TsdQuantizationTable = class(TPersistent)
  private
    FQuant: array[0..63] of word; // Quantization values Q
    FPrecision: TsdQuantizationPrecision; // P
    function GetQuant(Index: integer): word;
    procedure SetQuant(Index: integer; const Value: word);
  public
    procedure Transpose;
    property Quant[Index: integer]: word read GetQuant write SetQuant;
    property Precision: TsdQuantizationPrecision read FPrecision write FPrecision;
  end;

  TsdQuantizationTableList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdQuantizationTable;
  public
    property Items[Index: integer]: TsdQuantizationTable read GetItems; default;
  end;

  // Lookup table for Huffman decoding. The Huffman code is left-aligned
  // in the table, Len indicates the number of bits to take out of the stream,
  // Value is the associated symbol. If Len = 0, Value indicates an index
  // to a follow-up table (for codelengths > 8)
  TsdHuffLookupTable = record
    Len:   array[0..255] of byte;
    Value: array[0..255] of smallint;
  end;
  PsdHuffLookupTable = ^TsdHuffLookupTable;

  // Used to construct a histogram (frequency count) of encoded huffman symbols
  Tsd8bitHuffmanHistogram = array[0..255] of integer;
  Psd8bitHuffmanHistogram = ^Tsd8bitHuffmanHistogram;

  // Huffman table values + codes specified in DHT marker
  TsdHuffmanCode = record
    L: integer;    // Symbol length
    Code: integer; // Associated huffman code
    V: integer;    // Value for huffman code
  end;
  PsdHuffmanCode = ^TsdHuffmanCode;

  TsdHuffmanTable = class(TPersistent)
  private
    FItems: array of TsdHuffmanCode;
    function GetItems(Index: integer): PsdHuffmanCode;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
  public
    property Items[Index: integer]: PsdHuffmanCode read GetItems; default;
    property Count: integer read GetCount write SetCount;
  end;

  TsdHuffmanTableList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdHuffmanTable;
  public
    property Items[Index: integer]: TsdHuffmanTable read GetItems; default;
  end;

  // Frame component specified in SOF marker
  TsdFrameComponent = class(TPersistent)
  private
    FHorzSampling: Integer; // H
    FVertSampling: Integer; // V
    FComponentID: integer; // C
    FQTable: integer; // Tq
  public
    // Horizontal sampling factor
    property HorzSampling: Integer read FHorzSampling write FHorzSampling;
    // Vertical sampling factor
    property VertSampling: Integer read FVertSampling write FVertSampling;
    // Component identifier (can be ascii)
    property ComponentID: integer read FComponentID write FComponentID;
    // Quantization table destination
    property QTable: integer read FQTable write FQTable;
  end;

  TsdFrameComponentList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdFrameComponent;
  public
    property Items[Index: integer]: TsdFrameComponent read GetItems; default;
  end;

  // Scan component specified in SOS marker
  TsdScanComponent = class(TPersistent)
  private
    FComponent: integer;// Cidx
    FDCTable: integer; // Td
    FACTable: integer; // Ta
    FPredictor: smallint;
    function GetPredictor: PSmallInt;
  public
    // Index into frame components list
    property Component: integer read FComponent write FComponent;
    // DC entropy table destination
    property DCTable: integer read FDCTable write FDCTable;
    // AC entropy table destination
    property ACTable: integer read FACTable write FACTable;
    // Used as predictor in DC coding
    property Predictor: PSmallInt read GetPredictor;
  end;

  TsdScanComponentList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdScanComponent;
  public
    property Items[Index: integer]: TsdScanComponent read GetItems; default;
  end;

  // Collected component data from markers
  TsdJpegCodingInfo = class(TPersistent)
  private
    FDCHuffmanTables: TsdHuffmanTableList;
    FACHuffmanTables: TsdHuffmanTableList;
    FQuantizationTables: TsdQuantizationTableList;
    FFrames: TsdFrameComponentList;
    FScans: TsdScanComponentList;
    FFrameCount: integer; // Nf
    FScanCount: integer; // Ns
    FHorzSamplingMax: integer; // Hmax
    FVertSamplingMax: integer; // Vmax
    FRestartInterval: integer;// Ri, Restart interval is updated after RSI marker
    FWidth: integer; // X
    FHeight: integer; // Y
    FEncodingMethod: TsdJpegEncodingMethod;
    FSamplePrecision: integer; // P
    FSpectralStart: integer; // Ss
    FSpectralEnd: integer; // Se
    FApproxHigh: integer; // Ah
    FApproxLow: integer; // Al
    FMcuWidth: integer;
    FMcuHeight: integer;
    FHorzMcuCount: integer;
    FVertMcuCount: integer;
    FWaitForDNL: boolean;
    FTileWidth: integer;
    FTileHeight: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    // List of frames
    property Frames: TsdFrameComponentList read FFrames;
    // List of scans
    property Scans: TsdScanComponentList read FScans;
    // Number of image components in frame
    property FrameCount: integer read FFrameCount write FFrameCount;
    // Number of image components in scan
    property ScanCount: integer read FScanCount write FScanCount;

    // Repository of tables, are updated after DQT or DHT markers

    property DCHuffmanTables: TsdHuffmanTableList read FDCHuffmanTables;
    property ACHuffmanTables: TsdHuffmanTableList read FACHuffmanTables;
    property QuantizationTables: TsdQuantizationTableList read FQuantizationTables;

    // The following are updated after SOF marker (once)

    // Jpeg encoding method
    property EncodingMethod: TsdJpegEncodingMethod read FEncodingMethod write FEncodingMethod;
    // Sample precision in bits for samples
    property SamplePrecision: integer read FSamplePrecision write FSamplePrecision;
    // Image width
    property Width: integer read FWidth write FWidth;
    // Image height
    property Height: integer read FHeight write FHeight;

    // Restart interval MCU count (0 means disabled), updated after RST marker
    property RestartInterval: integer read FRestartInterval write FRestartInterval;

    // The following are updated after SOS marker (1 or more times)

    // Start of spectral selection
    property SpectralStart: integer read FSpectralStart write FSpectralStart;
    // End of spectral selection
    property SpectralEnd: integer read FSpectralEnd write FSpectralEnd;
    // Succ Approximation high bitpos
    property ApproxHigh: integer read FApproxHigh write FApproxHigh;
    // Succ Approximation low bitpos
    property ApproxLow: integer read FApproxLow write FApproxLow;

    // Values below are calculated after markers are read (not part of markers)

    // Maximum of all H_i in current scan
    property HorzSamplingMax: integer read FHorzSamplingMax write FHorzSamplingMax;
    // Maximum of all V_i in current scan
    property VertSamplingMax: integer read FVertSamplingMax write FVertSamplingMax;
    // Width of the MCU block in pixels
    property McuWidth: integer read FMcuWidth write FMcuWidth;
    // Height of the MCU block in pixels
    property McuHeight: integer read FMcuHeight write FMcuHeight;
    property HorzMcuCount: integer read FHorzMcuCount write FHorzMcuCount;
    property VertMcuCount: integer read FVertMcuCount write FVertMcuCount;
    property WaitForDNL: boolean read FWaitForDNL write FWaitForDNL;
    // Width of a tile in pixels during TileMode
    property TileWidth: integer read FTileWidth write FTileWidth;
    // Height of a tile in pixels during TileMode
    property TileHeight: integer read FTileHeight write FTileHeight;
  end;

  // Holds data for one image component in the frame, provides method
  // to add/extract samples to/from the MCU currently being decoded/encoded
  TsdJpegBlockMap = class(TPersistent)
  private
    FCoef: array of smallint;
    FCoefBackup: array of smallint; // used when adjusting brightness/contrast
    FSample: array of byte;
    FFrame: TsdFrameComponent; // Pointer to frame info
    FHorzBlockCount: integer; // Horizontal block count
    FVertBlockCount: integer; // Vertical block count
    FBlockStride: integer; // number of samples per block
    FScanStride: integer; // width of a scanline
  protected
    procedure CreateMap; virtual;
  public
    procedure SetSize(AHorzMcuCount, AVertMcuCount: integer;
      AFrame: TsdFrameComponent; ABlockStride: integer);
    procedure Resize(AHorzBlockCount, AVertBlockCount: integer);
    procedure ReduceBlockSize(ANewSize: integer);
    // Number of blocks in the MCU belonging to this image
    function McuBlockCount(AScanCount: integer): integer;
    // Total number of blocks in image (size / 8x8)
    function TotalBlockCount: integer;
    function GetCoefPointerMCU(AMcuX, AMcuY, AMcuIdx: integer): pointer;
    function GetCoefPointer(BlockX, BlockY: integer): pointer;
    function GetSamplePointer(BlockX, BlockY: integer): pointer;
    function FirstCoef: pointer;
    function FirstCoefBackup: pointer;
    function HasCoefBackup: boolean;
    procedure MakeCoefBackup;
    procedure ClearCoefBackup;
    procedure SaveRawValues(const AFileName: string);
    property HorzBlockCount: integer read FHorzBlockCount;
    property VertBlockCount: integer read FVertBlockCount;
    property BlockStride: integer read FBlockStride;
    property ScanStride: integer read FScanStride;
    property Frame: TsdFrameComponent read FFrame;
  end;

  TsdBlockMapList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdJpegBlockMap;
  public
    property Items[Index: integer]: TsdJpegBlockMap read GetItems; default;
  end;

  // TsdFastMemStream deals differently with capacity; it increases the
  // capacity two-fold each time, and has a basic capacity of 100Kb
  TsdFastMemStream = class(TStream)
  private
    FMemory: Pointer;
    FPosition: longint;
    FCapacity: longint;
    FSize: longint;
  protected
    procedure SetCapacity(Value: longint);
    procedure SetSize(NewSize: Longint); override;
  public
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Memory: Pointer read FMemory;
    property Size: longint read FSize write SetSize;
  end;

const
  cColorSpaceNames: array[TsdJpegColorSpace] of string =
  ('AutoDetect', 'Gray', 'GrayA', 'RGB', 'RGBA', 'YCbCr', 'YCbCrA',
   'CMYK', 'CMYK as YCbCrK', 'YCCK', 'PhotoYCC', 'PhotoYCCA', 'ITU CieLAB');

implementation

{ TsdQuantizationTable }

function TsdQuantizationTable.GetQuant(Index: integer): word;
begin
  Result := FQuant[Index];
end;

procedure TsdQuantizationTable.SetQuant(Index: integer; const Value: word);
begin
  FQuant[Index] := Value;
end;

procedure TsdQuantizationTable.Transpose;
var
  x, y, i, j: integer;
  Temp: word;
begin
  // transpose indices in table, but we must do this with the forward zigzag
  for y := 0 to 6 do
    for x := y + 1 to 7 do
    begin
      i := cJpegForwardZigZag8x8[x + y * 8];
      j := cJpegForwardZigZag8x8[x * 8 + y];
      Temp := FQuant[i];
      FQuant[i] := FQuant[j];
      FQuant[j] := Temp;
    end;
end;

{ TsdQuantizationTableList }

function TsdQuantizationTableList.GetItems(Index: integer): TsdQuantizationTable;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
  if not assigned(Result) then
  begin
    Result := TsdQuantizationTable.Create;
    Put(Index, Result);
  end;
end;

{ TsdHuffmanTable }

function TsdHuffmanTable.GetCount: integer;
begin
  Result := length(FItems);
end;

function TsdHuffmanTable.GetItems(Index: integer): PsdHuffmanCode;
begin
  Result := @FItems[Index];
end;

procedure TsdHuffmanTable.SetCount(const Value: integer);
begin
  SetLength(FItems, Value);
end;

{ TsdHuffmanTableList }

function TsdHuffmanTableList.GetItems(Index: integer): TsdHuffmanTable;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
  if not assigned(Result) then
  begin
    Result := TsdHuffmanTable.Create;
    Put(Index, Result);
  end;
end;

{ TsdFrameComponentList }

function TsdFrameComponentList.GetItems(Index: integer): TsdFrameComponent;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
  if not assigned(Result) then
  begin
    Result := TsdFrameComponent.Create;
    Put(Index, Result);
  end;
end;

{ TsdScanComponent }

function TsdScanComponent.GetPredictor: PSmallInt;
begin
  Result := @FPredictor;
end;

{ TsdScanComponentList }

function TsdScanComponentList.GetItems(Index: integer): TsdScanComponent;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
  if not assigned(Result) then
  begin
    Result := TsdScanComponent.Create;
    Put(Index, Result);
  end;
end;

{ TsdJpegCodingInfo }

procedure TsdJpegCodingInfo.Clear;
begin
  // Clear all data in Info
  FDCHuffmanTables.Clear;
  FACHuffmanTables.Clear;
  FQuantizationTables.Clear;
  FFrames.Clear;
  FScans.Clear;

  FFrameCount := 0;
  FScanCount := 0;
  FHorzSamplingMax := 0;
  FVertSamplingMax := 0;
  FRestartInterval := 0;
  FWidth := 0;
  FHeight := 0;
  FEncodingMethod := emUnspecified;
  FSamplePrecision := 0;
  FSpectralStart := 0;
  FSpectralEnd := 0;
  FApproxHigh := 0;
  FApproxLow := 0;
  FWaitForDNL := False;
end;

constructor TsdJpegCodingInfo.Create;
begin
  inherited Create;
  FDCHuffmanTables := TsdHuffmanTableList.Create;
  FACHuffmanTables := TsdHuffmanTableList.Create;
  FQuantizationTables := TsdQuantizationTableList.Create;
  FFrames := TsdFrameComponentList.Create;
  FScans := TsdScanComponentList.Create;
end;

destructor TsdJpegCodingInfo.Destroy;
begin
  FreeAndNil(FDCHuffmanTables);
  FreeAndNil(FACHuffmanTables);
  FreeAndNil(FQuantizationTables);
  FreeAndNil(FFrames);
  FreeAndNil(FScans);
  inherited;
end;

{ TsdJpegBlockMap }

procedure TsdJpegBlockMap.ClearCoefBackup;
begin
  SetLength(FCoefBackup, 0);
end;

procedure TsdJpegBlockMap.CreateMap;
var
  Count: integer;
begin
  FScanStride := FHorzBlockCount * FBlockStride;
  Count := FScanStride * FVertBlockCount;
  SetLength(FCoef, Count);
  SetLength(FSample, Count);
  // Clear the coefficients (since the decoder doesn't always reset them to 0)
  if Count > 0 then
    FillChar(FCoef[0], Count * SizeOf(smallint), 0);
  // Clear backup
  ClearCoefBackup;  
end;

function TsdJpegBlockMap.FirstCoef: pointer;
begin
  Result := @FCoef[0];
end;

function TsdJpegBlockMap.FirstCoefBackup: pointer;
begin
  Result := @FCoefBackup[0];
end;

function TsdJpegBlockMap.GetCoefPointer(BlockX, BlockY: integer): pointer;
begin
  Result := @FCoef[BlockX * FBlockStride + BlockY * FScanStride];
end;

function TsdJpegBlockMap.GetCoefPointerMCU(AMcuX, AMcuY, AMcuIdx: integer): pointer;
var
  X, Y: integer;
begin
  X := FFrame.HorzSampling * AMcuX;
  Y := FFrame.VertSampling * AMcuY;
  while AMcuIdx >= FFrame.HorzSampling do
  begin
    inc(Y);
    dec(AMcuIdx, FFrame.HorzSampling);
  end;
  inc(X, AMcuIdx);
  Result := @FCoef[X * FBlockStride + Y * FScanStride];
end;

function TsdJpegBlockMap.GetSamplePointer(BlockX, BlockY: integer): pointer;
begin
  Result := @FSample[BlockX * FBlockStride + BlockY * FScanStride];
end;

function TsdJpegBlockMap.HasCoefBackup: boolean;
begin
  Result := length(FCoefBackup) > 0;
end;

procedure TsdJpegBlockMap.MakeCoefBackup;
var
  Count: integer;
begin
  Count := length(FCoef);
  if Count <= 0 then exit;
  SetLength(FCoefBackup, Count);
  Move(FCoef[0], FCoefBackup[0], Count * SizeOf(smallint));
end;

function TsdJpegBlockMap.McuBlockCount(AScanCount: integer): integer;
begin
  if AScanCount = 1 then
    Result := 1
  else
    Result := FFrame.HorzSampling * FFrame.VertSampling;
end;

procedure TsdJpegBlockMap.ReduceBlockSize(ANewSize: integer);
var
  i, j, Count, Stride: integer;
  Sc, Dc: Psmallint;
  Ss, Ds: Pbyte;
begin
  if FBlockstride <> 64 then exit;

  Count := FHorzBlockCount * FVertBlockCount;
  // coefs
  Sc := @FCoef[0]; Dc := Sc;
  Stride := ANewSize * SizeOf(smallint);
  for i := 0 to Count - 1 do
  begin
    for j := 0 to 7 do
    begin
      if j < ANewSize then
      begin
        Move(Sc^, Dc^, Stride);
        inc(Dc, ANewSize);
      end;
      inc(Sc, 8);
    end;
  end;
  // samples
  Ss := @FSample[0]; Ds := Ss;
  Stride := ANewSize * SizeOf(byte);
  for i := 0 to Count - 1 do
  begin
    for j := 0 to 7 do
    begin
      if j < ANewSize then
      begin
        Move(Ss^, Ds^, Stride);
        inc(Ds, ANewSize);
      end;
      inc(Ss, 8);
    end;
  end;
  FBlockStride := ANewSize * ANewSize;
  Resize(FHorzBlockCount, FVertBlockCount);
end;

procedure TsdJpegBlockMap.Resize(AHorzBlockCount, AVertBlockCount: integer);
var
  Count: integer;
begin
  FHorzBlockCount := AHorzBlockCount;
  FVertBlockCount := AVertBlockCount;
  FScanStride := FHorzBlockCount * FBlockStride;
  Count := FScanStride * FVertBlockCount;
  SetLength(FCoef, Count);
  SetLength(FSample, Count);
  SetLength(FCoefBackup, 0);
end;

procedure TsdJpegBlockMap.SaveRawValues(const AFileName: string);
var
  i, x, y: integer;
  F: TFileStream;
  Block: PsdCoefBlock;
  procedure WriteS(const S: string);
  begin
    F.Write(S[1], length(S));
  end;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  try
    for y := 0 to FVertBlockCount - 1 do
    begin
      WriteS(Format('Line %d:', [y]) + #13#10);
      for x := 0 to FHorzBlockCount - 1 do
      begin
        WriteS(Format(' Block %d:', [x]) + #13#10);
        WriteS(' ');
        Block := GetCoefPointer(x, y);
        for i := 0 to 63 do
          WriteS(IntToStr(Block[i]) + ' ');
        WriteS(#13#10);
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TsdJpegBlockMap.SetSize(AHorzMcuCount, AVertMcuCount: integer;
  AFrame: TsdFrameComponent; ABlockStride: integer);
begin
  FFrame := AFrame;
  FBlockStride := ABlockStride;
  // Determine block dimensions
  FHorzBlockCount := AHorzMcuCount * FFrame.FHorzSampling;
  FVertBlockCount := AVertMcuCount * FFrame.FVertSampling;
  // Assume the data is valid, we can create the map
  CreateMap;
end;

function TsdJpegBlockMap.TotalBlockCount: integer;
begin
  Result := FHorzBlockCount * FVertBlockCount;
end;

{ TsdBlockMapList }

function TsdBlockMapList.GetItems(Index: integer): TsdJpegBlockMap;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
  if not assigned(Result) then
  begin
    Result := TsdJpegBlockMap.Create;
    Put(Index, Result);
  end;
end;

{ TsdFastMemStream }

destructor TsdFastMemStream.Destroy;
begin
  ReallocMem(FMemory, 0);
  inherited;
end;

function TsdFastMemStream.Read(var Buffer; Count: Integer): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      Move(Pointer(Longint(FMemory) + FPosition)^, Buffer, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TsdFastMemStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TsdFastMemStream.SetCapacity(Value: longint);
begin
  FCapacity := 100 * 1024;
  while FCapacity < Value do FCapacity := FCapacity shl 2;
  ReallocMem(FMemory, FCapacity);
end;

procedure TsdFastMemStream.SetSize(NewSize: longint);
var
  OldPosition: Longint;
begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then Seek(0, soFromEnd);
end;

function TsdFastMemStream.Write(const Buffer; Count: Integer): Longint;
var
  NewPos: Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then begin
    NewPos := FPosition + Count;
    if NewPos > 0 then begin
      if NewPos > FSize then begin
        if NewPos > FCapacity then
          SetCapacity(NewPos);
        FSize := NewPos;
      end;
      System.Move(Buffer, Pointer(Longint(FMemory) + FPosition)^, Count);
      FPosition := NewPos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

end.
