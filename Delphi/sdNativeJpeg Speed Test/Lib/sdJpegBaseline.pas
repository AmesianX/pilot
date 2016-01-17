//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegBaseline;

// If defined, we will count bits used for huffman codes and bits (reported through
// debugout).
{.$DEFINE DETAILS}

interface

uses
  Windows, Classes, Contnrs, SysUtils, Graphics, sdJpegFormat, sdJpegTypes,
  sdJpegBitStream, sdMapIterator, sdJpegBlockCoder, sdJpegHuffman, sdJpegConsts,
  sdJpegMarkers;

type

  // Specific Huffman DC baseline decoder
  TsdDCBaselineHuffmanDecoder = class(Tsd8bitHuffmanDecoder)
  private
    {$IFDEF DETAILS}
    FCountCodes: integer;
    FCountBits: integer;
    {$ENDIF}
  public
    procedure DecodeMcuBlock(var ABlock: TsdMcuBlock; AReader: TsdBitReader);
  end;

  // Specific Huffman AC baseline decoder
  TsdACBaselineHuffmanDecoder = class(Tsd8bitHuffmanDecoder)
  private
    {$IFDEF DETAILS}
    FCountCodes: integer;
    FCountBits: integer;
    {$ENDIF}
  public
    procedure DecodeMcuBlock(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      AZigZag: PsdZigZagArray);
    // Special routine for jsDiv8 scale loading, just skipping this data
    procedure DecodeMcuBlockSkip(AReader: TsdBitReader);
  end;

  // Specific Huffman DC baseline encoder
  TsdDCBaselineHuffmanEncoder = class(Tsd8bitHuffmanEncoder)
  public
    procedure EncodeMcuBlock(var ABlock: TsdMcuBlock; AWriter: TsdBitWriter);
  end;

  // Specific Huffman AC baseline encoder
  TsdACBaselineHuffmanEncoder = class(Tsd8bitHuffmanEncoder)
  public
    procedure EncodeMcuBlock(var ABlock: TsdMcuBlock; AWriter: TsdBitWriter);
  end;

  TsdJpegTile = class
  private
    FMcuIndex: integer;
    FStreamPos: int64;
    FBits: dword;
    FBitsLeft: integer;
    FPredictors: array of smallint;
  end;

  TsdJpegTileList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdJpegTile;
  public
    function IndexByMcuIndex(AMcuIndex: integer): integer;
    property Items[Index: integer]: TsdJpegTile read GetItems; default;
  end;

  // The Jpeg Baseline coder implements the baseline huffman DC and AC decoding
  // and encoding
  TsdJpegBaselineCoder = class(TsdJpegBlockCoder)
  private
  protected
    FDCCoders: TsdEntropyCoderList;
    FACCoders: TsdEntropyCoderList;
    FMcu: array of TsdMcuBlock;
    FMcuBlockCount: integer;
    FBitReader: TsdBitReader;
    FBitWriter: TsdBitWriter;
    FMcuIndex: integer;
    FHorzMcuCount, FVertMcuCount: integer;
    FRstIndex: integer;
    FZigZag: PsdZigZagArray;
    FIsDryRun: boolean;
    FTiles: TsdJpegTileList;
    procedure DoMcuBlockCount;
    procedure InitializeDecoderTables; virtual;
    procedure InitializeEncoderTables; virtual;
    procedure DecodeMcu(AMcuX, AMcuY: integer; Skip: boolean); virtual;
    procedure EncodeMcu(AMcuX, AMcuY: integer); virtual;
    procedure ResetDecoder;
    procedure ResetEncoder;
    procedure HandleEndOfStreamError(S: TStream); virtual;
    procedure HandleRestartInterval(S: TStream; Warn: boolean); virtual;
    procedure HandleHitMarkerError(S: TStream); virtual;
    function HandleDNLMarker(AMcuY: integer; S: TStream): boolean; virtual;
    procedure ResizeVerticalMcu(NewVertMcuCount: integer); virtual;
  public
    constructor Create(AInfo: TsdJpegCodingInfo); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Initialize(AScale: TsdJpegScale); override;
    procedure Decode(S: TStream); override;
    procedure DecodeBlock(S: TStream; XStart, YStart, XCount, YCount: integer); override;
    procedure Encode(S: TStream); override;
    procedure EncodeStripstart(S: TStream);
    procedure EncodeStrip(S: TStream);
    procedure EncodeStripClose;
    function CreateDHTMarker: TsdDHTMarker; override;
  end;

  // Same as baseline
  TsdJpegExtendedCoder = class(TsdJpegBlockCoder)
  end;


implementation

uses
  SortedLists;

{ TsdDCBaselineHuffmanDecoder }

procedure TsdDCBaselineHuffmanDecoder.DecodeMcuBlock(var ABlock: TsdMcuBlock;
  AReader: TsdBitReader);
var
  S, Code: smallint; // S = category
  Bits: word;
  Idx, Len: byte;
  Table: PsdHuffLookupTable;
begin
  // Get the S code. Since its guaranteed to have <= 16 bits we can use
  // this two-step mechanism (without loop)
  Idx := AReader.ThisByte^;
  Table := @FLookup[0];
  Len := Table.Len[Idx];
  S := Table.Value[Idx];
  if Len = 0 then
  begin
    Idx := AReader.NextByte^;
    Table := @FLookup[S];
    Len := 8 + Table.Len[Idx];
    S := Table.Value[Idx];
  end;

  // We already have the code, but need to actually remove the bits from the stream
  AReader.RemoveBits(Len);
  {$IFDEF DETAILS}
  inc(FCountCodes, Len);
  {$ENDIF}

  // Process the S code, it's an index into a category. We find "Code", and correct
  // it with the Pred value (undifferencing)
  {$IFDEF DETAILS}
  inc(FCountBits, S);
  {$ENDIF}
  case S of
  0: Code := ABlock.PPred^;
  1:
    begin
      if AReader.GetBits(1) = 1 then
        Code := ABlock.PPred^ + 1
      else
        Code := ABlock.PPred^ - 1;
    end;
  else
    // We use the EXTEND function, Figure F12
    Bits := AReader.GetBits(S);
    if Bits < cExtendTest[S] then
      Code := ABlock.PPred^ + Bits + cExtendOffset[S]
    else
      Code := ABlock.PPred^ + Bits;
  end;//case

  // Update block
  ABlock.Values[0] := Code;

  // Update image component's predictor
  ABlock.PPred^ := Code;
end;

{ TsdACBaselineHuffmanDecoder }

procedure TsdACBaselineHuffmanDecoder.DecodeMcuBlock(var ABlock: TsdMcuBlock;
  AReader: TsdBitReader; AZigZag: PsdZigZagArray);
var
  k, kz: integer; // Position in zigzag
  Values: PsdCoefBlock;
  RS, R, S: integer; // RS = range,category
  Bits, Idx, Len: integer;
  Table1, Table2: PsdHuffLookupTable;
  ThisByte: PByte;
begin
  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  Values := ABlock.Values;
  // DC did k = 0, now we're at k = 1
  k := 1;
  repeat
    // Get the RS code. Since its guaranteed to have <= 16 bits we can use
    // this two-step mechanism (without loop)
    Idx := ThisByte^;
    Len := Table1.Len[Idx];
    RS := Table1.Value[Idx];
    if Len = 0 then
    begin
      Idx := AReader.NextByte^;
      Table2 := @FLookup[RS];
      Len := 8 + Table2.Len[Idx];
      RS := Table2.Value[Idx];
    end;

    // We already have the code, but need to actually remove the bits from the stream
    AReader.RemoveBits(Len);
    {$IFDEF DETAILS}
    inc(FCountCodes, Len);
    {$ENDIF}

    // Split range,category
    R := RS shr 4;
    S := RS and $0F;

    if S = 0 then
      if R = 15 then
      begin
        // 16 sample runlength, no sample setting
        inc(k, 16);
        continue;
      end else
        // All other values except R = 0 are undefined, we take it as to
        // jump out for these too. R=0,S=0 means end of block
        break;

    // Increment range-coded index
    inc(k, R);

    // Process the S code, it's an index into a category.
    // We use the EXTEND function, Figure F12
    Bits := AReader.GetBits(S);
    {$IFDEF DETAILS}
    inc(FCountBits, S);
    {$ENDIF}
    kz := AZigZag[k];
    if kz > 0 then
    begin
      if S = 1 then
      begin
        // Optimized for S = 1 (very often)
        if Bits = 0 then
          Values[kz] := -1
        else
          Values[kz] := 1;
      end else
        // S > 1
        if Bits < cExtendTest[S] then
          Values[kz] := Bits + cExtendOffset[S]
        else
          Values[kz] := Bits;
    end;
    inc(k);
    // Check if we're at the end of the 8x8 zigzagging
  until k > 63;
end;

procedure TsdACBaselineHuffmanDecoder.DecodeMcuBlockSkip(AReader: TsdBitReader);
var
  k: integer; // Position in zigzag
  RS, R, S: integer; // RS = range,category
  Idx, Len: integer;
  Table1, Table2: PsdHuffLookupTable;
  ThisByte: PByte;
begin
  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  // DC did k = 0, now we're at k = 1
  k := 1;
  repeat
    // Get the RS code. Since its guaranteed to have <= 16 bits we can use
    // this two-step mechanism (without loop)
    Idx := ThisByte^;
    Len := Table1.Len[Idx];
    RS := Table1.Value[Idx];
    if Len = 0 then
    begin
      Idx := AReader.NextByte^;
      Table2 := @FLookup[RS];
      Len := 8 + Table2.Len[Idx];
      RS := Table2.Value[Idx];
    end;

    // We already have the code, but need to actually remove the bits from the stream
    AReader.RemoveBits(Len);

    // Split range,category
    R := RS shr 4;
    S := RS and $0F;

    if S = 0 then
      if R = 15 then
      begin
        // 16 sample runlength, no sample setting
        inc(k, 16);
        continue;
      end else
        // All other values except R = 0 are undefined, we take it as to
        // jump out for these too. R=0,S=0 means end of block
        break;

    // Increment range-coded index
    inc(k, R + 1);

    // Process the S code, it's an index into a category.
    // We use the EXTEND function, Figure F12
    AReader.GetBits(S);
    // Check if we're at the end of the 8x8 zigzagging
  until k > 63;
end;

{ TsdDCBaselineHuffmanEncoder }

procedure TsdDCBaselineHuffmanEncoder.EncodeMcuBlock(var ABlock: TsdMcuBlock;
  AWriter: TsdBitWriter);
var
  S, Diff: smallint; // S = category
begin
  Diff := ABlock.Values[0] - ABlock.PPred^;
  ABlock.PPred^ := ABlock.Values[0];

  // count the bits
  S := AWriter.CountBits(Diff);

  // Put S code  + extend
  AWriter.PutCodeExtend(@FCodes[S], Diff, S);
end;

{ TsdACBaselineHuffmanEncoder }

procedure TsdACBaselineHuffmanEncoder.EncodeMcuBlock(var ABlock: TsdMcuBlock;
  AWriter: TsdBitWriter);
var
  k: integer; // Position in zigzag
  Values: PsdCoefBlock;
  RS, R, S, Diff: integer; // RS = range,category
begin
  Values := ABlock.Values;
  R := 0;
  k := 1;
  repeat
    Diff := Values[cJpegInverseZigZag8x8[k]];
    inc(k);
    if Diff = 0 then
    begin
      inc(R);
      continue;
    end;
    while R >= 16 do
    begin
      // Code an RS = $F0
      AWriter.PutCode(@FCodes[$F0]);
      dec(R, 16);
    end;
    // Code the value
    S := AWriter.CountBits(Diff);
    // RS value
    RS := R shl 4 + S;
    R := 0;
    AWriter.PutCodeExtend(@FCodes[RS], Diff, S);
  until k = 64;
  // if we have R > 0 this means we must code end of block
  if R > 0 then
    AWriter.PutCode(@FCodes[$00]);
end;

{ TsdJpegTileList }

function TsdJpegTileList.GetItems(Index: integer): TsdJpegTile;
begin
  Result := Get(Index);
end;

function TsdJpegTileList.IndexByMcuIndex(AMcuIndex: integer): integer;
var
  Min, Max: integer;
begin
  // Find position for insert - binary method
  Min := 0;
  Max := Count;
  while Min < Max do begin
    Result := (Min + Max) div 2;
    case CompareInteger(Items[Result].FMcuIndex, AMcuIndex) of
    -1: Min := Result + 1;
     0: exit;
     1: Max := Result;
    end;
  end;
  Result := Min;
end;

{ TsdJpegBaselineCoder }

procedure TsdJpegBaselineCoder.Clear;
begin
  inherited;
  FDCCoders.Clear;
  FACCoders.Clear;
  FTiles.Clear;
end;

constructor TsdJpegBaselineCoder.Create(AInfo: TsdJpegCodingInfo);
begin
  inherited;
  FDCCoders := TsdEntropyCoderList.Create;
  FACCoders := TsdEntropyCoderList.Create;
  FTiles := TsdJpegTileList.Create;
end;

function TsdJpegBaselineCoder.CreateDHTMarker: TsdDHTMarker;
var
  i: integer;
  C: Tsd8bitHuffmanEncoder;
  Item: PsdDHTMarkerInfo;
  ItemCount: integer;
begin
  Result := TsdDHTMarker.Create(FInfo, mkDHT);
  ItemCount := 0;
  // Loop through the DC tables
  for i := 0 to FDCCoders.Count - 1 do
  begin
    C := FDCCoders[i] as Tsd8bitHuffmanEncoder;
    if assigned(C) then
    begin
      SetLength(Result.FMarkerInfo, ItemCount + 1);
      Item := @Result.FMarkerInfo[ItemCount];
      Item.Tc := 0;
      Item.Th := i;
      inc(ItemCount);
      C.OptimiseHuffmanFromHistogram(Item^);
    end;
  end;
  // Loop through the AC tables
  for i := 0 to FACCoders.Count - 1 do
  begin
    C := FACCoders[i] as Tsd8bitHuffmanEncoder;
    if assigned(C) then
    begin
      SetLength(Result.FMarkerInfo, ItemCount + 1);
      Item := @Result.FMarkerInfo[ItemCount];
      Item.Tc := 1;
      Item.Th := i;
      inc(ItemCount);
      C.OptimiseHuffmanFromHistogram(Item^);
    end;
  end;
  if ItemCount = 0 then
    FreeAndNil(Result);
end;

procedure TsdJpegBaselineCoder.Decode(S: TStream);
var
  Tile: TsdJpegTile;
  i: integer;
  McuX, McuY: integer;
{$IFDEF DETAILS}
  FCountCodes, FCountBits: int64;
{$ENDIF}
begin
  // Count number of blocks in MCU and number of MCU cycles
  DoMcuBlockCount;

  // Initialize the decoder tables for DC and AC in this scan
  InitializeDecoderTables;

  // Initialize bit reader
  if S is TMemoryStream then
    FBitReader := TsdMemoryBitReader.Create(S)
  else
    FBitReader := TsdStreamBitReader.Create(S);
  try

    FMcuIndex := 0;
    FRstIndex := 0;
    McuX := 0;
    McuY := 0;
    repeat

      if (McuX = 0) and FInfo.WaitForDNL then
        // Check if we have enough size vertically, in case of waiting for DNL marker
        if McuY >= FVertMcuCount then
          // Resize the maps, 16 MCU lines at a time. This 16 is an arbitrary number
          ResizeVerticalMcu(McuY + 16);

      // Tiled loading? Then we create the tile info for each 8 McuX blocks
      if FTileMode and (McuX mod 8 = 0)then
      begin
        Tile := TsdJpegTile.Create;
        Tile.FMcuIndex := FMcuIndex;
        Tile.FStreamPos := FBitReader.StreamPos;
        Tile.FBits := FBitReader.Bits;
        Tile.FBitsLeft := FBitReader.BitsLeft;
        SetLength(Tile.FPredictors, FInfo.Scans.Count);
        for i := 0 to FInfo.Scans.Count - 1 do
          Tile.FPredictors[i] := FInfo.Scans[i].Predictor^;
        FTiles.Add(Tile);
      end;

      // Decode one MCU, skip if tiled loading is in effect
      DecodeMcu(McuX, McuY, FTileMode);
      inc(FMcuIndex);
      inc(McuX);
      if McuX = FHorzMcuCount then
      begin
        McuX := 0;
        inc(McuY);
        if FInfo.WaitForDNL then
          if HandleDNLMarker(McuY, S) then
            Break;
      end;

      // Check for errors
      if FBitReader.HitEndOfStream then
        HandleEndOfStreamError(S);

      // Check for restart interval
      if (FInfo.RestartInterval > 0) and (FMcuIndex mod FInfo.RestartInterval = 0) then
        HandleRestartInterval(S, True);

      // Check for markers
      if FBitReader.HitMarkerNoBitsLeft then
      begin
        HandleHitMarkerError(S);
        McuX := FMcuIndex mod FHorzMcuCount;
        McuY := FMcuIndex div FHorzMcuCount;
      end;

    until not FInfo.WaitForDNL and (McuY = FVertMcuCount);

    // For good measure we add one more tile if in tilemode (without any data though)
    if FTileMode then
    begin
      Tile := TsdJpegTile.Create;
      Tile.FMcuIndex := FMcuIndex;
      FTiles.Add(Tile);
    end;

    ResetDecoder;

    {$IFDEF DETAILS}
    FCountCodes := 0;
    FCountBits := 0;
    for i := 0 to FDCCoders.Count - 1 do
    begin
      inc(FCountCodes, TsdDCBaselineHuffmanDecoder(FDCCoders[i]).FCountCodes);
      inc(FCountBits,  TsdDCBaselineHuffmanDecoder(FDCCoders[i]).FCountBits);
    end;
    for i := 0 to FACCoders.Count - 1 do
    begin
      inc(FCountCodes, TsdACBaselineHuffmanDecoder(FACCoders[i]).FCountCodes);
      inc(FCountBits , TsdACBaselineHuffmanDecoder(FACCoders[i]).FCountBits);
    end;
    // Report
    DoDebugOut(Format('Codes bitcout = %d (%3.1f%%)',
      [FCountCodes, FCountCodes * 100/(FCountCodes + FCountBits)]));
    DoDebugOut(Format('Bits  bitcout = %d (%3.1f%%)',
      [FCountBits, FCountBits * 100/(FCountCodes + FCountBits)]));
    {$ENDIF}

  finally
    FreeAndNil(FBitReader);
    FHasCoefs := True;
    FHasSamples := False;
  end;
end;

procedure TsdJpegBaselineCoder.DecodeBlock(S: TStream; XStart, YStart, XCount, YCount: integer);
var
  x, y, i, Idx, McuIdx: integer;
  Tile: TsdJpegTile;
begin
  // Setup maps with this special count
  SetupMaps(True, XCount, YCount);

  // Initialize bit reader
  if S is TMemoryStream then
    FBitReader := TsdMemoryBitReader.Create(S)
  else
    FBitReader := TsdStreamBitReader.Create(S);
  try

    for y := 0 to YCount - 1 do
    begin
      if y + YStart >= FVertMcuCount then
        break;
      FMcuIndex := (y + YStart) * FHorzMcuCount + XStart;

      // Find tile that has equal or smaller mcuindex
      Idx := FTiles.IndexByMcuIndex(FMcuIndex); // index in tilelist
      if Idx = FTiles.Count then
        raise EInvalidGraphic.Create(sRangeErrorInTileLoading);
      if FTiles[Idx].FMcuIndex > FMcuIndex then
        dec(Idx);

      // Position bitreader and reset predictors
      Tile := FTiles[Idx];
      FBitReader.StreamPos := Tile.FStreamPos;
      FBitReader.Bits := Tile.FBits;
      FBitReader.BitsLeft := Tile.FBitsLeft;
      for i := 0 to length(Tile.FPredictors) - 1 do
        FInfo.Scans[i].Predictor^ := Tile.FPredictors[i];

      // Skip preceding mcu's
      McuIdx := Tile.FMcuIndex;
      while McuIdx < FMcuIndex do
      begin
        DecodeMcu(0, 0, True);
        inc(McuIdx);
        if (FInfo.RestartInterval > 0) and (McuIdx mod FInfo.RestartInterval = 0) then
          HandleRestartInterval(S, False);
      end;

      for x := 0 to XCount - 1 do
      begin
        if x + XStart >= FHorzMcuCount then
          break;
        // Now don't skip
        DecodeMcu(x, y, False);
        inc(FMcuIndex);
        // Check for restart interval
        if (FInfo.RestartInterval > 0) and (FMcuIndex mod FInfo.RestartInterval = 0) then
          HandleRestartInterval(S, False);
      end;
    end;

  finally
    FreeAndNil(FBitReader);
    FHasCoefs := True;
    FHasSamples := False;
  end;
end;

procedure TsdJpegBaselineCoder.DecodeMcu(AMcuX, AMcuY: integer; Skip: boolean);
var
  i: integer;
  McuBlock: PsdMCUBlock;
  Dummy: TsdCoefBlock;
begin
  for i := 0 to FMcuBlockCount - 1 do
  begin
    // The current MCU block
    McuBlock := @FMcu[i];
    // Initialize MCU values pointer
    if Skip then
      McuBlock.Values := @Dummy[0]
    else
      McuBlock.Values := Maps[McuBlock.MapIdx].GetCoefPointerMCU(AMcuX, AMcuY, McuBlock.BlockIdx);
    // Each MCU block has an index to a DC and AC table, use it to do the decoding
    TsdDCBaselineHuffmanDecoder(FDCCoders[McuBlock.DCTable]).DecodeMcuBlock(McuBlock^, FBitReader);
    if (FScale = jsDiv8) or Skip then
      TsdACBaselineHuffmanDecoder(FACCoders[McuBlock.ACTable]).DecodeMcuBlockSkip(FBitReader)
    else
      TsdACBaselineHuffmanDecoder(FACCoders[McuBlock.ACTable]).DecodeMcuBlock(McuBlock^, FBitReader, FZigZag);
    if FBitReader.HitEndOfStream then exit;
  end;
end;

destructor TsdJpegBaselineCoder.Destroy;
begin
  FreeAndNil(FDCCoders);
  FreeAndNil(FACCoders);
  FreeAndNil(FTiles);
  inherited;
end;

procedure TsdJpegBaselineCoder.DoMcuBlockCount;
var
  HSize, VSize: integer;
  i: integer;
  Frame: TsdFrameComponent;
begin
  if FInfo.ScanCount = 1 then
  begin
    // Single channel: spec tells there can only be one MCU block
    FMcuBlockCount := 1;
    // calculate # blocks in horz and vert direction
    Frame := FInfo.Frames[FInfo.Scans[0].Component];
    HSize := 8 * FInfo.HorzSamplingMax div Frame.HorzSampling;
    VSize := 8 * FInfo.VertSamplingMax div Frame.VertSampling;
    FHorzMcuCount := (FInfo.Width + HSize - 1) div HSize;
    FVertMcuCount := (FInfo.Height + VSize - 1) div VSize;
  end else
  begin
    // Multi channel
    FHorzMcuCount := FInfo.HorzMcuCount;
    FVertMcuCount := FInfo.VertMcuCount;
    FMcuBlockCount := 0;
    for i := 0 to FInfo.ScanCount - 1 do
      inc(FMcuBlockCount, Maps[FInfo.Scans[i].Component].McuBlockCount(FInfo.ScanCount));
  end;
  SetLength(FMcu, FMcuBlockCount);
end;

procedure TsdJpegBaselineCoder.Encode(S: TStream);
var
  B: byte;
  McuX, McuY: integer;
begin
  FIsDryRun := (S = nil);

  // Count number of blocks in MCU and number of MCU cycles
  DoMcuBlockCount;

  // Initialize the encoder tables for DC and AC in this scan
  InitializeEncoderTables;

  // Initialize bit reader
  if FIsDryRun then
    FBitWriter := TsdDryRunBitWriter.Create(S)
  else
    FBitWriter := TsdBitWriter.Create(S);
  try

    FMcuIndex := 0;
    FRstIndex := 0;
    McuX := 0;
    McuY := 0;
    repeat

      // Encode one MCU
      EncodeMcu(McuX, McuY);
      inc(FMcuIndex);
      inc(McuX);
      if McuX = FHorzMcuCount then
      begin
        McuX := 0;
        inc(McuY);
      end;

      if McuY = FVertMcuCount then break;

      // Check for restart interval
      if (FInfo.RestartInterval > 0) and (FMcuIndex mod FInfo.RestartInterval = 0) then
      begin
        // Restart interval
        ResetEncoder;
        if not FIsDryRun then
        begin
          // write RST
          B := $FF;
          S.Write(B, 1);
          B := (FRstIndex mod 8) + mkRST0;
          S.Write(B, 1);
        end;
        inc(FRstIndex);
      end;

    until (McuY = FVertMcuCount);
    ResetEncoder;

  finally
    FreeAndNil(FBitWriter);
  end;
end;

procedure TsdJpegBaselineCoder.EncodeMcu(AMcuX, AMcuY: integer);
var
  i: integer;
  McuBlock: PsdMCUBlock;
  DC: TsdDCBaselineHuffmanEncoder;
  AC: TsdACBaselineHuffmanEncoder;
begin
  for i := 0 to FMcuBlockCount - 1 do
  begin
    // The current MCU block
    McuBlock := @FMcu[i];
    // Initialize MCU values pointer
    McuBlock.Values := Maps[McuBlock.MapIdx].GetCoefPointerMCU(AMcuX, AMcuY, McuBlock.BlockIdx);
    // Each MCU block has an index to a DC and AC table, use it to do the encoding
    DC := TsdDCBaselineHuffmanEncoder(FDCCoders[McuBlock.DCTable]);
    AC := TsdACBaselineHuffmanEncoder(FACCoders[McuBlock.ACTable]);
    if FIsDryRun then
      TsdDryRunBitWriter(FBitWriter).Histogram := DC.Histogram;
    DC.EncodeMcuBlock(McuBlock^, FBitWriter);
    if FIsDryRun then
      TsdDryRunBitWriter(FBitWriter).Histogram := AC.Histogram;
    AC.EncodeMcuBlock(McuBlock^, FBitWriter);
  end;
end;

procedure TsdJpegBaselineCoder.EncodeStrip(S: TStream);
var
  McuX: integer;
  B: byte;
begin
  McuX := 0;
  repeat

    // Encode one MCU
    EncodeMcu(McuX, 0);
    inc(FMcuIndex);
    inc(McuX);
    if McuX = FHorzMcuCount then
      break;

    // Check for restart interval
    if (FInfo.RestartInterval > 0) and (FMcuIndex mod FInfo.RestartInterval = 0) then
    begin
      // Restart interval
      ResetEncoder;
      // write RST
      B := $FF;
      S.Write(B, 1);
      B := (FRstIndex mod 8) + mkRST0;
       S.Write(B, 1);
      inc(FRstIndex);
    end;

  until False;
end;

procedure TsdJpegBaselineCoder.EncodeStripClose;
begin
  ResetEncoder;
  FreeAndNil(FBitWriter);
end;

procedure TsdJpegBaselineCoder.EncodeStripstart(S: TStream);
begin
  // Setup maps to the size of one strip
  SetupMaps(True, FInfo.HorzMCUCount, 1);

  // Count number of blocks in MCU and number of MCU cycles
  DoMcuBlockCount;

  // Initialize the encoder tables for DC and AC in this scan
  InitializeEncoderTables;

  // Initialize bit reader
  FBitWriter := TsdBitWriter.Create(S);

  FMcuIndex := 0;
  FRstIndex := 0;
end;

function TsdJpegBaselineCoder.HandleDNLMarker(AMcuY: integer; S: TStream): boolean;
var
  ReadBytes: integer;
  B, Tag: byte;
begin
  Result := False;
  if FBitReader.HitMarker then
  begin
    // It should be a DNL marker
    ResetDecoder;
    ReadBytes := S.Read(B, 1);
    if not (ReadBytes = 1) or (B <> $FF) then
      raise EInvalidGraphic.Create(sDNLMarkerExpected);
    S.Read(Tag, 1);
    if Tag <> mkDNL then
      raise EInvalidGraphic.Create(sDNLMarkerExpected);
    FInfo.WaitForDNL := False;
    ResizeVerticalMcu(AMcuY);
    Result := True;
  end;
end;

procedure TsdJpegBaselineCoder.HandleEndOfStreamError(S: TStream);
begin
  // Serious error: there weren't enough bits in the stream
  if FBitReader.HitMarkerNoBitsLeft then
    DoDebugOut(Format('Error: Hit Marker $%s', [IntToHex(FBitReader.MarkerTag, 2)]));
  ResetDecoder;
  DoDebugOut(Format('Error: Premature stream end at position %d', [S.Position]));
  raise EInvalidGraphic.Create(sInputStreamChopped);
end;

procedure TsdJpegBaselineCoder.HandleHitMarkerError(S: TStream);
begin
  case FBitReader.MarkerTag of
  mkRST0..mkRST7:
    begin
      // We found a restart too early, set McuIndex to the correct value
      DoDebugOut(Format('Restart interval %d (too early)', [FRstIndex]));
      inc(FRstIndex);
      FMcuIndex := FRstIndex * FInfo.RestartInterval;
      ResetDecoder;
      S.Seek(2, soFromCurrent);
      FBitReader.Reload;
    end;
  end;//case
end;

procedure TsdJpegBaselineCoder.HandleRestartInterval(S: TStream; Warn: boolean);
// Restart interval
var
  SuperfluousCount, ReadBytes: integer;
  B: byte;
  Tag: byte;
begin
  ResetDecoder;
  // find + skip restart
  SuperfluousCount := 0;
  repeat
    ReadBytes := S.Read(B, 1);
    if B = $FF then
    begin
      S.Read(Tag, 1);
      case Tag of
      mkRST0..mkRST7:
        begin
          // Check restart interval
          if Warn then
            if (Tag - mkRST0) <> (FRstIndex mod 8) then
              DoDebugOut(Format('WARNING: Restart interval error (expected: %d, found: %d)',
                [Tag - mkRST0, FRstIndex mod 8]));
          break;
        end;
      mkEOI:
        begin
          S.Seek(-2, soFromCurrent);
          break;
        end;
      else
        // Any other tag is an error
        if Warn then
          raise EInvalidGraphic.Create(sUnexpectedMarkerInEncodedStream)
        else
          break;
      end;
    end;
    // If we're here, we had superfluous bytes in the stream
    if ReadBytes > 0 then
      inc(SuperfluousCount);
  until ReadBytes = 0;
  if SuperfluousCount > 0 then
    DoDebugOut(Format('WARNING: %d superfluous bytes found at pos %d', [SuperfluousCount, S.Position - SuperfluousCount]));
  inc(FRstIndex);
  FBitReader.Reload;
end;

procedure TsdJpegBaselineCoder.Initialize(AScale: TsdJpegScale);
begin
  inherited;

  // Determine blockstride
  FBlockStride := BlockStrideForScale(Scale);

  // Setup image maps in frame
  if FTileMode then
    // In tilemode, we create maps with zero size, size will be set later
    SetupMaps(True, 0, 0)
  else
    // otherwise we create maps at full-size
    SetupMaps(False, 0, 0);
end;

procedure TsdJpegBaselineCoder.InitializeDecoderTables;
var
  i, j, Idx: integer;
  Scan: TsdScanComponent;
  DC: TsdDCBaselineHuffmanDecoder;
  AC: TsdACBaselineHuffmanDecoder;
begin
  // Zigzag array that is used
  case BlockStride of
  64: FZigZag := @cJpegInverseZigZag8x8;
  16: FZigZag := @cJpegInverseZigZag4x4;
   4: FZigZag := @cJpegInverseZigZag2x2;
   1: FZigZag := @cJpegInverseZigZag1x1;
  end;

  // Initialize used tables
  FDCCoders.Clear;
  FACCoders.Clear;
  Idx := 0;

  // Loop through image components in scan
  for i := 0 to FInfo.ScanCount - 1 do
  begin
    // Scan's i-th image component info
    Scan := FInfo.Scans[i];
    // Create DC and AC decoders for i-th image
    if not assigned(FDCCoders[Scan.DCTable])
       and (FInfo.DCHuffmanTables[Scan.DCTable].Count > 0) then
    begin
      DC := TsdDCBaselineHuffmanDecoder.Create;
      FDCCoders[Scan.DCTable] := DC;
      DC.GenerateLookupTables(FInfo.DcHuffmanTables[Scan.DCTable]);
    end;
    if not assigned(FACCoders[Scan.ACTable])
       and (FInfo.ACHuffmanTables[Scan.ACTable].Count > 0) then
    begin
      AC := TsdACBaselineHuffmanDecoder.Create;
      FACCoders[Scan.ACTable] := AC;
      AC.GenerateLookupTables(FInfo.ACHuffmanTables[Scan.ACTable]);
    end;
    // Assign table numbers to MCU blocks
    for j := 0 to Maps[Scan.Component].McuBlockCount(FInfo.ScanCount) - 1 do
    begin
      FMcu[Idx].DCTable := Scan.DCTable;
      FMcu[Idx].ACTable := Scan.ACTable;
      FMcu[Idx].PPred := Scan.Predictor;
      FMcu[Idx].BlockIdx := j;
      FMcu[Idx].MapIdx := Scan.Component;
      inc(Idx);
    end;
  end;
end;

procedure TsdJpegBaselineCoder.InitializeEncoderTables;
var
  i, j, Idx: integer;
  Scan: TsdScanComponent;
  DC: TsdDCBaselineHuffmanEncoder;
  AC: TsdACBaselineHuffmanEncoder;
begin
  // Initialize used tables
  FDCCoders.Clear;
  FACCoders.Clear;
  Idx := 0;

  // Loop through image components in scan
  for i := 0 to FInfo.ScanCount - 1 do
  begin
    // Scan's i-th image component info
    Scan := FInfo.Scans[i];
    // Create DC and AC decoders for i-th image
    if not assigned(FDCCoders[Scan.DCTable])
       and ((FInfo.DCHuffmanTables[Scan.DCTable].Count > 0) or FIsDryRun) then
    begin
      DC := TsdDCBaselineHuffmanEncoder.Create;
      FDCCoders[Scan.DCTable] := DC;
      DC.GenerateCodeTable(FInfo.DcHuffmanTables[Scan.DCTable]);
    end;
    if not assigned(FACCoders[Scan.ACTable])
       and ((FInfo.ACHuffmanTables[Scan.ACTable].Count > 0) or FIsDryRun) then
    begin
      AC := TsdACBaselineHuffmanEncoder.Create;
      FACCoders[Scan.ACTable] := AC;
      AC.GenerateCodeTable(FInfo.AcHuffmanTables[Scan.ACTable]);
    end;
    // Assign table numbers to MCU blocks
    for j := 0 to Maps[Scan.Component].McuBlockCount(FInfo.ScanCount) - 1 do
    begin
      FMcu[Idx].DCTable := Scan.DCTable;
      FMcu[Idx].ACTable := Scan.ACTable;
      FMcu[Idx].PPred := Scan.Predictor;
      FMcu[Idx].BlockIdx := j;
      FMcu[Idx].MapIdx := Scan.Component;
      inc(Idx);
    end;
  end;
end;

procedure TsdJpegBaselineCoder.ResetDecoder;
var
  i: integer;
begin
  FBitReader.Reset;
  // Also reset the DC PRED values
  for i := 0 to FInfo.ScanCount - 1 do
    FInfo.Scans[i].Predictor^ := 0;
end;

procedure TsdJpegBaselineCoder.ResetEncoder;
var
  i: integer;
begin
  FBitWriter.Restart;
  // Also reset the DC PRED values
  for i := 0 to FInfo.ScanCount - 1 do
    FInfo.Scans[i].Predictor^ := 0;
end;

procedure TsdJpegBaselineCoder.ResizeVerticalMcu(NewVertMcuCount: integer);
var
  i: integer;
  HorzBlockCount, VertBlockCount: integer;
begin
  FVertMcuCount := NewVertMcuCount;
  FInfo.VertMcuCount :=  NewVertMcuCount;

  // Resize maps
  for i := 0 to FInfo.FrameCount - 1 do
  begin
    HorzBlockCount := FInfo.HorzMcuCount * FInfo.Frames[i].HorzSampling;
    VertBlockCount := FInfo.VertMcuCount * FInfo.Frames[i].VertSampling;
    Maps[i].Resize(HorzBlockCount, VertBlockCount);
  end;
end;

end.
