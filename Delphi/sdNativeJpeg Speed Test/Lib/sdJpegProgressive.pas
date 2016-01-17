//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegProgressive;

interface

uses
  Windows, Classes, Graphics, SysUtils, sdJpegFormat, sdJpegBaseline, sdJpegTypes,
  sdJpegHuffman, sdJPegBitstream;

type

  TsdDCProgressiveHuffmanDecoder = class(TsdDCBaselineHuffmanDecoder)
  public
    // Progressive
    procedure DecodeProgFirst(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      ApproxLow: integer);
    procedure DecodeProgRefine(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      ApproxLow: integer);
  end;

  TsdACProgressiveHuffmanDecoder = class(TsdACBaselineHuffmanDecoder)
  public
     // Progressive
    procedure DecodeProgFirst(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
    procedure DecodeProgRefine(var ABlock: TsdMcuBlock; AReader: TsdBitReader;
      var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
 end;

  TsdJpegProgressiveCoder = class(TsdJpegBaselineCoder)
  private
    FEOBRun: integer;
    FIsDCBand: boolean;
    FIsFirst: boolean;
  protected
    procedure DecodeMcu(AMcuX, AMcuY: integer; Skip: boolean); override;
    procedure InitializeDecoderTables; override;
    function BlockstrideForScale(AScale: TsdJpegScale): integer; override;
    procedure HandleRestartInterval(S: TStream; Warn: boolean); override;
  public
    procedure Decode(S: TStream); override;
    procedure Finalize; override;
  end;

implementation

uses
  sdJpegConsts;

{ TsdDCProgressiveHuffmanDecoder }

procedure TsdDCProgressiveHuffmanDecoder.DecodeProgFirst(var ABlock: TsdMcuBlock; AReader: TsdBitReader; ApproxLow: integer);
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

  // Process the S code, it's an index into a category. We find "Code", and correct
  // it with the Pred value (undifferencing)
  Code := 0;
  if S > 0 then
  begin
    // We use the EXTEND function, Figure F12
    Bits := AReader.GetBits(S);
    if Bits < cExtendTest[S] then
      Code := Bits + cExtendOffset[S]
    else
      Code := Bits;
  end;
  inc(Code, ABlock.PPred^);

  // Update image component's predictor
  ABlock.PPred^ := Code;
  // Update block
  ABlock.Values[0] := Code shl ApproxLow;
end;

procedure TsdDCProgressiveHuffmanDecoder.DecodeProgRefine(var ABlock: TsdMcuBlock;
  AReader: TsdBitReader; ApproxLow: integer);
var
  Plus: integer;
  Value: Psmallint;
begin
  Plus := 1 shl ApproxLow;
  Value := @ABlock.Values[0];
  // Update block
  if AReader.GetBits(1) = 1 then
    if Value^ > 0 then
      inc(Value^, Plus)
    else
      dec(Value^, Plus);
end;

{ TsdACProgressiveHuffmanDecoder }

procedure TsdACProgressiveHuffmanDecoder.DecodeProgFirst(var ABlock: TsdMcuBlock;
  AReader: TsdBitReader; var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
var
  k, kz: integer; // Position in zigzag
  Values: PsdCoefBlock;
  RS, R, S: integer; // RS = range,category
  Idx, Len: integer;
  Table1, Table2: PsdHuffLookupTable;
  ThisByte, NextByte: PByte;
begin
  // Part of EOB run? In that case, decrement and exit
  if EOBRun > 0 then
  begin
    dec(EOBRun);
    exit;
  end;
  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  NextByte := AReader.NextByte;
  Values := ABlock.Values;

  // Start of the spectral band
  k := SSStart;

  // Check if we're at the end of the spectral band
  while k <= SSEnd do
  begin

    // Get the RS code. Since its guaranteed to have <= 16 bits we can use
    // this two-step mechanism (without loop)
    Idx := ThisByte^;
    Len := Table1.Len[Idx];
    RS := Table1.Value[Idx];
    if Len = 0 then
    begin
      Idx := NextByte^;
      Table2 := @FLookup[RS];
      Len := 8 + Table2.Len[Idx];
      RS := Table2.Value[Idx];
    end;

    // We already have the code, but need to actually remove the bits from the stream
    AReader.RemoveBits(Len);

    // Split range,category
    R := RS shr 4;
    S := RS and $0F;

    if S <> 0 then
    begin

      // Increment range-coded index
      inc(k, R);

      // Process the S code, it's an index into a category.
      // We use the EXTEND function, Figure F12
      R := AReader.GetBits(S);
      if R < cExtendTest[S] then
        S := R + cExtendOffset[S]
      else
        S := R;

      kz := cJpegInverseZigZag8x8[k];
      if kz > 0 then
        Values[kz] := S shl ApproxLow;

    end else
    begin

      if R = 15 then
      begin

        // 16 sample runlength, no sample setting
        inc(k, 15);

      end else
      begin

        // EOB run
        EOBRun := 1 shl R;
        if R > 0 then
        begin
          R := AReader.GetBits(R);
          inc(EOBRun, R);
        end;
        dec(EOBRun);
        break;

      end;
    end;
    inc(k);
  end;
end;

procedure TsdACProgressiveHuffmanDecoder.DecodeProgRefine(var ABlock: TsdMcuBlock;
  AReader: TsdBitReader; var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
var
  k, kz: integer;
  Values: PsdCoefBlock;
  RS, R, S, Plus: integer; // RS = range,category
  Idx, Len: integer;
  Table1, Table2: PsdHuffLookupTable;
  ThisByte, NextByte: PByte;
begin
  // Prepare some local variables for fast access
  Plus := 1 shl ApproxLow;
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  NextByte := AReader.NextByte;
  Values := ABlock.Values;

  // Start of the spectral band
  k := SSStart;

  // Not part of EOB run?
  if EOBRun = 0 then
  begin

    while k <= SSEnd do
    begin
      // Get the RS code. Since its guaranteed to have <= 16 bits we can use
      // this two-step mechanism (without loop)
      Idx := ThisByte^;
      Len := Table1.Len[Idx];
      RS := Table1.Value[Idx];
      if Len = 0 then
      begin
        Idx := NextByte^;
        Table2 := @FLookup[RS];
        Len := 8 + Table2.Len[Idx];
        RS := Table2.Value[Idx];
      end;

      // We already have the code, but need to actually remove the bits from the stream
      AReader.RemoveBits(Len);

      // Split range,category
      R := RS shr 4;
      S := RS and $0F;

      if (S = 0) and (R < 15) then
      begin
        // EOB run
        EOBRun := 1 shl R;
        if R <> 0 then
        begin
          R := AReader.GetBits(R);
          inc(EOBRun, R);
        end;
        break;
      end;

      if S <> 0 then
      begin
        case AReader.GetBits(1) of
        1: S :=  Plus;
        0: S := -Plus;
        end;
      end;

      // Fill values for remainder
      repeat
        kz := cJpegInverseZigZag8x8[k];
        if Values[kz] <> 0 then
        begin
          if AReader.GetBits(1) = 1 then
          begin
            if Values[kz] > 0 then
              inc(Values[kz], Plus)
            else
              dec(Values[kz], Plus);
          end;
        end else
        begin
          dec(R);
          if R < 0 then break;
        end;
        inc(k);
      until k > SSEnd;

      if k <= SSend then
      begin
        if S <> 0 then
        begin
          kz := cJpegInverseZigZag8x8[k];
          if kz > 0 then
            Values[kz] := S;
        end;
      end;

      // Increment range-coded index
      inc(k);

    end;//while
  end;// EOBRun = 0

  // Deal with EOBRun
  if EOBRun > 0 then
  begin

    while k <= SSEnd do
    begin
      kz := cJpegInverseZigZag8x8[k];
      if Values[kz] <> 0 then
      begin
        if AReader.GetBits(1) = 1 then
        begin
          if Values[kz] > 0 then
            inc(Values[kz], Plus)
          else
            dec(Values[kz], Plus);
        end;
      end;
      inc(k);
    end;

    // decrement the EOB run
    dec(EOBRun);
  end;
end;

{ TsdJpegProgressiveCoder }

function TsdJpegProgressiveCoder.BlockstrideForScale(AScale: TsdJpegScale): integer;
begin
  // Blockstride is *always* 64 for Progressive coding, because the coder depends
  // on AC coefficents being set.
  Result := 64;
end;

procedure TsdJpegProgressiveCoder.Decode(S: TStream);
begin
  // Decide which band (DC or AC) and whether first scan
  FIsDCBand := FInfo.SpectralStart = 0;
  FIsFirst := FInfo.ApproxHigh = 0;
  FEOBRun := 0;

  if FTileMode then
    raise EInvalidGraphic.Create(sCannotUseTileMode);

  // Use the standard decoder, with overridden methods
  inherited;
end;

procedure TsdJpegProgressiveCoder.DecodeMcu(AMcuX, AMcuY: integer; Skip: boolean);
var
  i: integer;
  McuBlock: PsdMCUBlock;
begin
  for i := 0 to FMcuBlockCount - 1 do
  begin
    // The current MCU block
    McuBlock := @FMcu[i];
    // Initialize MCU values pointer
    if FInfo.ScanCount > 1 then
      McuBlock.Values := Maps[McuBlock.MapIdx].GetCoefPointerMCU(AMcuX, AMcuY, McuBlock.BlockIdx)
    else
      McuBlock.Values := Maps[McuBlock.MapIdx].GetCoefPointer(AMcuX, AMcuY);
    // Each MCU block has an index to a DC and AC table, use it to do the decoding
    if FIsDCBand and assigned(FDCCoders[McuBlock.DCTable]) then
      if FIsFirst then
        TsdDCProgressiveHuffmanDecoder(FDCCoders[McuBlock.DCTable]).DecodeProgFirst(McuBlock^,
          FBitReader, FInfo.ApproxLow)
      else
        TsdDCProgressiveHuffmanDecoder(FDCCoders[McuBlock.DCTable]).DecodeProgRefine(McuBlock^,
          FBitReader, FInfo.ApproxLow);
    if not FIsDCBand and assigned(FACCoders[McuBlock.ACTable]) then
      if FIsFirst then
        TsdACProgressiveHuffmanDecoder(FACCoders[McuBlock.ACTable]).DecodeProgFirst(McuBlock^,
          FBitReader, FEOBRun, FInfo.SpectralStart, FInfo.SpectralEnd, FInfo.ApproxLow)
      else
        TsdACProgressiveHuffmanDecoder(FACCoders[McuBlock.ACTable]).DecodeProgRefine(McuBlock^,
          FBitReader, FEOBRun, FInfo.SpectralStart, FInfo.SpectralEnd, FInfo.ApproxLow);
    if FBitReader.HitEndOfStream then exit;
  end;
end;

procedure TsdJpegProgressiveCoder.Finalize;
begin
  CorrectBlockStride;
end;

procedure TsdJpegProgressiveCoder.HandleRestartInterval(S: TStream; Warn: boolean);
begin
  inherited;
  // Reset EOB run
  FEOBRun := 0;
end;

procedure TsdJpegProgressiveCoder.InitializeDecoderTables;
var
  i, j, Idx: integer;
  Scan: TsdScanComponent;
  DC: TsdDCProgressiveHuffmanDecoder;
  AC: TsdACProgressiveHuffmanDecoder;
begin
  // Initialize tables to use (max 4 per AC/DC)
  FDCCoders.Clear;
  FACCoders.Clear;
  Idx := 0;

  // Loop through image components in scan
  for i := 0 to FInfo.ScanCount - 1 do
  begin
    // Scan's i-th image component info
    Scan := FInfo.Scans[i];
    // Create DC and AC decoders for i-th image
    if FIsDCBand
       and not assigned(FDCCoders[Scan.DCTable])
       and (FInfo.DCHuffmanTables[Scan.DCTable].Count > 0) then
    begin
      DC := TsdDCProgressiveHuffmanDecoder.Create;
      FDCCoders[Scan.DCTable] := DC;
      DC.GenerateLookupTables(FInfo.DcHuffmanTables[Scan.DCTable]);
    end;
    if not FIsDCBand
       and not assigned(FACCoders[Scan.ACTable])
       and (FInfo.ACHuffmanTables[Scan.ACTable].Count > 0) then
    begin
      AC := TsdACProgressiveHuffmanDecoder.Create;
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

end.
