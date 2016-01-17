//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegDCT;

interface

uses
  Classes, sdJpegTypes, sdJpegConsts, sdJpegFormat;

type

  TsdJpegDCT = class(TPersistent)
  private
    FQuant: TsdIntArray64;
    FMap: TsdJpegBlockMap;
    FMethod: TsdJpegDCTCodingMethod;
  public
    procedure BuildQuantTableFrom(ATable: TsdQuantizationTable);
    property Map: TsdJpegBlockMap read FMap write FMap;
    property Method: TsdJpegDCTCodingMethod read FMethod write FMethod;
  end;

  // Forward DCT
  TsdJpegFDCT = class(TsdJpegDCT)
  public
    procedure PerformFDCT(ATable: TsdQuantizationTable);
  end;

  // Inverse DCT
  TsdJpegIDCT = class(TsdJpegDCT)
  public
    procedure PerformIDCT;
  end;

  // Method definition for forward DCT on one block of samples
  TFDCTMethod = procedure(const Sample: TsdSampleBlock; out Coef: TsdCoefBlock;
   var Wrksp: TsdIntArray64);

  // Method definition for inverse DCT on one block of coefficients
  TIDCTMethod = procedure(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
   const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);

implementation

uses
  sdJpegDCTFast, sdJpegDCTAccurate;

{ TsdJpegDCT }

procedure TsdJpegDCT.BuildQuantTableFrom(ATable: TsdQuantizationTable);
// we must use the inverse zig-zag
var
  i: integer;
begin
  if (FMethod = dmAccurate) or (FMap.BlockStride < 64) then
  begin
    // Get the quantization values from the table (and undo zigzag)
    for i := 0 to 63 do
      FQuant[cJpegInverseZigZag8x8[i]] := ATable.Quant[i];
    // Premultiply the quantization factors
    for i := 0 to 63 do
      // give correct bit precision
      FQuant[i] := FQuant[i] * cIAccConstScale;
  end else
  begin
    // Get the quantization values from the table (and undo zigzag)
    for i := 0 to 63 do
      FQuant[cJpegInverseZigZag8x8[i]] := ATable.Quant[i];
    // Premultiply the quantization factors
    for i := 0 to 63 do
      // scales are with 14 bits of precision, we only want 9 so divide
      // by 5 bits of precision
      FQuant[i] := (FQuant[i] * cIFastQuantScales[i]) div (1 shl (14 - cIFastConstBits));
  end;
end;

{ TsdJpegFDCT }

procedure TsdJpegFDCT.PerformFDCT(ATable: TsdQuantizationTable);
var
  i, j, k: integer;
  PCoef: PsdCoefBlock;
  PSample: PsdSampleBlock;
  Work: TsdIntArray64;
  FFDctMethod: TFDCTMethod;
  CVal, QVal: SmallInt;
begin
  // Quantization coefficients, unzigzagged
  for i := 0 to 63 do
    // We multiply divisors by 8 because the FDCT will create values that
    // are multiplied by 8 (shl 3) versus what they should be according
    // to theoretical DCT.
    FQuant[cJpegInverseZigZag8x8[i]] := ATable.Quant[i] * 8;

  // Forward DCT method (we always use this one)
  FFDctMethod := ForwardDCTIntAccurate8x8;

  for j := 0 to FMap.VertBlockCount - 1 do
    for i := 0 to FMap.HorzBlockCount - 1 do
    begin
      // DCT the samples into coefficients
      PSample := FMap.GetSamplePointer(i, j);
      PCoef := FMap.GetCoefPointer(i, j);
      FFDctMethod(PSample^, PCoef^, Work);

      // Quantize the coefficients
      for k := 0 to 63 do
      begin
        CVal := PCoef[k];
        QVal := FQuant[k];
        if CVal < 0 then
        begin
          CVal := -CVal;
          inc(CVal, QVal shr 1); // rounding
          if CVal >= QVal then
            CVal := - (CVal div QVal)
          else
            CVal := 0;
        end else
        begin
          inc(CVal, QVal shr 1); // rounding
          if CVal >= QVal then
            CVal := CVal div QVal
          else
            CVal := 0;
        end;
        PCoef[k] := CVal;
      end;
    end;
end;

{ TsdJpegIDCT }

procedure TsdJpegIDCT.PerformIDCT;
var
  i, j: integer;
  PCoef: PsdCoefBlock;
  PSample: PsdSampleBlock;
  Work: TsdIntArray64;
  FIDctMethod: TIDCTMethod;
begin
  // Select method
  FIDctMethod := nil;
  case FMethod of
  dmFast:
    begin
      case FMap.BlockStride of
      64: FIDctMethod := InverseDCTIntFast8x8; // 8x8
      16: FIDctMethod := InverseDCTIntAccurate4x4; // 4x4
       4: FIDctMethod := InverseDCTIntAccurate2x2; // 2x2
       1: FIDctMethod := InverseDCTIntAccurate1x1; // 1x1
      end;
    end;
  dmAccurate:
    begin
      case FMap.BlockStride of
      64: FIDctMethod := InverseDCTIntAccurate8x8; // 8x8
      16: FIDctMethod := InverseDCTIntAccurate4x4; // 4x4
       4: FIDctMethod := InverseDCTIntAccurate2x2; // 2x2
       1: FIDctMethod := InverseDCTIntAccurate1x1; // 1x1
      end;
    end;
  end;

  if not assigned(FIDctMethod) then exit;
  for j := 0 to FMap.VertBlockCount - 1 do
    for i := 0 to FMap.HorzBlockCount - 1 do
    begin
      PCoef := FMap.GetCoefPointer(i, j);
      PSample := FMap.GetSamplePointer(i, j);
      FIDctMethod(PCoef^, PSample^, FQuant, Work);
    end;
end;

end.
