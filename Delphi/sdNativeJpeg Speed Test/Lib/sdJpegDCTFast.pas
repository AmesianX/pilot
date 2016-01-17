unit sdJpegDCTFast;

//  Based in part on original code: jidctfst.c
//
//  Copyright (C) 1994-1998, Thomas G. Lane.
//  This file is part of the Independent JPEG Group's software.
//  for more information see www.ijg.org
//
//  This file contains a fast, not so accurate integer implementation of the
//  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
//  must also perform dequantization of the input coefficients.
//
//  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
//  on each row (or vice versa, but it's more convenient to emit a row at
//  a time).  Direct algorithms are also available, but they are much more
//  complex and seem not to be any faster when reduced to code.
//
//  This implementation is based on Arai, Agui, and Nakajima's algorithm for
//  scaled DCT.  Their original paper (Trans. IEICE E-71(11):1095) is in
//  Japanese, but the algorithm is described in the Pennebaker & Mitchell
//  JPEG textbook (see REFERENCES section in file README).  The following code
//  is based directly on figure 4-8 in P&M.
//  While an 8-point DCT cannot be done in less than 11 multiplies, it is
//  possible to arrange the computation so that many of the multiplies are
//  simple scalings of the final outputs.  These multiplies can then be
//  folded into the multiplications or divisions by the JPEG quantization
//  table entries.  The AA&N method leaves only 5 multiplies and 29 adds
//  to be done in the DCT itself.
//  The primary disadvantage of this method is that with fixed-point math,
//  accuracy is lost due to imprecise representation of the scaled
//  quantization values.  The smaller the quantization table entry, the less
//  precise the scaled value, so this implementation does worse with high-
//  quality-setting files than with low-quality ones.

//  Delphi translation: (c) 2007 by Nils Haeck M.Sc. (www.simdesign.nl)
//  Changes:
//  - We use 9 bits of precision in the integer arithmetic.
//

interface

uses
  Windows, sdJpegDCT, sdJpegTypes, sdJpegConsts;

procedure InverseDCTIntFast8x8(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);

const

  // For AA&N IDCT method, multipliers are equal to quantization
  // coefficients scaled by scalefactor[row]*scalefactor[col], where
  //    scalefactor[0] := 1
  //    scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7
  // To get integer precision, the multiplier table is scaled by 14 bits
  cIFastQuantScales : array[0..63] of integer =
    ({ precomputed values scaled up by 14 bits }
     16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
     22725, 31521, 29692, 26722, 22725, 17855, 12299,  6270,
     21407, 29692, 27969, 25172, 21407, 16819, 11585,  5906,
     19266, 26722, 25172, 22654, 19266, 15137, 10426,  5315,
     16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
     12873, 17855, 16819, 15137, 12873, 10114,  6967,  3552,
      8867, 12299, 11585, 10426,  8867,  6967,  4799,  2446,
      4520,  6270,  5906,  5315,  4520,  3552,  2446,  1247);

const
  // we use 9 bits of precision, so must multiply by 2^9
  cIFastConstBits = 9;
  cIFastRangeBits = cIFastConstBits + 3;
  cIFastConstScale = 1 shl cIFastConstBits;

implementation

const

  FIX_1_082392200 = integer(Round(cIFastConstScale * 1.082392200));
  FIX_1_414213562 = integer(Round(cIFastConstScale * 1.414213562));
  FIX_1_847759065 = integer(Round(cIFastConstScale * 1.847759065));
  FIX_2_613125930 = integer(Round(cIFastConstScale * 2.613125930));

// integer multiply with shift arithmetic right
function Multiply(A, B: integer): integer;
begin
  // Delphi seems to convert the "div" here to SAR just fine (D7), so we
  // don't use ASM but plain pascal
  Result := (A * B) div cIFastConstScale;
end;

// Descale and range limit to byte domain. We shift right over
// 13 bits: 10 bits to remove precision, and 3 bits to get rid of the additional
// factor 8 introducted by the IDCT transform.
function RangeLimit(A: integer): integer;
begin
  // Delphi seems to convert the "div" here to SAR just fine (D7), so we
  // don't use ASM but plain pascal
  Result := A div (1 shl cIFastRangeBits) + 128;
  if Result < 0 then
    Result := 0
  else
    if Result > 255 then
      Result := 255;
end;

procedure InverseDCTIntFast8x8(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
var
  i, QIdx: integer;
  dci: integer;
  dcs: byte;
  p0, p1, p2, p3, p4, p5, p6, p7: Psmallint;
  w0, w1, w2, w3, w4, w5, w6, w7: Pinteger;
  s0, s1, s2, s3, s4, s5, s6, s7: Pbyte;
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: integer;
  tmp10, tmp11, tmp12, tmp13: integer;
  z5, z10, z11, z12, z13: integer;
begin
  QIdx := 0;
  // First do the columns
  p0 := @Coef[ 0]; p1 := @Coef[ 8]; p2 := @Coef[16]; p3 := @Coef[24];
  p4 := @Coef[32]; p5 := @Coef[40]; p6 := @Coef[48]; p7 := @Coef[56];
  w0 := @Wrksp[ 0]; w1 := @Wrksp[ 8]; w2 := @Wrksp[16]; w3 := @Wrksp[24];
  w4 := @Wrksp[32]; w5 := @Wrksp[40]; w6 := @Wrksp[48]; w7 := @Wrksp[56];
  for i := 0 to 7 do
  begin
    if (p1^ = 0) and (p2^ = 0) and (p3^ = 0) and (p4^ = 0) and
       (p5^ = 0) and (p6^ = 0) and (p7^ = 0) then
    begin
      dci := p0^ * Quant[QIdx];
      w0^ := dci; w1^ := dci; w2^ := dci; w3^ := dci;
      w4^ := dci; w5^ := dci; w6^ := dci; w7^ := dci;
    end else
    begin
      // Even part

      tmp0 := p0^ * Quant[QIdx     ];
      tmp1 := p2^ * Quant[QIdx + 16];
      tmp2 := p4^ * Quant[QIdx + 32];
      tmp3 := p6^ * Quant[QIdx + 48];

      tmp10 := tmp0 + tmp2;	// phase 3
      tmp11 := tmp0 - tmp2;

      tmp13 := tmp1 + tmp3;	// phases 5-3
      tmp12 := Multiply(tmp1 - tmp3, FIX_1_414213562) - tmp13; // 2*c4

      tmp0 := tmp10 + tmp13;	// phase 2
      tmp3 := tmp10 - tmp13;
      tmp1 := tmp11 + tmp12;
      tmp2 := tmp11 - tmp12;

      // Odd part

      tmp4 := p1^ * Quant[QIdx +  8];
      tmp5 := p3^ * Quant[QIdx + 24];
      tmp6 := p5^ * Quant[QIdx + 40];
      tmp7 := p7^ * Quant[QIdx + 56];

      z13 := tmp6 + tmp5;		// phase 6
      z10 := tmp6 - tmp5;
      z11 := tmp4 + tmp7;
      z12 := tmp4 - tmp7;

      tmp7 := z11 + z13;		// phase 5
      tmp11 := Multiply(z11 - z13, FIX_1_414213562); // 2*c4

      z5    := Multiply(z10 + z12, FIX_1_847759065); // 2*c2
      tmp10 := Multiply(z12, FIX_1_082392200) - z5; // 2*(c2-c6)
      tmp12 := Multiply(z10, - FIX_2_613125930) + z5; // -2*(c2+c6)

      tmp6 := tmp12 - tmp7;	// phase 2
      tmp5 := tmp11 - tmp6;
      tmp4 := tmp10 + tmp5;

      w0^ := tmp0 + tmp7;
      w7^ := tmp0 - tmp7;
      w1^ := tmp1 + tmp6;
      w6^ := tmp1 - tmp6;
      w2^ := tmp2 + tmp5;
      w5^ := tmp2 - tmp5;
      w4^ := tmp3 + tmp4;
      w3^ := tmp3 - tmp4;

    end;
    // Advance block pointers
    inc(p0); inc(p1); inc(p2); inc(p3); inc(p4); inc(p5); inc(p6); inc(p7);
    inc(w0); inc(w1); inc(w2); inc(w3); inc(w4); inc(w5); inc(w6); inc(w7);
    inc(QIdx);
  end;

  // Next do the rows
  w0 := @Wrksp[0]; w1 := @Wrksp[1]; w2 := @Wrksp[2]; w3 := @Wrksp[3];
  w4 := @Wrksp[4]; w5 := @Wrksp[5]; w6 := @Wrksp[6]; w7 := @Wrksp[7];
  s0 := @Sample[0]; s1 := @Sample[1]; s2 := @Sample[2]; s3 := @Sample[3];
  s4 := @Sample[4]; s5 := @Sample[5]; s6 := @Sample[6]; s7 := @Sample[7];
  for i := 0 to 7 do
  begin
    if (w1^ = 0) and (w2^ = 0) and (w3^ = 0) and (w4^ = 0) and
       (w5^ = 0) and (w6^ = 0) and (w7^ = 0) then
    begin
      dcs := RangeLimit(w0^);
      s0^ := dcs; s1^ := dcs; s2^ := dcs; s3^ := dcs;
      s4^ := dcs; s5^ := dcs; s6^ := dcs; s7^ := dcs;
    end else
    begin
    
      // Even part

      tmp10 := w0^ + w4^;
      tmp11 := w0^ - w4^;

      tmp13 := w2^ + w6^;
      tmp12 := Multiply(w2^ - w6^, FIX_1_414213562) - tmp13;

      tmp0 := tmp10 + tmp13;
      tmp3 := tmp10 - tmp13;
      tmp1 := tmp11 + tmp12;
      tmp2 := tmp11 - tmp12;

      // Odd part

      z13 := w5^ + w3^;
      z10 := w5^ - w3^;
      z11 := w1^ + w7^;
      z12 := w1^ - w7^;

      tmp7 := z11 + z13;		// phase 5
      tmp11 := Multiply(z11 - z13, FIX_1_414213562); // 2*c4

      z5    := Multiply(z10 + z12, FIX_1_847759065); // 2*c2
      tmp10 := Multiply(z12, FIX_1_082392200) - z5; // 2*(c2-c6)
      tmp12 := Multiply(z10, - FIX_2_613125930) + z5; // -2*(c2+c6)

      tmp6 := tmp12 - tmp7;	// phase 2
      tmp5 := tmp11 - tmp6;
      tmp4 := tmp10 + tmp5;

      // Final output stage: scale down by a factor of 8 and range-limit

      s0^ := RangeLimit(tmp0 + tmp7);
      s7^ := RangeLimit(tmp0 - tmp7);
      s1^ := RangeLimit(tmp1 + tmp6);
      s6^ := RangeLimit(tmp1 - tmp6);
      s2^ := RangeLimit(tmp2 + tmp5);
      s5^ := RangeLimit(tmp2 - tmp5);
      s4^ := RangeLimit(tmp3 + tmp4);
      s3^ := RangeLimit(tmp3 - tmp4);

    end;
    // Advance block pointers
    inc(s0, 8); inc(s1, 8); inc(s2, 8); inc(s3, 8);
    inc(s4, 8); inc(s5, 8); inc(s6, 8); inc(s7, 8);
    inc(w0, 8); inc(w1, 8); inc(w2, 8); inc(w3, 8);
    inc(w4, 8); inc(w5, 8); inc(w6, 8); inc(w7, 8);
  end;
end;

end.
