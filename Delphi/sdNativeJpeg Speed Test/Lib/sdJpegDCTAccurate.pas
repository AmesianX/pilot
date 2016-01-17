unit sdJpegDCTAccurate;
//
//  Based in part on original code: jidctint.c
//
//  Copyright (C) 1991-1998, Thomas G. Lane.
//  Modification developed 2002 by Guido Vollbeding
//  This file is part of the Independent JPEG Group's software.
//  for more information see www.ijg.org
//
//  This file contains a slow-but-accurate integer implementation of the
//  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
//  must also perform dequantization of the input coefficients.
//
//  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
//  on each row (or vice versa, but it's more convenient to emit a row at
//  a time).  Direct algorithms are also available, but they are much more
//  complex and seem not to be any faster when reduced to code.
//
//  This implementation is based on an algorithm described in
//    C. Loeffler, A. Ligtenberg and G. Moschytz, "Practical Fast 1-D DCT
//    Algorithms with 11 Multiplications", Proc. Int'l. Conf. on Acoustics,
//    Speech, and Signal Processing 1989 (ICASSP '89), pp. 988-991.
//  The primary algorithm described there uses 11 multiplies and 29 adds.
//  We use their alternate method with 12 multiplies and 32 adds.
//  The advantage of this method is that no data path contains more than one
//  multiplication; this allows a very simple and accurate implementation in
//  scaled fixed-point arithmetic, with a minimal number of shifts.
//
//  We also provide IDCT routines with various output sample block sizes for
//  direct resolution reduction or enlargement and for direct resolving the
//  common 2x1 and 1x2 subsampling cases without additional resampling: NxN
//  (N=1...16), 2NxN, and Nx2N (N=1...8) pixels for one 8x8 input DCT block.
//
//  For N<8 we simply take the corresponding low-frequency coefficients of
//  the 8x8 input DCT block and apply an NxN point IDCT on the sub-block
//  to yield the downscaled outputs.
//  This can be seen as direct low-pass downsampling from the DCT domain
//  point of view rather than the usual spatial domain point of view,
//  yielding significant computational savings and results at least
//  as good as common bilinear (averaging) spatial downsampling.
//
//  For N>8 we apply a partial NxN IDCT on the 8 input coefficients as
//  lower frequencies and higher frequencies assumed to be zero.
//  It turns out that the computational effort is similar to the 8x8 IDCT
//  regarding the output size.
//  Furthermore, the scaling and descaling is the same for all IDCT sizes.
//
//  CAUTION: We rely on the FIX() macro except for the N=1,2,4,8 cases
//  since there would be too many additional constants to pre-calculate.

//  Delphi translation: (c) 2007 by Nils Haeck M.Sc. (www.simdesign.nl)
//  Changes:
//  - This unit also provides the forward DCT int accurate method
//  - The int accurate IDCT is also adapted to make reduced size
//    IDCT methods (4x4, 2x2 and 1x1).
//

interface

uses
  Windows, sdJpegDCT, sdJpegTypes, sdJpegConsts;

procedure ForwardDCTIntAccurate8x8(const Sample: TsdSampleBlock; out Coef: TsdCoefBlock;
  var Wrksp: TsdIntArray64);

procedure InverseDCTIntAccurate8x8(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
procedure InverseDCTIntAccurate4x4(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
procedure InverseDCTIntAccurate2x2(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
procedure InverseDCTIntAccurate1x1(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);

const
  cIAccConstBits = 9;
  cIAccRangeBits = cIAccConstBits + 3;
  // we use 9 bits of precision, so must multiply by 2^9
  cIAccConstScale = 1 shl cIAccConstBits;

  cCenterSample = 128;
  cMaxSample    = 255;

implementation

procedure ForwardDCTIntAccurate8x8(const Sample: TsdSampleBlock; out Coef: TsdCoefBlock;
  var Wrksp: TsdIntArray64);
const
  cConstBits = 13;
  cConstScale = 1 SHL cConstBits;
  cPass1Bits = 2;
const
  // Constants used in forward DCT
  FIX_0_298631336 = Round(cConstScale * 0.298631336);
  FIX_0_390180644 = Round(cConstScale * 0.390180644);
  FIX_0_541196100 = Round(cConstScale * 0.541196100);
  FIX_0_765366865 = Round(cConstScale * 0.765366865);
  FIX_0_899976223 = Round(cConstScale * 0.899976223);
  FIX_1_175875602 = Round(cConstScale * 1.175875602);
  FIX_1_501321110 = Round(cConstScale * 1.501321110);
  FIX_1_847759065 = Round(cConstScale * 1.847759065);
  FIX_1_961570560 = Round(cConstScale * 1.961570560);
  FIX_2_053119869 = Round(cConstScale * 2.053119869);
  FIX_2_562915447 = Round(cConstScale * 2.562915447);
  FIX_3_072711026 = Round(cConstScale * 3.072711026);
var
  i: integer;
  s0, s1, s2, s3, s4, s5, s6, s7: Pbyte;
  p0, p1, p2, p3, p4, p5, p6, p7: Psmallint;
  w0, w1, w2, w3, w4, w5, w6, w7: Pinteger;
  z1, z2, z3, z4, z5, z10, z11, z12, z13: integer;
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp10, tmp11, tmp12, tmp13: integer;
  // local
  function DescaleMin(x: integer): integer;
  begin
    // Delphi seems to convert the "div" here to SAR just fine (D7), so we
    // don't use ASM but plain pascal
    Result := x div (1 shl (cConstBits - cPass1Bits));
  end;
  function DescalePlus(x: integer): integer;
  begin
    // Delphi seems to convert the "div" here to SAR just fine (D7), so we
    // don't use ASM but plain pascal
    Result := x div (1 shl (cConstBits + cPass1Bits));
  end;
  function DescalePass(x: integer): integer;
  begin
    // Delphi seems to convert the "div" here to SAR just fine (D7), so we
    // don't use ASM but plain pascal
    Result := x div (1 shl cPass1Bits);
  end;
// main
begin

  // Pass 1: process rows.
  // Note results are scaled up by sqrt(8) compared to a true DCT;
  // furthermore, we scale the results by 2**PASS1_BITS.
  s0 := @Sample[0]; s1 := @Sample[1]; s2 := @Sample[2]; s3 := @Sample[3];
  s4 := @Sample[4]; s5 := @Sample[5]; s6 := @Sample[6]; s7 := @Sample[7];
  w0 := @Wrksp[0]; w1 := @Wrksp[1]; w2 := @Wrksp[2]; w3 := @Wrksp[3];
  w4 := @Wrksp[4]; w5 := @Wrksp[5]; w6 := @Wrksp[6]; w7 := @Wrksp[7];

  for i := 0 to 7 do
  begin
    // Samples are in range 0..255, but we must put them in range -128..127
    // So if two samples are added, we should substract 2 times the centersample
    // value, and if two samples are substracted, we do not have to correct.
    tmp0 := s0^ + s7^ - 2 * cCenterSample;
    tmp1 := s1^ + s6^ - 2 * cCenterSample;
    tmp2 := s2^ + s5^ - 2 * cCenterSample;
    tmp3 := s3^ + s4^ - 2 * cCenterSample;
    tmp7 := s0^ - s7^;
    tmp6 := s1^ - s6^;
    tmp5 := s2^ - s5^;
    tmp4 := s3^ - s4^;

    // Even part per LL&M figure 1 --- note that published figure is faulty;
    // rotator "sqrt(2)*c1" should be "sqrt(2)*c6".

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    w0^ := (tmp10 + tmp11) shl cPass1Bits;
    w4^ := (tmp10 - tmp11) shl cPass1Bits;

    z1 := (tmp12 + tmp13) * FIX_0_541196100;
    w2^ := DescaleMin(z1 + tmp13 * FIX_0_765366865);
    w6^ := DescaleMin(z1 - tmp12 * FIX_1_847759065);

    // Odd part per figure 8 --- note paper omits factor of sqrt(2).
    // cK represents cos(K*pi/16).
    // i0..i3 in the paper are tmp4..tmp7 here.

    z1 := tmp4 + tmp7;
    z2 := tmp5 + tmp6;
    z3 := tmp4 + tmp6;
    z4 := tmp5 + tmp7;
    z5 := (z3 + z4) * FIX_1_175875602; // sqrt(2) * c3

    tmp4 := tmp4 * FIX_0_298631336; // sqrt(2) * (-c1+c3+c5-c7)
    tmp5 := tmp5 * FIX_2_053119869; // sqrt(2) * ( c1+c3-c5+c7)
    tmp6 := tmp6 * FIX_3_072711026; // sqrt(2) * ( c1+c3+c5-c7)
    tmp7 := tmp7 * FIX_1_501321110; // sqrt(2) * ( c1+c3-c5-c7)
    z1 := - z1 * FIX_0_899976223; // sqrt(2) * (c7-c3)
    z2 := - z2 * FIX_2_562915447; // sqrt(2) * (-c1-c3)
    z3 := - z3 * FIX_1_961570560; // sqrt(2) * (-c3-c5)
    z4 := - z4 * FIX_0_390180644; // sqrt(2) * (c5-c3)

    Inc(z3, z5);
    Inc(z4, z5);

    w7^ := DescaleMin(tmp4 + z1 + z3);
    w5^ := DescaleMin(tmp5 + z2 + z4);
    w3^ := DescaleMin(tmp6 + z2 + z3);
    w1^ := DescaleMin(tmp7 + z1 + z4);

    // Advance block pointers
    inc(s0, 8); inc(s1, 8); inc(s2, 8); inc(s3, 8);
    inc(s4, 8); inc(s5, 8); inc(s6, 8); inc(s7, 8);
    inc(w0, 8); inc(w1, 8); inc(w2, 8); inc(w3, 8);
    inc(w4, 8); inc(w5, 8); inc(w6, 8); inc(w7, 8);
  end;

  // Pass 2: process columns.
  // We remove the PASS1_BITS scaling, but leave the results scaled up
  // by an overall factor of 8.

  p0 := @Coef[ 0]; p1 := @Coef[ 8]; p2 := @Coef[16]; p3 := @Coef[24];
  p4 := @Coef[32]; p5 := @Coef[40]; p6 := @Coef[48]; p7 := @Coef[56];
  w0 := @Wrksp[ 0]; w1 := @Wrksp[ 8]; w2 := @Wrksp[16]; w3 := @Wrksp[24];
  w4 := @Wrksp[32]; w5 := @Wrksp[40]; w6 := @Wrksp[48]; w7 := @Wrksp[56];
  for i := 0 to 7 do
  begin
    tmp0 := w0^ + w7^;
    tmp7 := w0^ - w7^;
    tmp1 := w1^ + w6^;
    tmp6 := w1^ - w6^;
    tmp2 := w2^ + w5^;
    tmp5 := w2^ - w5^;
    tmp3 := w3^ + w4^;
    tmp4 := w3^ - w4^;

    // Even part per LL&M figure 1 --- note that published figure is faulty;
    // rotator "sqrt(2)*c1" should be "sqrt(2)*c6".

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    p0^ := DescalePass(tmp10 + tmp11);
    p4^ := DescalePass(tmp10 - tmp11);

    z1 := (tmp12 + tmp13) * FIX_0_541196100;
    p2^ := DescalePlus(z1 + tmp13 * FIX_0_765366865);
    p6^ := DescalePlus(z1 - tmp12 * FIX_1_847759065);

    // Odd part per figure 8 --- note paper omits factor of sqrt(2).
    // cK represents cos(K*pi/16).
    // i0..i3 in the paper are tmp4..tmp7 here.

    z1 := tmp4 + tmp7;
    z2 := tmp5 + tmp6;
    z3 := tmp4 + tmp6;
    z4 := tmp5 + tmp7;
    z5 := (z3 + z4) * FIX_1_175875602; // sqrt(2) * c3 }

    tmp4 := tmp4 * FIX_0_298631336; // sqrt(2) * (-c1+c3+c5-c7)
    tmp5 := tmp5 * FIX_2_053119869; // sqrt(2) * ( c1+c3-c5+c7)
    tmp6 := tmp6 * FIX_3_072711026; // sqrt(2) * ( c1+c3+c5-c7)
    tmp7 := tmp7 * FIX_1_501321110; // sqrt(2) * ( c1+c3-c5-c7)
    z1 := - z1 * FIX_0_899976223; // sqrt(2) * (c7-c3)
    z2 := - z2 * FIX_2_562915447; // sqrt(2) * (-c1-c3)
    z3 := - z3 * FIX_1_961570560; // sqrt(2) * (-c3-c5)
    z4 := - z4 * FIX_0_390180644; // sqrt(2) * (c5-c3)

    Inc(z3, z5);
    Inc(z4, z5);

    p7^ := DescalePlus(tmp4 + z1 + z3);
    p5^ := DescalePlus(tmp5 + z2 + z4);
    p3^ := DescalePlus(tmp6 + z2 + z3);
    p1^ := DescalePlus(tmp7 + z1 + z4);

    // Advance block pointers
    inc(p0); inc(p1); inc(p2); inc(p3); inc(p4); inc(p5); inc(p6); inc(p7);
    inc(w0); inc(w1); inc(w2); inc(w3); inc(w4); inc(w5); inc(w6); inc(w7);
  end;
end;

const
  // Constants used in Inverse DCT
  FIX_0_298631336 = Round(cIAccConstScale * 0.298631336);
  FIX_0_390180644 = Round(cIAccConstScale * 0.390180644);
  FIX_0_541196100 = Round(cIAccConstScale * 0.541196100);
  FIX_0_765366865 = Round(cIAccConstScale * 0.765366865);
  FIX_0_899976223 = Round(cIAccConstScale * 0.899976223);
  FIX_1_175875602 = Round(cIAccConstScale * 1.175875602);
  FIX_1_501321110 = Round(cIAccConstScale * 1.501321110);
  FIX_1_847759065 = Round(cIAccConstScale * 1.847759065);
  FIX_1_961570560 = Round(cIAccConstScale * 1.961570560);
  FIX_2_053119869 = Round(cIAccConstScale * 2.053119869);
  FIX_2_562915447 = Round(cIAccConstScale * 2.562915447);
  FIX_3_072711026 = Round(cIAccConstScale * 3.072711026);

// integer multiply with shift arithmetic right
function Multiply(A, B: integer): integer;
begin
  // Delphi seems to convert the "div" here to SAR just fine (D7), so we
  // don't use ASM but plain pascal
  Result := (A * B) div cIAccConstScale;
end;

// Descale and range limit to byte domain. We shift right over
// 12 bits: 9 bits to remove precision, and 3 bits to get rid of the additional
// factor 8 introducted by the IDCT transform.
function RangeLimit(A: integer): integer;
begin
  // Delphi seems to convert the "div" here to SAR just fine (D7), so we
  // don't use ASM but plain pascal
  Result := A div (1 shl cIAccRangeBits) + cCenterSample;
  if Result < 0 then
    Result := 0
  else
    if Result > cMaxSample then
      Result := cMaxSample;
end;

procedure InverseDCTIntAccurate8x8(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
var
  i, QIdx: integer;
  dci: integer;
  dcs: byte;
  p0, p1, p2, p3, p4, p5, p6, p7: Psmallint;
  w0, w1, w2, w3, w4, w5, w6, w7: Pinteger;
  s0, s1, s2, s3, s4, s5, s6, s7: Pbyte;
  z1, z2, z3, z4, z5: integer;
  tmp0, tmp1, tmp2, tmp3, tmp10, tmp11, tmp12, tmp13: integer;
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

      z2 := p2^ * Quant[QIdx + 2 * 8];
      z3 := p6^ * Quant[QIdx + 6 * 8];

      z1 := MULTIPLY(z2 + z3, FIX_0_541196100);
      tmp2 := z1 + MULTIPLY(z3, - FIX_1_847759065);
      tmp3 := z1 + MULTIPLY(z2, FIX_0_765366865);

      z2 := p0^ * Quant[QIdx + 0 * 8];
      z3 := p4^ * Quant[QIdx + 4 * 8];

      tmp0 := (z2 + z3);
      tmp1 := (z2 - z3);

      tmp10 := tmp0 + tmp3;
      tmp13 := tmp0 - tmp3;
      tmp11 := tmp1 + tmp2;
      tmp12 := tmp1 - tmp2;

      // Odd part

      tmp0 := p7^ * Quant[QIdx + 7 * 8];
      tmp1 := p5^ * Quant[QIdx + 5 * 8];
      tmp2 := p3^ * Quant[QIdx + 3 * 8];
      tmp3 := p1^ * Quant[QIdx + 1 * 8];

      z1 := tmp0 + tmp3;
      z2 := tmp1 + tmp2;
      z3 := tmp0 + tmp2;
      z4 := tmp1 + tmp3;
      z5 := MULTIPLY(z3 + z4, FIX_1_175875602);

      tmp0 := MULTIPLY(tmp0, FIX_0_298631336);
      tmp1 := MULTIPLY(tmp1, FIX_2_053119869);
      tmp2 := MULTIPLY(tmp2, FIX_3_072711026);
      tmp3 := MULTIPLY(tmp3, FIX_1_501321110);
      z1 := MULTIPLY(z1, - FIX_0_899976223);
      z2 := MULTIPLY(z2, - FIX_2_562915447);
      z3 := MULTIPLY(z3, - FIX_1_961570560);
      z4 := MULTIPLY(z4, - FIX_0_390180644);

      Inc(z3, z5);
      Inc(z4, z5);

      Inc(tmp0, z1 + z3);
      Inc(tmp1, z2 + z4);
      Inc(tmp2, z2 + z3);
      Inc(tmp3, z1 + z4);

      w0^ := tmp10 + tmp3;
      w7^ := tmp10 - tmp3;
      w1^ := tmp11 + tmp2;
      w6^ := tmp11 - tmp2;
      w2^ := tmp12 + tmp1;
      w5^ := tmp12 - tmp1;
      w3^ := tmp13 + tmp0;
      w4^ := tmp13 - tmp0;

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

      // Even part:
      z2 := w2^;
      z3 := w6^;

      z1 := MULTIPLY(z2 + z3, FIX_0_541196100);
      tmp2 := z1 + MULTIPLY(z3, - FIX_1_847759065);
      tmp3 := z1 + MULTIPLY(z2, FIX_0_765366865);

      tmp0 := w0^ + w4^;
      tmp1 := w0^ - w4^;

      tmp10 := tmp0 + tmp3;
      tmp13 := tmp0 - tmp3;
      tmp11 := tmp1 + tmp2;
      tmp12 := tmp1 - tmp2;

      // Odd part:
      tmp0 := w7^;
      tmp1 := w5^;
      tmp2 := w3^;
      tmp3 := w1^;

      z1 := tmp0 + tmp3;
      z2 := tmp1 + tmp2;
      z3 := tmp0 + tmp2;
      z4 := tmp1 + tmp3;
      z5 := MULTIPLY(z3 + z4, FIX_1_175875602);

      tmp0 := MULTIPLY(tmp0, FIX_0_298631336);
      tmp1 := MULTIPLY(tmp1, FIX_2_053119869);
      tmp2 := MULTIPLY(tmp2, FIX_3_072711026);
      tmp3 := MULTIPLY(tmp3, FIX_1_501321110);
      z1 := MULTIPLY(z1, - FIX_0_899976223);
      z2 := MULTIPLY(z2, - FIX_2_562915447);
      z3 := MULTIPLY(z3, - FIX_1_961570560);
      z4 := MULTIPLY(z4, - FIX_0_390180644);

      Inc(z3, z5);
      Inc(z4, z5);

      Inc(tmp0, z1 + z3);
      Inc(tmp1, z2 + z4);
      Inc(tmp2, z2 + z3);
      Inc(tmp3, z1 + z4);

      s0^ := RangeLimit(tmp10 + tmp3);
      s7^ := RangeLimit(tmp10 - tmp3);
      s1^ := RangeLimit(tmp11 + tmp2);
      s6^ := RangeLimit(tmp11 - tmp2);
      s2^ := RangeLimit(tmp12 + tmp1);
      s5^ := RangeLimit(tmp12 - tmp1);
      s3^ := RangeLimit(tmp13 + tmp0);
      s4^ := RangeLimit(tmp13 - tmp0);

    end;
    // Advance block pointers
    inc(s0, 8); inc(s1, 8); inc(s2, 8); inc(s3, 8);
    inc(s4, 8); inc(s5, 8); inc(s6, 8); inc(s7, 8);
    inc(w0, 8); inc(w1, 8); inc(w2, 8); inc(w3, 8);
    inc(w4, 8); inc(w5, 8); inc(w6, 8); inc(w7, 8);
  end;
end;

procedure InverseDCTIntAccurate4x4(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
var
  i, QIdx: integer;
  dci: integer;
  dcs: byte;
  p0, p1, p2, p3: Psmallint;
  w0, w1, w2, w3: Pinteger;
  s0, s1, s2, s3: Pbyte;
  z1, z2, z3, z4, z5: integer;
  tmp0, tmp1, tmp2, tmp3, tmp10, tmp11, tmp12, tmp13: integer;
begin
  QIdx := 0;

  // First do the columns
  p0 := @Coef[ 0]; p1 := @Coef[ 4]; p2 := @Coef[ 8]; p3 := @Coef[12];
  w0 := @Wrksp[ 0]; w1 := @Wrksp[ 4]; w2 := @Wrksp[ 8]; w3 := @Wrksp[12];
  for i := 0 to 3 do
  begin
    if (p1^ = 0) and (p2^ = 0) and (p3^ = 0) then
    begin
      dci := p0^ * Quant[QIdx];
      w0^ := dci; w1^ := dci; w2^ := dci; w3^ := dci;
    end else
    begin
      // Even part:

      z2 := p2^ * Quant[QIdx + 2 * 8];

      z1 := MULTIPLY(z2, FIX_0_541196100);
      tmp3 := z1 + MULTIPLY(z2, FIX_0_765366865);

      z2 := p0^ * Quant[QIdx + 0 * 8];

      tmp10 := z2 + tmp3;
      tmp13 := z2 - tmp3;
      tmp11 := z2 + z1;
      tmp12 := z2 - z1;

      // Odd part:

      z3 := p3^ * Quant[QIdx + 3 * 8];
      z4 := p1^ * Quant[QIdx + 1 * 8];

      z5 := MULTIPLY(z3 + z4, FIX_1_175875602);

      tmp2 := MULTIPLY(z3, FIX_3_072711026);
      tmp3 := MULTIPLY(z4, FIX_1_501321110);
      z1 := MULTIPLY(z4, - FIX_0_899976223);
      z2 := MULTIPLY(z3, - FIX_2_562915447);
      z3 := MULTIPLY(z3, - FIX_1_961570560);
      z4 := MULTIPLY(z4, - FIX_0_390180644);

      Inc(z3, z5);
      Inc(z4, z5);

      tmp0 := z1 + z3;
      tmp1 := z2 + z4;
      Inc(tmp2, z2 + z3);
      Inc(tmp3, z1 + z4);

      w0^ := tmp10 + tmp3;
      w3^ := tmp11 - tmp2;
      w1^ := tmp12 + tmp1;
      w2^ := tmp13 - tmp0;

    end;
    // Advance block pointers
    inc(p0); inc(p1); inc(p2); inc(p3);
    inc(w0); inc(w1); inc(w2); inc(w3);
    inc(QIdx);
  end;

  // Next do the rows
  w0 := @Wrksp[0]; w1 := @Wrksp[1]; w2 := @Wrksp[2]; w3 := @Wrksp[3];
  s0 := @Sample[0]; s1 := @Sample[1]; s2 := @Sample[2]; s3 := @Sample[3];
  for i := 0 to 3 do
  begin
    if (w1^ = 0) and (w2^ = 0) and (w3^ = 0) then
    begin
      dcs := RangeLimit(w0^);
      s0^ := dcs; s1^ := dcs; s2^ := dcs; s3^ := dcs;
    end else
    begin
      // Even part:

      z2 := w2^;

      z1 := MULTIPLY(z2, FIX_0_541196100);
      tmp3 := z1 + MULTIPLY(z2, FIX_0_765366865);

      tmp0 := w0^;

      tmp10 := tmp0 + tmp3;
      tmp13 := tmp0 - tmp3;
      tmp11 := tmp0 + z1;
      tmp12 := tmp0 - z1;

      // Odd part:

      tmp2 := w3^;
      tmp3 := w1^;

      z3 := tmp2;
      z4 := tmp3;
      z5 := MULTIPLY(z3 + z4, FIX_1_175875602);

      tmp2 := MULTIPLY(tmp2, FIX_3_072711026);
      tmp3 := MULTIPLY(tmp3, FIX_1_501321110);
      z1 := MULTIPLY(z4, - FIX_0_899976223);
      z2 := MULTIPLY(z3, - FIX_2_562915447);
      z3 := MULTIPLY(z3, - FIX_1_961570560);
      z4 := MULTIPLY(z4, - FIX_0_390180644);

      Inc(z3, z5);
      Inc(z4, z5);

      tmp0 := z1 + z3;
      tmp1 := z2 + z4;
      Inc(tmp2, z2 + z3);
      Inc(tmp3, z1 + z4);

      s0^ := RangeLimit(tmp10 + tmp3);
      s3^ := RangeLimit(tmp11 - tmp2);
      s1^ := RangeLimit(tmp12 + tmp1);
      s2^ := RangeLimit(tmp13 - tmp0);

    end;

    // Advance block pointers
    inc(s0, 4); inc(s1, 4); inc(s2, 4); inc(s3, 4);
    inc(w0, 4); inc(w1, 4); inc(w2, 4); inc(w3, 4);
  end;
end;

procedure InverseDCTIntAccurate2x2(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
var
  i, QIdx: integer;
  dci: integer;
  dcs: byte;
  p0, p1: Psmallint;
  w0, w1: Pinteger;
  s0, s1: Pbyte;
  z1, z2, z4, z5: integer;
  tmp0, tmp3, tmp10: integer;
begin
  QIdx := 0;
  // First do the columns
  p0 := @Coef[ 0]; p1 := @Coef[ 2];
  w0 := @Wrksp[ 0]; w1 := @Wrksp[ 2];
  for i := 0 to 1 do
  begin
    if p1^ = 0 then
    begin
      dci := p0^ * Quant[QIdx];
      w0^ := dci; w1^ := dci;
    end else
    begin
      z2 := p0^ * Quant[QIdx + 0 * 8];

      z4 := p1^ * Quant[QIdx + 1 * 8];

      z5 := MULTIPLY(z4, FIX_1_175875602);

      tmp3 := MULTIPLY(z4, FIX_1_501321110);
      z1 := MULTIPLY(z4, - FIX_0_899976223);
      z4 := MULTIPLY(z4, - FIX_0_390180644);

      Inc(z4, z5);

      tmp0 := z1 + z5;
      Inc(tmp3, z1 + z4);

      w0^ := z2 + tmp3;
      w1^ := z2 - tmp0;

    end;
    // Advance block pointers
    inc(p0); inc(p1);
    inc(w0); inc(w1);
    inc(QIdx);
  end;

  // Next do the rows
  w0 := @Wrksp[0]; w1 := @Wrksp[1];
  s0 := @Sample[0]; s1 := @Sample[1];
  for i := 0 to 1 do
  begin
    if w1^ = 0 then
    begin
      dcs := RangeLimit(w0^);
      s0^ := dcs; s1^ := dcs;
    end else
    begin
      tmp10 := w0^;

      z4 := w1^;

      z5 := MULTIPLY(z4, FIX_1_175875602);

      tmp3 := MULTIPLY(z4, FIX_1_501321110);
      z1 := MULTIPLY(z4, - FIX_0_899976223);
      z4 := MULTIPLY(z4, - FIX_0_390180644);

      Inc(z4, z5);

      tmp0 := z1 + z5;
      Inc(tmp3, z1 + z4);

      s0^ := RangeLimit(tmp10 + tmp3);
      s1^ := RangeLimit(tmp10 - tmp0);

    end;
    // Advance block pointers
    inc(s0, 2); inc(s1, 2);
    inc(w0, 2); inc(w1, 2);
  end;
end;

procedure InverseDCTIntAccurate1x1(const Coef: TsdCoefBlock; out Sample: TsdSampleBlock;
  const Quant: TsdIntArray64; var Wrksp: TsdIntArray64);
begin
  // Just the DC value to process
  Sample[0] := RangeLimit(Coef[0] * Quant[0]);
end;

end.
