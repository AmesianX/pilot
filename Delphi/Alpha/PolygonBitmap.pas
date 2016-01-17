unit PolygonBitmap;

interface

uses
  Windows, Classes, SysUtils, Graphics;

type
  TPoints = array of TPoint;

  TPolygonBitmap = class (TBitmap)
  private
    procedure do_ClearBitmap;
    procedure do_SetBitmapSize;
  private
    FPoints: TPoints;
    FBackgroundColor: TColor;
    FForegroundColor: TColor;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetForegroundColor(const Value: TColor);
  public
    constructor Create; override;
    
    procedure Clear;
    procedure AddPolygon(Points:TPoints);

    procedure DrawPolygon(X,Y:integer; Bitmap:TBitmap; Rate:integer=255);

    property Points : TPoints read FPoints;
    property ForegroundColor : TColor read FForegroundColor write SetForegroundColor;
    property BackgroundColor : TColor read FBackgroundColor write SetBackgroundColor;
  end;

implementation

{ TPolygonBitmap }

procedure TPolygonBitmap.AddPolygon(Points: TPoints);
var
  Loop, iOldLength, iLength : Integer;
begin
  iOldLength := Length(FPoints);
  iLength := iOldLength + Length(Points);
  SetLength(FPoints, iLength);

  for Loop := iOldLength to iLength - 1 do begin
    FPoints[Loop].X := Points[Loop-iOldLength].X + (Canvas.Pen.Width div 2);
    FPoints[Loop].Y := Points[Loop-iOldLength].Y + (Canvas.Pen.Width div 2);
  end;

  do_SetBitmapSize;

  Canvas.Pen.Color := ForegroundColor;
  Canvas.Brush.Color := BackgroundColor;
  Canvas.Polygon(FPoints);
end;

procedure TPolygonBitmap.Clear;
begin
  do_ClearBitmap;
  SetLength(FPoints, 0);
end;

constructor TPolygonBitmap.Create;
begin
  inherited;

  Canvas.Pen.Width := 3;
  FForegroundColor := clBlack;
  FBackgroundColor := clRed;
end;

procedure TPolygonBitmap.do_ClearBitmap;
begin
  Width := 0;
  Height := 0;
end;

procedure TPolygonBitmap.do_SetBitmapSize;
var
  Loop, iWidth, iHeight, iMargin : integer;
begin
  iMargin := (Canvas.Pen.Width div 2) + Integer(Canvas.Pen.Width = 1);
  
  for Loop := Low(FPoints) to High(FPoints) do  begin
    iWidth  := FPoints[Loop].X + iMargin;
    iHeight := FPoints[Loop].Y + iMargin;
    if iWidth > Width then Width := iWidth;
    if iHeight > Height then Height := iHeight
  end;

  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(Rect(0, 0, Width, Height));
end;

type
  TPixels = array [0..0] of TRGBQuad;

function BuffersEqual(Buf1,Buf2:Pointer; Size:Integer): Boolean; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,Buf1
        MOV     EDI,Buf2
        XOR     EAX,EAX
        JECXZ   @@1
        CLD
        REPE    CMPSB
        JNE     @@1
        INC     EAX
@@1:    POP     ESI
        POP     EDI
end;

procedure TPolygonBitmap.DrawPolygon(X, Y: integer; Bitmap: TBitmap; Rate: integer);
var
  Col, Row, iX, iY : Integer;
  pxForeground : TRGBQuad;
  pxSrc, pxDst : ^TPixels;
  dRateSrc, dRateDst : double;
begin
  if Rate > 100 then Rate := 100;

  X := X - (Canvas.Pen.Width div 2);
  Y := Y - (Canvas.Pen.Width div 2);

  dRateSrc := Rate / 100;
  dRateDst := (100-Rate) / 100;

  Move(ForegroundColor, pxForeground, SizeOf(ForegroundColor));

  for Row := 0 to Height - 1 do begin
    iY := Y + Row;
    if (iY < 0) or (iY >= Bitmap.Height) then Continue;

    pxSrc := ScanLine[Row];
    pxDst := Bitmap.ScanLine[iY];

    for Col := 0 to Width - 1 do begin
      iX := X + Col;
      if (iX < 0) or (iX >= Bitmap.Width) then Continue;

      if BuffersEqual(@pxSrc^[Col], @pxForeground, SizeOf(pxForeground)-1) then begin
        pxDst^[iX].rgbRed :=   pxSrc^[Col].rgbRed;
        pxDst^[iX].rgbGreen := pxSrc^[Col].rgbGreen;
        pxDst^[iX].rgbBlue :=  pxSrc^[Col].rgbBlue;
      end else begin
        pxDst^[iX].rgbRed :=   Round(pxSrc^[Col].rgbRed*dRateSrc   + pxDst^[Col].rgbRed*dRateDst);
        pxDst^[iX].rgbGreen := Round(pxSrc^[Col].rgbGreen*dRateSrc + pxDst^[Col].rgbGreen*dRateDst);
        pxDst^[iX].rgbBlue :=  Round(pxSrc^[Col].rgbBlue*dRateSrc  + pxDst^[Col].rgbBlue*dRateDst);
      end;
    end;
  end;
end;

procedure TPolygonBitmap.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
end;

procedure TPolygonBitmap.SetForegroundColor(const Value: TColor);
begin
  FForegroundColor := Value;
end;

end.
