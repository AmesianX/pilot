unit _fmMain;

interface

uses
  RyuGraphics,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg;

type
  TfmMain = class(TForm)
    Image: TImage;
    ImageLayer: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ImageLayerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageLayerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FMouseDown : TPoint;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure PrepareAlpha(ABitmap:TBitmap);
var
  Loop: Integer;
  pPixel : PRGBQuad;
begin
  if ABitmap.PixelFormat <> pf32bit then
    raise Exception.Create('RyuGraphics.SetAlpha - ABitmap.PixelFormat <> pf32bit');

  pPixel := ABitmap.ScanLine[ABitmap.Height-1];

  for Loop := 1 to ABitmap.Width * ABitmap.Height do begin
    pPixel^.rgbBlue :=  $68;
    pPixel^.rgbGreen := $92;
    pPixel^.rgbRed :=   $FA;
    pPixel^.rgbReserved := $00;

    Inc( pPixel );
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  MakeOpaque( Self );

  ImageLayer.Picture.Bitmap.PixelFormat := pf32bit;
  ImageLayer.Picture.Bitmap.AlphaFormat := afDefined;
  ImageLayer.Picture.Bitmap.Width  := ClientWidth;
  ImageLayer.Picture.Bitmap.Height := ClientHeight;

  PrepareAlpha( ImageLayer.Picture.Bitmap );

  ImageLayer.Repaint;
end;

procedure TfmMain.ImageLayerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Caption := '';

  FMouseDown.X := X;
  FMouseDown.Y := Y;

  ImageLayer.Picture.Bitmap.Canvas.MoveTo( X, Y );
end;

function IsTransparentColor(APixel:TRGBQuad):boolean;
begin
  Result :=
    (APixel.rgbBlue  = $68) and
    (APixel.rgbGreen = $92) and
    (APixel.rgbRed   = $FA);
end;

procedure SetTransparentColor(ABitmap:TBitmap; AColor:TColor);
var
  Loop: Integer;
  pPixel : PRGBQuad;
begin
  if ABitmap.PixelFormat <> pf32bit then
    raise Exception.Create('RyuGraphics.SetAlpha - ABitmap.PixelFormat <> pf32bit');

  pPixel := ABitmap.ScanLine[ABitmap.Height-1];

  for Loop := 1 to ABitmap.Width * ABitmap.Height do begin
    if IsTransparentColor(pPixel^) then pPixel^.rgbReserved := $00
    else pPixel^.rgbReserved := $FF;

    Inc( pPixel );
  end;
end;

function PointDistance(A,B:TPoint):integer;
begin
  Result := Round( SQRT (SQR(A.X-B.X) + SQR(A.Y-B.Y)) );
end;

procedure TfmMain.ImageLayerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
const
  POINT_DISTANCE_LIMIT = 1;
begin
  if Shift = [ssLeft] then begin
    if PointDistance(FMouseDown, Point(X, Y)) > POINT_DISTANCE_LIMIT then Caption := 'Moved';

    ImageLayer.Picture.Bitmap.Canvas.LineTo( X, Y );
    SetTransparentColor( ImageLayer.Picture.Bitmap, $123456 );
  end;
end;

end.
