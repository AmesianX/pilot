unit _fmMain;

interface

uses
  MegaCastUtils, Capture, ScreenCapture, BlockGrid,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btCapture: TButton;
    ScrollBox: TScrollBox;
    Image: TImage;
    procedure btCaptureClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FBlockGrid : TBlockGrid;
    FScreenCapture : TScreenCapture;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

type
  TRGB = packed record
    R,G,B : byte;
  end;

procedure TfmMain.btCaptureClick(Sender: TObject);
var
  pByte, pScanLine : ^byte;
  X, Y : integer;
  pPicxel : ^TRGB;
  Loop, LineCount, BlockWidth, Line, Size, Index  : integer;
  Loop2: Integer;
begin
  FScreenCapture.Capture;

  pByte := FScreenCapture.Bitmap.ScanLine[FScreenCapture.Bitmap.Height-1];

  Size := FScreenCapture.Bitmap.Width * FScreenCapture.Bitmap.Height * _PixelSize;
  LineCount := Size div _BlockLineSize;
  BlockWidth := FScreenCapture.Bitmap.Width div _BlockSize;

  Line := 0;
  X := 0;
  Y := 0;
  for Loop := 1 to LineCount do begin
//    pScanLine := Image.Picture.Bitmap.ScanLine[Image.Picture.Bitmap.Height-1];
//    Index := X*_BlockSize*_PixelSize + (Y*_BlockSize + Line)*BlockWidth*_BlockSize*_PixelSize;
//    Inc(pScanLine, Index);
//    Move(pByte^, pScanLine^, _BlockLineSize);

//    pPicxel := Pointer(pByte);
//    for Loop2 := 1 to _BlockLineSize div _PixelSize do begin
//      Image.Picture.Bitmap.Canvas.Pixels[X*_BlockSize+Loop2, Y*_BlockSize+Line] := pPicxel^.R shl 16 + pPicxel^.G shl 8 + pPicxel^.B;
//      Inc(pPicxel);
//    end;

    FBlockGrid.SetLine(X, Y, Line, pByte);

    Inc(X);
    if X >= BlockWidth then begin
      X := 0;

      // 이미지를 블록으로 나눴기 때문에, 이미지가 _BlockSize 단위로 라인을 읽을 때마다 아래 블록으로 내려간다.
      Inc(Line);
      if Line >= _BlockSize then begin
        Line := 0;
        Inc(Y);
      end;
    end;

    Inc(pByte, _BlockLineSize);
  end;

  if FBlockGrid.GetBitmap(Image.Picture.Bitmap) then Image.Repaint;	
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.PixelFormat := _PixelFormat;
  Image.Picture.Bitmap.Width  := 1024;
  Image.Picture.Bitmap.Height := 768;

  FBlockGrid := TBlockGrid.Create(Self);
  FBlockGrid.SetFrameSize(FrameSize(1024, 768));

  FScreenCapture := TScreenCapture.Create(Self);
  FScreenCapture.CaptureType := ctRect;
  FScreenCapture.PixelFormat := _PixelFormat;
  FScreenCapture.X := 0;
  FScreenCapture.Y := 0;
  FScreenCapture.Width  := 1024;
  FScreenCapture.Height := 768;
  FScreenCapture.WithCursor := true;
end;

end.
