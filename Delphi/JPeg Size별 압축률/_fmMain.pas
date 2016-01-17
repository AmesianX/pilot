unit _fmMain;

interface

uses
  IJLUtils,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

const
  _FrameSize = 32;
  _PixelSize = 3;
  _BufferSize = 8 * 1024;

type
  TForm2 = class(TForm)
    imgSrc: TImage;
    imgDst: TImage;
    btExecute: TButton;
    moMsg: TMemo;
    Button1: TButton;
    procedure btExecuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FJpegBuffer : array [0..1024*1024] of byte;
    FBuffer1, FBuffer2 : array [0.._BufferSize-1] of byte;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure RandomPixel(ABitmap:TBitmap);
var
  pData : PByte;
  Loop: Integer;
begin
  pData := ABitmap.ScanLine[ABitmap.Height-1];
  for Loop := 0 to (ABitmap.Width * ABitmap.Height * _PixelSize)-1 do begin
    pData^ := Random(256);
    Inc(pData);
  end;
end;

function HeaderSize(pSrc,pDst:PByte; ASize:integer):integer;
begin
  for Result := 0 to ASize-1 do begin
    if pSrc^ <> pDst^ then Break;    

    Inc(pSrc);
    Inc(pDst);
  end;
end;

procedure TForm2.btExecuteClick(Sender: TObject);
var
  iSize, iMinSize : integer;
begin
//  RandomPixel(imgSrc.Picture.Bitmap);

  RandomPixel(imgDst.Picture.Bitmap);

  BitmapToJpegCopy(@FJpegBuffer, imgSrc.Picture.Bitmap.ScanLine[_FrameSize-1], _FrameSize, _FrameSize, @FBuffer1, iSize, 20);
  moMsg.Lines.Add(Format('FBuffer1 Size: %d', [iSize - 659]));

  iMinSize := iSize;

  BitmapToJpegCopy(@FJpegBuffer, imgDst.Picture.Bitmap.ScanLine[_FrameSize-1], _FrameSize, _FrameSize, @FBuffer2, iSize, 20);
  moMsg.Lines.Add(Format('FBuffer2 Size: %d', [iSize - 659]));

  if iSize < iMinSize then iMinSize := iSize;

  moMsg.Lines.Add(Format('HeaderSize Size: %d', [HeaderSize(@FBuffer1, @FBuffer2, iMinSize)]));
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  iSize, s, w, h : integer;
begin
//  RandomPixel(imgSrc.Picture.Bitmap);
//  imgSrc.Repaint;

  BitmapToJpegCopy(@FJpegBuffer, imgSrc.Picture.Bitmap.ScanLine[_FrameSize-1], _FrameSize, _FrameSize, @FBuffer1, iSize, 10);
  moMsg.Lines.Add(Format('FBuffer1 Size: %d', [iSize - 659]));

  JpegToBitmapCopy(@FBuffer1, iSize, imgDst.Picture.Bitmap.ScanLine[_FrameSize-1], s, w, h);

  imgDst.Repaint;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  imgSrc.Picture.Bitmap.Width  := _FrameSize;
  imgSrc.Picture.Bitmap.Height := _FrameSize;

  imgDst.Picture.Bitmap.Width  := _FrameSize;
  imgDst.Picture.Bitmap.Height := _FrameSize;

  imgSrc.Picture.Bitmap.PixelFormat := pf24bit;
  imgDst.Picture.Bitmap.PixelFormat := pf24bit;
end;

end.
