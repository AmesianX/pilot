unit _fmMain;

interface

uses
  JPegEncoder, BitmapSlice, CompareBytes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, WebCam, StdCtrls, ExtCtrls;

const
  _PixelFormat = pf32bit;
  _BlockSize = 64; 

type
  TfmMain = class(TForm)
    btStart: TButton;
    btStop: TButton;
    plCam: TPanel;
    Timer: TTimer;
    Image: TImage;
    moJPeg: TMemo;
    moWPeg: TMemo;
    procedure btStartClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FWebCam : TfrWebCam;
    FOldList, FNewList : TBitmapList;
    FBitmapSlice : TBitmapSlice;
    FJPenStream : TMemoryStream;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  FWebCam.Start;
end;

procedure TfmMain.btStopClick(Sender: TObject);
begin
  FWebCam.Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FOldList := TBitmapList.Create(_PixelFormat, _BlockSize);
  FNewList := TBitmapList.Create(_PixelFormat, _BlockSize);

  FBitmapSlice := TBitmapSlice.Create(_PixelFormat, _BlockSize);
  FBitmapSlice.BitmapList := FOldList;

  FJPenStream := TMemoryStream.Create;

  FWebCam := TfrWebCam.Create(Self);
  FWebCam.Parent := plCam;
  FWebCam.Start;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FWebCam.Free;
  FJPenStream.Free;
  FOldList.Free;
  FNewList.Free;
  FBitmapSlice.Free;
end;

function CompareBitmap(A,B:TBitmap; Limit:integer=25):boolean;
type
  TRGB = record
    B : byte;
    G : byte;
    R : byte;
    P : byte;
  end;
var
  Loop : Integer;
  rgbA, rgbB : ^TRGB;
  bCondition : boolean;
begin
  Result := true;

  rgbA := A.ScanLine[A.Height-1];
  rgbB := B.ScanLine[B.Height-1];
  for Loop := 0 to (A.Width * A.Height)-1 do begin
    bCondition := SQRT(SQR(rgbA^.R - rgbB^.R) + SQR(rgbA^.G-rgbB^.G) + SQR(rgbA^.B- rgbB^.B)) > Limit;
    if bCondition then begin
      Result := false;
      Break;
    end;

    Inc(rgbA);
    Inc(rgbB);
  end;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  Temp : pointer;
  msData : TMemoryStream;
  LoopX, LoopY, iCount, iSize : Integer;
begin
  if not FWebCam.GetBitmap(Image.Picture.Bitmap, 320, 240) then Exit;

  // JPeg Encoding
  BitmapToJpeg(Image.Picture.Bitmap, FJPenStream, 50);
  moJPeg.Lines.Values['Size'] := Format('%d KB', [FJPenStream.Size div 1024]);

  // WPeg Encoding
  Temp := FOldList;
  FOldList := FNewList;
  FNewList := Pointer(Temp);
  FBitmapSlice.BitmapList := FNewList;

  FBitmapSlice.Slice(Image.Picture.Bitmap);
  moWPeg.Lines.Values['Size'] := Format('Width:%d, Height:%d', [FBitmapSlice.BitmapList.Width, FBitmapSlice.BitmapList.Height]);

  FOldList.SetSize(FNewList.Width, FNewList.Height);

  msData := TMemoryStream.Create;
  try
    iCount := 0;
    iSize := 0;
    for LoopY := 0 to FNewList.Height - 1 do
    for LoopX := 0 to FNewList.Width - 1 do begin
      if not CompareBitmap(FNewList.Blocks[LoopX, LoopY], FOldList.Blocks[LoopX, LoopY]) then begin
        Inc(iCount);

        msData.Clear;
        BitmapToJpeg(FNewList.Blocks[LoopX, LoopY], msData, 50);
        iSize := iSize + msData.Size;
      end;
    end;

    moWPeg.Lines.Values['Count'] := Format('%d', [iCount]);
    moWPeg.Lines.Values['JPeg.Size'] := Format('%d', [iSize div 1024]);
  finally
    msData.Free;
  end;
end;

end.

