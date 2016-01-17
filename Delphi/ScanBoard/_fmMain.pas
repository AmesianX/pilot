unit _fmMain;

interface

uses
  BitmapSlice, EdgeProcess,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btExecute: TButton;
    OpenDialog: TOpenDialog;
    edBrightness: TEdit;
    edColor: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    btOpen: TButton;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    sbSrc: TScrollBox;
    imgSrc: TImage;
    sbDst: TScrollBox;
    imgDst: TImage;
    spPenColor: TShape;
    edPenHSB: TEdit;
    edHSB: TEdit;
    spColor: TShape;
    procedure btExecuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure imgSrcMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgSrcClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBitmapSlice : TBitmapSlice;
    FEdgeProcess : TEdgeProcess;
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  Math, RyuGraphics;

{$R *.dfm}

procedure TfmMain.btExecuteClick(Sender: TObject);
var
  LoopX, LoopY, BrightLimit, ColorAaccuracy : integer;
begin
  imgDst.Picture.Bitmap.Width := imgSrc.Picture.Bitmap.Width;
  imgDst.Picture.Bitmap.Height := imgSrc.Picture.Bitmap.Height;

  BrightLimit := StrToIntDef(edBrightness.Text, 0);
  ColorAaccuracy := StrToIntDef(edColor.Text, 0);
  FBitmapSlice.Slice(imgSrc.Picture.Bitmap);

  for LoopY := 0 to FBitmapSlice.Height-1 do
  for LoopX := 0 to FBitmapSlice.Width-1 do begin
    FEdgeProcess.BrightAaccuracy := BrightnessOfBitmap(FBitmapSlice.Blocks[LoopX, LoopY]) * BrightLimit div 100;
    FEdgeProcess.ColorAaccuracy :=  ColorAaccuracy;
    FEdgeProcess.Execute(FBitmapSlice.Blocks[LoopX, LoopY]);

    imgDst.Picture.Bitmap.Canvas.Draw(LoopX*FBitmapSlice.BlockSize, LoopY*FBitmapSlice.BlockSize, FBitmapSlice.Blocks[LoopX, LoopY]);
  end;

  imgDst.Picture.SaveToFile('./After.bmp');
end;

procedure TfmMain.btOpenClick(Sender: TObject);
var
  Bitmap : TBitmap;
  RyuJpegImage : TRyuJpegImage;
begin
  if not OpenDialog.Execute then Exit;

  if Pos(LowerCase(ExtractFileExt(OpenDialog.FileName)), '.jpeg .jpg') > 0 then begin
    RyuJpegImage := TRyuJpegImage.Create;
    try
      RyuJpegImage.LoadFromFile(OpenDialog.FileName);
      RyuJpegImage.SaveToBitmap(imgSrc.Picture.Bitmap);
    finally
      RyuJpegImage.Free;
    end;
  end else begin
     Bitmap := TBitmap.Create;
     try
       Bitmap.LoadFromFile(OpenDialog.FileName);
       imgSrc.Picture.Bitmap.Width := Bitmap.Width;
       imgSrc.Picture.Bitmap.Height := Bitmap.Height;
       imgSrc.Picture.Bitmap.Canvas.Draw(0, 0, Bitmap);
     finally
       Bitmap.Free;
     end;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  imgSrc.Picture.Bitmap.PixelFormat := pf32bit;
  imgDst.Picture.Bitmap.PixelFormat := pf32bit;

  FBitmapSlice := TBitmapSlice.Create;
  FBitmapSlice.BlockSize := 32;

  FEdgeProcess := TEdgeProcess.Create(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FEdgeProcess.Free;
  FBitmapSlice.Free;
end;

procedure TfmMain.imgSrcClick(Sender: TObject);
var
  H, S, B : integer;
begin
  spPenColor.Brush.Color := spColor.Brush.Color;
  FEdgeProcess.PenColor := spPenColor.Brush.Color;

  ColorToHSB(spPenColor.Brush.Color, H, S, B);
  edPenHSB.Text := Format('%d, %d, %d', [H, S, B])
end;

procedure TfmMain.imgSrcMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  H, S, B : integer;
begin
  spColor.Brush.Color := imgSrc.Picture.Bitmap.Canvas.Pixels[X, Y];
  ColorToHSB(spColor.Brush.Color, H, S, B);
  edHSB.Text := Format('%d, %d, %d', [H, S, B])
end;

end.
