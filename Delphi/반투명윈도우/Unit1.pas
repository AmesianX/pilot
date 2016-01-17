unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  pngimage, Dialogs, ExtCtrls, Menus, StdCtrls, ExtDlgs, BitmapButton;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    PNG1: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    Label1: TLabel;
    N1: TMenuItem;
    Timer: TTimer;
    BitmapButton1: TBitmapButton;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PNG1Click(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure BitmapButton1Click(Sender: TObject);
  private
    FBitmap : TBitmap;
    FPNGImage : TPNGObject;
    procedure LoadPNG(AFileName:string);
    procedure UpdateLayered;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure MakeAlphaBlend(Bitmap: TBitmap; PNG: TPNGObject);
type
  TPixels = array [0 .. 0] of TRGBQuad;
  TPixels2 = array [0 .. 0] of TRGBTriple;
var
  Pixels: ^TPixels;
  Pixels2: ^TPixels2;
  Col, Row: Integer;
  Alpha: single;
begin
  for Row := 0 to Bitmap.Height - 1 do begin
    Pixels := Bitmap.ScanLine[Row];
    Pixels2 := PNG.ScanLine[Row];

    for Col := 0 to Bitmap.Width - 1 do begin
      Pixels^[Col].rgbReserved := PNG.AlphaScanline[Row]^[Col];

      if PNG.AlphaScanline[Row]^[Col] < 1 then begin
        Pixels^[Col].rgbRed := 0;
        Pixels^[Col].rgbGreen := 0;
        Pixels^[Col].rgbBlue := 0;
      end else begin
        Alpha := Pixels^[Col].rgbReserved / 255;
        Pixels^[Col].rgbRed := Round(Pixels2^[Col].rgbtRed * Alpha);
        Pixels^[Col].rgbGreen := Round(Pixels2^[Col].rgbtGreen * Alpha);
        Pixels^[Col].rgbBlue := Round(Pixels2^[Col].rgbtBlue * Alpha);
      end;
    end;
  end;
end;

procedure TForm1.BitmapButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBitmap := TBitmap.Create;
  FPNGImage := TPNGObject.Create;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   ReleaseCapture;
   SendMessage(Handle, WM_SYSCOMMAND, $F012, 0);
end;

procedure TForm1.LoadPNG(AFileName: string);
begin
  FPNGImage.LoadFromFile( AFileName );

  FBitmap.PixelFormat := pf32Bit;

  FBitmap.Width  := FPNGImage.Width;
  FBitmap.Height := FPNGImage.Height;

  MakeAlphaBlend( FBitmap, FPNGImage );
end;

procedure TForm1.PNG1Click(Sender: TObject);
begin
  if not OpenPictureDialog.Execute then Exit;

  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
//  ShowWindow(Application.Handle, SW_HIDE);

  LoadPNG(OpenPictureDialog.FileName);

  Self.Width  := FBitmap.Width;
  Self.Height := FBitmap.Height;

  BitmapButton1.Left := (Width  div 2) - (BitmapButton1.Width  div 2);
  BitmapButton1.Top  := (Height div 2) - (BitmapButton1.Height div 2);

  Timer.Enabled := true;
//  UpdateLayered;
end;

procedure TForm1.TimerTimer(Sender: TObject);
var
  pPixels : ^TRGBQuad;
  Loop: Integer;
begin
//  FPNGImage.Canvas.Draw( BitmapButton1.Left, BitmapButton1.Top, BitmapButton1.InternalBitmap );
//  MakeAlphaBlend( FBitmap, FPNGImage );

  pPixels := BitmapButton1.InternalBitmap.ScanLine[BitmapButton1.InternalBitmap.Height-1];

  for Loop := 1 to 21 * 21 do begin
    pPixels^.rgbReserved := 255;
    Inc( pPixels );
  end;

  FBitmap.Canvas.Draw( BitmapButton1.Left, BitmapButton1.Top, BitmapButton1.InternalBitmap );

  UpdateLayered;
end;

procedure TForm1.UpdateLayered;
var
  DestPoint, SourcePoint: TPoint;
  BlendFunction: TBlendFunction;
  Size: TSize;
  DC : HDC;
begin
  Size.cx := FBitmap.Width;
  Size.cy := FBitmap.Height;

  DestPoint := BoundsRect.TopLeft;
  SourcePoint := Point(0, 0);

  DC := GetDC(0);
  try
    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := 255;
    BlendFunction.AlphaFormat := AC_SRC_ALPHA;

    UpdateLayeredWindow(Handle, DC, @DestPoint, @Size, FBitmap.Canvas.Handle, @SourcePoint, clBlack, @BlendFunction, ULW_ALPHA);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TForm1.N1Click(Sender: TObject);
begin
  close;
end;

end.
