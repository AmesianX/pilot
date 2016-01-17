unit _fmMain;

interface

uses
  Disk,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, Vcl.Imaging.jpeg;

type
  TfmMain = class(TForm)
    pl1: TPanel;
    pl2: TPanel;
    spl1: TSplitter;
    ScrollBox: TScrollBox;
    imgMain: TImage;
    btOpen: TButton;
    dlgOpen: TOpenDialog;
    procedure btOpenClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

type
  TBackgroundColors = array of TColor;

procedure SilceBitmap(ABitmap:TBitmap; BackgroundColor:TColor; AFileName:string); overload;
begin

end;

procedure SilceBitmap(ABitmap:TBitmap; BackgroundColors:TBackgroundColors; AFileName:string); overload;
begin

end;

{$R *.dfm}

procedure TfmMain.btOpenClick(Sender: TObject);
var
  Bitmap : TBitmap;
begin
  if not dlgOpen.Execute then exit;

  imgMain.Picture.LoadFromFile(dlgOpen.FileName);

  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.Width  := imgMain.Picture.Width;
    Bitmap.Height := imgMain.Picture.Height;
    Bitmap.Canvas.Draw(0, 0, imgMain.Picture.Graphic);

    SilceBitmap(Bitmap, $E6E8E8, GetExecPath + 'Test');
  finally
    Bitmap.Free;
  end;
end;

end.
