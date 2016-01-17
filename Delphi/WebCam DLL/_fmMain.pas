unit _fmMain;

interface

uses
  WebCam,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TfmMain = class(TForm)
    Image: TImage;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FWebCam : TWebCam;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FWebCam.Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.PixelFormat := pf32bit;

  FWebCam := TWebCam.Create(Self);
  FWebCam.Start(320, 240);

  Image.Picture.Bitmap.Width  := FWebCam.Width;
  Image.Picture.Bitmap.Height := FWebCam.Height;

  Timer.Enabled := true;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWebCam);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  if FWebCam.SaveBitmapToData(Image.Picture.Bitmap.ScanLine[FWebCam.Height-1]) then Image.Repaint;
//  if FWebCam.GetBitmap(Image.Picture.Bitmap) then Image.Repaint;
end;

end.
