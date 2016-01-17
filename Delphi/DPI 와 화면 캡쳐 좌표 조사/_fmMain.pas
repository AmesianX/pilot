unit _fmMain;

interface

uses
  ScreenCapture, WindowRect,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm, IWindowRect)
    btCapture: TButton;
    Panel1: TPanel;
    ScrollBox: TScrollBox;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btCaptureClick(Sender: TObject);
  private  // implementation of IWindowRect
    function GetWindowLeft:integer;
    function GetWindowTop:integer;
    function GetWindowWidth:integer;
    function GetWindowHeight:integer;
  private
    FScreenCapture : TScreenCapture;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btCaptureClick(Sender: TObject);
begin
  FScreenCapture.X := Left;
  FScreenCapture.Y := Top;
  FScreenCapture.Width  := Width;
  FScreenCapture.Height := Height;

  FScreenCapture.Capture;

  Image.Picture.Bitmap.Assign( FScreenCapture.Bitmap );

  Caption := Format( '%d, %d', [FScreenCapture.Bitmap.Width, FScreenCapture.Bitmap.Height] );
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FScreenCapture := TScreenCapture.Create(Self);
  FScreenCapture.Width  := 320;
  FScreenCapture.Height := 240;
end;

function TfmMain.GetWindowHeight: integer;
begin
  Result := Height;
end;

function TfmMain.GetWindowLeft: integer;
begin
  Result := Left;
end;

function TfmMain.GetWindowTop: integer;
begin
  Result := Top;
end;

function TfmMain.GetWindowWidth: integer;
begin
  Result := Width;
end;

end.
