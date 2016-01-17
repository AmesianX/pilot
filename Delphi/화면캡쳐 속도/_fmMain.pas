unit _fmMain;

interface

uses
  ScreenCapture,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FTick : Cardinal;
    FCount : integer;
    FScreenCapture : TScreenCapture;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FTick := GetTickCount;
  FCount := 0;

  FScreenCapture := TScreenCapture.Create(Self);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  iTerm : Cardinal;
begin
  Timer.Enabled := false;
  try
    FScreenCapture.Capture;

    FCount := FCount + 1;
    iTerm := (GetTickCount - FTick) div 1000;

    if iTerm > 0 then
      Caption := Format('%d, %d, %d', [FCount div iTerm, FCount, iTerm]);
  finally
    Timer.Enabled := true;
  end;
end;

end.
