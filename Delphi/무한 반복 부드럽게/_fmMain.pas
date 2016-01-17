unit _fmMain;

interface

uses
  SpeedGun, SimpleThread,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
  private
    FSpeedGun : TSpeedGun;
    FSimpleThread : TSimpleThread;
    procedure on_Repeat(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSimpleThread.Terminate;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FSpeedGun := TSpeedGun.Create;
  FSimpleThread := TSimpleThread.Create(on_Repeat);
end;

procedure TfmMain.on_Repeat(Sender: TObject);
var
  iFrequency : int64;
  iTaskStart, iTaskEnd : int64;
begin
  QueryPerformanceFrequency(iFrequency);

  iFrequency := iFrequency div 1000;

  FSpeedGun.Start;

  while not FSimpleThread.Terminated do begin
    QueryPerformanceCounter( iTaskStart );

    // Task
    FSpeedGun.IncSize(1);

    QueryPerformanceCounter( iTaskEnd );

    if (iTaskEnd - iTaskStart) < iFrequency then Sleep(1);
  end;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := false;
  try
    Caption := IntToStr(FSpeedGun.Speed);
  finally
    Timer.Enabled := true;
  end;
end;

end.
