unit _fmMain;

interface

uses
  ApplicationList,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    Label1: TLabel;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FOldWindowHandle : HWND;
    FApplicationList : TApplicationList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FOldWindowHandle := 0;
  FApplicationList := TApplicationList.Create(Self);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  Loop: Integer;
  WindowHandle : HWND;
begin
  Timer.Enabled := false;
  try
    WindowHandle := GetForegroundWindow;
    if WindowHandle = FOldWindowHandle then Exit;

    FOldWindowHandle := WindowHandle;

    Caption := IntToStr(WindowHandle);

    FApplicationList.Update;

    for Loop := 0 to FApplicationList.Count-1 do
      if FApplicationList.Handles[Loop] = WindowHandle then
        Label1.Caption := FApplicationList.Names[Loop];
  finally
    Timer.Enabled := true;
  end;
end;

end.
