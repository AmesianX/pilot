unit _fmMain;

interface

uses
  VirtualSocketClient, VirtualSocketClientProviderIndy9, DeskCamSender, Capture,
  DeskCamSampleUtils,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, _fmLog;

type
  TfmMain = class(TForm)
    MainMenu: TMainMenu;
    N1: TMenuItem;
    mi_ViewLog: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mi_ViewLogClick(Sender: TObject);
  private
    FClientProvider: TVirtualSocketClientProvider;
    FSender: TDeskCamSender;
    FLogForm: TfmLog;
    procedure on_disconnected(Sender: TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  if ParamStr(1) = '' then
  begin
    ShowMessage('파라미터를 입력해 주십시오.');
    Application.Terminate;
    Exit;
  end;

  FClientProvider := TVirtualSocketClientProviderIndy9.Create(Self);

  FSender := TDeskCamSender.Create(Self, FClientProvider.CreateSocket);
  FSender.OnDisconnected := on_disconnected;

  FClientProvider.Host := ParamStr(1);
  FClientProvider.Port := SERVER_PORT;

  if not FClientProvider.Connect then
  begin
    ShowMessage('서버와의 연결에 실패하였습니다.');
    Application.Terminate;
    Exit;
  end;

  FSender.MonitorNo := 0;
//  FSender.CaptureType := ctRect;
//  FSender.CaptureRect := Rect(100,100,910,616);
  FSender.Start;

  FLogForm := TfmLog.Create(Self);
  FLogForm.Caption := 'Sender Log';
  FLogForm.Log := FSender.Log;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FSender.Free;
  FClientProvider.Free;
  FLogForm.Free;
end;

procedure TfmMain.mi_ViewLogClick(Sender: TObject);
begin
  FLogForm.Show;
end;

procedure TfmMain.on_disconnected(Sender: TObject);
begin
  ShowMessage('서버와의 연결이 끊겼습니다.');
  Application.Terminate;
end;

end.
