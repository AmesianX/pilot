program Client;

uses
  ExceptionLog,
  Forms,
  SysUtils,
  Dialogs,
  CamClient in 'Global\CamClient.pas',
  Global in 'Global\Global.pas',
  TextClient in 'Global\TextClient.pas',
  UserList in 'Global\UserList.pas',
  Option in 'Option\Option.pas',
  _fmMain in 'View\_fmMain.pas' {fmMain},
  View in 'View\View.pas',
  _fmLogin in 'Dialog\_fmLogin.pas' {fmLogin},
  DataTypes in '..\..\Lib\DataTypes.pas',
  _frChat in 'Frame\_frChat.pas' {frChat: TFrame},
  _frCam in 'Frame\_frCam.pas' {frCam: TFrame},
  _frToolBox in 'Frame\_frToolBox.pas' {frToolBox: TFrame},
  _frMultiCamProxy in 'Frame\_frMultiCamProxy.pas' {frMultiCamProxy: TFrame},
  _frMultiUserCam in '..\..\..\..\Working\MegaChannel.Library\MultiUserCam\_frMultiUserCam.pas' {frMultiUserCamView: TFrame},
  _frMicSetting in 'Frame\_frMicSetting.pas' {frMicSetting: TFrame},
  _fmAbout in 'Dialog\_fmAbout.pas' {fmAbout};

{$R *.res}

begin
  if ParamCount < 3 then
  begin
    MessageDlg('실행 파라메터가 잘못 되었습니다.', mtError, [mbOK], 0);
    Exit;
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;
  Application.Title := TOption.Obj.ApplicationTitle;

  try
    TGlobal.Obj.Initialize;
    try
      Application.CreateForm(TfmMain, fmMain);
  TOption.Obj.Logined := LoginDlg;

      if TOption.Obj.Logined then
      begin
        fmMain.Show;
        Application.Run;
      end
      else begin
        Application.Terminate;
      end;
    finally
      TGlobal.Obj.Finalize;
    end;
  except on E: Exception do
    MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end.
