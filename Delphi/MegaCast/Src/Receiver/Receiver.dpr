program Receiver;

uses
  SysUtils,
  Forms,
  Dialogs,
  Global in 'Globals\Global.pas',
  Option in 'Options\Option.pas',
  View in 'Views\View.pas',
  _fmMain in 'Views\_fmMain.pas' {fmMain},
  _fmLogin in 'Views\_fmLogin.pas' {fmLogin},
  _fmMsg in 'Views\_fmMsg.pas' {fmMsg},
  UserList in '..\..\Lib\Global\UserList.pas',
  MegaCastMasterVoiceClient in '..\..\Lib\Global\MegaCastMasterVoiceClient.pas',
  MegaCastTextClient in '..\..\Lib\Global\MegaCastTextClient.pas',
  MegaCastUtils in '..\..\Lib\MegaCast\MegaCastUtils.pas',
  RoomClient in '..\..\Lib\Global\RoomClient.pas',
  MegaCastClient in '..\..\Lib\MegaCast\MegaCastClient.pas',
  MegaCastPlayer in '..\..\Lib\MegaCast\MegaCastPlayer.pas',
  ViewBase in '..\..\Lib\View\ViewBase.pas',
  _frDeskTopScreen in 'Views\_frDeskTopScreen.pas' {frDeskTopScreen: TFrame},
  BlockBuffer in '..\..\Lib\MegaCast\BlockBuffer.pas',
  BlockSync in '..\..\Lib\MegaCast\BlockSync.pas',
  MegaCastDecoder in '..\..\Lib\MegaCast\MegaCastDecoder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := true;

  try
  TGlobal.Obj.Initialize;

  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmLogin, fmLogin);
  Application.CreateForm(TfmMsg, fmMsg);
  Application.Run;
  except
    on E : Exception do MessageDlg(E.Message, mtError, [mbOk], 0);
  end;

  TGlobal.Obj.Finalize;
end.

