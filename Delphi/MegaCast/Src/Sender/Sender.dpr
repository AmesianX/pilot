program Sender;

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
  MegaCastRecorder in '..\..\Lib\MegaCast\MegaCastRecorder.pas',
  MegaCastClient in '..\..\Lib\MegaCast\MegaCastClient.pas',
  ViewBase in '..\..\Lib\View\ViewBase.pas',
  DeskTopCapture in '..\..\Lib\MegaCast\DeskTopCapture.pas',
  FrameSlice in '..\..\Lib\MegaCast\FrameSlice.pas',
  BlockBuffer in '..\..\Lib\MegaCast\BlockBuffer.pas',
  BlockGrid in '..\..\Lib\MegaCast\BlockGrid.pas',
  MegaCastEncoder in '..\..\Lib\MegaCast\MegaCastEncoder.pas',
  OptionBase in '..\..\..\..\Working\Mega Channel - MegaCast\Lib\Option\OptionBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := false;
  Application.Title := 'MegaCast Sender';

  try
  TGlobal.Obj.Initialize;

  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmMsg, fmMsg);
  Application.CreateForm(TfmLogin, fmLogin);
  Application.Run;
  except
    on E : Exception do MessageDlg(E.Message, mtError, [mbOk], 0);
  end;

  TGlobal.Obj.Finalize;
end.

