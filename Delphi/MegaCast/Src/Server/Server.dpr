program Server;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  RoomUnit in 'Classes\RoomUnit.pas',
  MagaCastTextServer in 'Classes\MagaCastTextServer.pas',
  RoomInfo in 'Classes\RoomInfo.pas',
  MegaCastServerBase in '..\..\Lib\MegaCast\MegaCastServerBase.pas',
  MegaCastUtils in '..\..\Lib\MegaCast\MegaCastUtils.pas',
  MegaCastServer in '..\..\Lib\MegaCast\MegaCastServer.pas',
  BlockBuffer in '..\..\Lib\MegaCast\BlockBuffer.pas',
  FrameSizeSync in '..\..\Lib\MegaCast\FrameSizeSync.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
