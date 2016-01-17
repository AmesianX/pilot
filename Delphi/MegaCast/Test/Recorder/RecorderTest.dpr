program RecorderTest;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  MegaCastUtils in '..\..\Lib\MegaCast\MegaCastUtils.pas',
  BlockBuffer in '..\..\Lib\MegaCast\BlockBuffer.pas',
  BlockGrid in '..\..\Lib\MegaCast\BlockGrid.pas',
  BlockSync in '..\..\Lib\MegaCast\BlockSync.pas',
  DeskTopCaptrue in '..\..\Lib\MegaCast\DeskTopCaptrue.pas',
  FrameSizeSync in '..\..\Lib\MegaCast\FrameSizeSync.pas',
  FrameSlice in '..\..\Lib\MegaCast\FrameSlice.pas',
  MegaCastClient in '..\..\Lib\MegaCast\MegaCastClient.pas',
  MegaCastDecoder in '..\..\Lib\MegaCast\MegaCastDecoder.pas',
  MegaCastEncoder in '..\..\Lib\MegaCast\MegaCastEncoder.pas',
  MegaCastPlayer in '..\..\Lib\MegaCast\MegaCastPlayer.pas',
  MegaCastRecorder in '..\..\Lib\MegaCast\MegaCastRecorder.pas',
  MegaCastServer in '..\..\Lib\MegaCast\MegaCastServer.pas',
  MegaCastServerBase in '..\..\Lib\MegaCast\MegaCastServerBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
