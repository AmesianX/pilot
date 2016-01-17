program BlockBufferTest;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  BlockBuffer in '..\..\Lib\MegaCast\BlockBuffer.pas',
  MegaCastUtils in '..\..\Lib\MegaCast\MegaCastUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
