program BlockGridTest;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  MegaCastUtils in '..\..\Lib\MegaCast\MegaCastUtils.pas',
  BlockGrid in '..\..\Lib\MegaCast\BlockGrid.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
