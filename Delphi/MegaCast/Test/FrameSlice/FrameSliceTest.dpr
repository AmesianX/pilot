program FrameSliceTest;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  FrameSlice in '..\..\Lib\MegaCast\FrameSlice.pas',
  MegaCastUtils in '..\..\Lib\MegaCast\MegaCastUtils.pas',
  BlockGrid in '..\..\Lib\MegaCast\BlockGrid.pas',
  DeskTopCaptrue in '..\..\Lib\MegaCast\DeskTopCaptrue.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
