program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  PolygonBitmap in 'PolygonBitmap.pas',
  Strg in '..\..\Lib\RAD 2007\RyuLib\Strg.pas',
  VisualClass in 'VisualClass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
