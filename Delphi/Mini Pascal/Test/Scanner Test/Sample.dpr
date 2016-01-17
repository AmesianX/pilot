program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  Scanner in '..\..\Lib\Scanner.pas',
  ParserUtils in '..\..\Lib\ParserUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
