program Calc;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  Scanner in '..\..\Lib\Scanner.pas',
  Infix2Postfix in 'Infix2Postfix.pas',
  PolishParser in 'PolishParser.pas',
  ParserUtils in '..\..\Lib\ParserUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
