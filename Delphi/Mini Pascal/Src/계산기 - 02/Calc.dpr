program Calc;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  Scanner in '..\..\Lib\Scanner.pas',
  ParserUtils in '..\..\Lib\ParserUtils.pas',
  Calculator in 'Calculator.pas',
  TokenBuilder in 'TokenBuilder.pas',
  Tokens in 'Tokens.pas',
  TokenList in 'TokenList.pas',
  Infix2Postfix in 'Infix2Postfix.pas',
  ParsingContext in 'ParsingContext.pas',
  PolishParser in 'PolishParser.pas',
  Functions in 'Functions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
