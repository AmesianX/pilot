program MiniPascal;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  ParserUtils in '..\..\Lib\ParserUtils.pas',
  Scanner in '..\..\Lib\Scanner.pas',
  ParsingNode in '..\..\Lib\ParsingNode.pas',
  NodeProgram in 'NodeProgram.pas',
  NodeCommandList in 'NodeCommandList.pas',
  NodeGo in 'NodeGo.pas',
  NodeTurn in 'NodeTurn.pas',
  NodeRepeat in 'NodeRepeat.pas',
  ParsingContext in 'ParsingContext.pas',
  TokenList in 'TokenList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
