program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  ParsingContext in '..\..\Lib\ParsingContext.pas',
  Scanner in '..\..\Lib\Scanner.pas',
  TokenList in '..\..\Lib\TokenList.pas',
  ParserUtils in '..\..\Lib\ParserUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
