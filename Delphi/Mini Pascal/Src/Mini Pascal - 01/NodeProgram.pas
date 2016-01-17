unit NodeProgram;

interface

uses
  ParserUtils, ParsingContext, ParsingNode,
  Windows, Classes, SysUtils;

type
  TNodeProgram = class (TNonTerminalNode)
  private
  public
    ProgrameName : string;
  public
    procedure Parse; override;
    procedure Compile(AOutPut:TStringList); override;
  end;

implementation

{ TNodeProgram }

procedure TNodeProgram.Compile(AOutPut: TStringList);
begin
  AOutPut.Add(Format('// Program %s', [ProgrameName]));
  AOutPut.Add('');
  AOutPut.Add('Const Left 0');
  AOutPut.Add('Const Right 1');
  AOutPut.Add('Const Around 2');
  AOutPut.Add('');

  do_VisitNodesAndCompile(AOutPut);
end;

procedure TNodeProgram.Parse;
var
  TokenInfo : TTokenInfo;
begin
  {$IFDEF DEBUG}
    OutputDebugString('TNodeProgram.Parse');
  {$ENDIF}

  FContext.SkipToken('program');

  TokenInfo := FContext.SkipToken(ttIdentifier);
  ProgrameName := TokenInfo.Text;

  FContext.SkipToken(';');

  while not FContext.EOF do begin
    if not FindAppropriateCommandAndParse then Break;
  end;

  {$IFDEF DEBUG}
    OutputDebugString(PChar(Format('%s.Parse: FNodes.Count = %d', [ClassName, FNodes.Count])));
  {$ENDIF}
  
  FContext.SkipToken('end');
  FContext.SkipToken('.');
end;

end.
