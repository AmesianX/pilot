unit NodeCommandList;

interface

uses
  ParserUtils, ParsingContext, ParsingNode,
  Windows, Classes, SysUtils;

type
  TNodeCommandList = class (TNonTerminalNode)
  private
  public
    procedure Parse; override;
    function Clone:TParsingNode; override;
    function IsMyTurn:boolean; override;
    procedure Compile(AOutPut:TStringList); override;
  end;

implementation

{ TNodeCommandList }

function TNodeCommandList.Clone: TParsingNode;
begin
  Result := TNodeCommandList.Create(FCommands, FContext);
end;

procedure TNodeCommandList.Compile(AOutPut: TStringList);
begin
  AOutPut.Add('// CommandList Start');

  do_VisitNodesAndCompile(AOutPut);
end;

function TNodeCommandList.IsMyTurn: boolean;
begin
  Result := FContext.CurrentToken.IsSameToken('begin');
end;

procedure TNodeCommandList.Parse;
begin
  {$IFDEF DEBUG}
    OutputDebugString('TNodeCommandList.Parse');
  {$ENDIF}

  FContext.SkipToken('begin');

  while not FContext.EOF do begin
    if not FindAppropriateCommandAndParse then Break;
  end;

  FContext.SkipToken('end');
  if FContext.CurrentToken.IsSameToken(';') then FContext.SkipToken;
end;

end.
