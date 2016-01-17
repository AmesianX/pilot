unit NodeRepeat;

interface

uses
  ParserUtils, ParsingContext, ParsingNode,
  Windows, Classes, SysUtils;

type
  TNodeRepeat = class (TNonTerminalNode)
  private
  public
    RepeatTimes : integer;
  public
    procedure Parse; override;
    function Clone:TParsingNode; override;
    function IsMyTurn:boolean; override;
    procedure Compile(AOutPut:TStringList); override;
  end;

implementation

{ TNodeRepeat }

function TNodeRepeat.Clone: TParsingNode;
begin
  Result := TNodeRepeat.Create(FCommands, FContext);
end;

procedure TNodeRepeat.Compile(AOutPut: TStringList);
var
  Index : integer;
begin
  Index := FContext.Index;
  
  AOutPut.Add('// Repeat Start');
  AOutPut.Add(Format('Number Count%d', [Index]));
  AOutPut.Add(Format('Move %d to Count%d', [RepeatTimes, Index]));
  AOutPut.Add(Format('@RepeatStart%d', [Index]));
  AOutPut.Add(Format('IfNot Count%d Jump @RepeatEnd%d', [Index, Index]));
  AOutPut.Add(Format('Dec Count%d', [Index]));

  do_VisitNodesAndCompile(AOutPut);

  AOutPut.Add(Format('Jump @RepeatStart%d', [Index]));
  AOutPut.Add(Format('@RepeatEnd%d', [Index]));
  AOutPut.Add('');
end;

function TNodeRepeat.IsMyTurn: boolean;
begin
  Result := FContext.CurrentToken.IsSameToken('repeat');
end;

procedure TNodeRepeat.Parse;
var
  TokenInfo : TTokenInfo;
begin
  {$IFDEF DEBUG}
    OutputDebugString('TNodeRepeat.Parse');
  {$ENDIF}

  FContext.SkipToken('repeat');

  while not FContext.EOF do begin
    if not FindAppropriateCommandAndParse then Break;
  end;

  {$IFDEF DEBUG}
    OutputDebugString(PChar(Format('%s.Parse: FNodes.Count = %d', [ClassName, FNodes.Count])));
  {$ENDIF}
  
  FContext.SkipToken('for');

  TokenInfo := FContext.SkipToken(ttNumber);
  RepeatTimes := StrToInt(TokenInfo.Text);

  FContext.SkipToken('times');
  FContext.SkipToken(';');
end;

end.
