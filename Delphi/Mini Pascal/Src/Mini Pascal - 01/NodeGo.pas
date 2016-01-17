unit NodeGo;

interface

uses
  ParserUtils, ParsingNode,
  Windows, Classes, SysUtils;

type
  TNodeGo = class (TTerminalNode)
  private
  public
    Distance : integer;
  public
    procedure Parse; override;
    function Clone:TParsingNode; override;
    function IsMyTurn:boolean; override;
    procedure Compile(AOutPut:TStringList); override;
  end;

implementation

{ TNodeGo }

function TNodeGo.Clone: TParsingNode;
begin
  Result := TNodeGo.Create(FCommands, FContext);
end;

procedure TNodeGo.Compile(AOutPut: TStringList);
begin
  AOutPut.Add(Format('Call Go %d', [Distance]));
end;

function TNodeGo.IsMyTurn: boolean;
begin
  Result := FContext.CurrentToken.IsSameToken('go');
end;

procedure TNodeGo.Parse;
var
  TokenInfo : TTokenInfo;
begin
  {$IFDEF DEBUG}
    OutputDebugString('TNodeGo.Parse');
  {$ENDIF}

  FContext.SkipToken('go');

  TokenInfo := FContext.SkipToken(ttNumber);
  Distance := StrToInt(TokenInfo.Text);

  FContext.SkipToken(';');
end;

end.
