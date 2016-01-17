unit NodeTurn;

interface

uses
  ParserUtils, ParsingNode,
  Windows, Classes, SysUtils;

type
  TNodeTurn = class (TTerminalNode)
  private
  public
    Direction : string;
  public
    procedure Parse; override;
    function Clone:TParsingNode; override;
    function IsMyTurn:boolean; override;
    procedure Compile(AOutPut:TStringList); override;
  end;

implementation

{ TNodeTurn }

function TNodeTurn.Clone: TParsingNode;
begin
  Result := TNodeTurn.Create(FCommands, FContext);
end;

procedure TNodeTurn.Compile(AOutPut: TStringList);
begin
  AOutPut.Add(Format('Call Turn %s', [Direction]));
end;

function TNodeTurn.IsMyTurn: boolean;
begin
  Result := FContext.CurrentToken.IsSameToken('turn');
end;

procedure TNodeTurn.Parse;
var
  TokenInfo : TTokenInfo;
begin
  {$IFDEF DEBUG}
    OutputDebugString('TNodeTurn.Parse');
  {$ENDIF}

  FContext.SkipToken('turn');

  TokenInfo := FContext.SkipToken(ttIdentifier);
  Direction := TokenInfo.Text;

  FContext.SkipToken(';');
end;

end.
