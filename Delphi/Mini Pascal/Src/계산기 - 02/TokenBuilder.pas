unit TokenBuilder;

interface

uses
  ParserUtils, Tokens, TokenList,
  Classes, SysUtils;

type
  TTokenBuilder = class
  private
    FTokenList : TTokenList;
    procedure do_CreateTokens;
    procedure do_ReleaseTokens;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken;
  end;  

implementation

{ TTokenBuilder }

constructor TTokenBuilder.Create;
begin
  inherited;

  FTokenList := TTokenList.Create;
  
  do_CreateTokens;
end;

destructor TTokenBuilder.Destroy;
begin
  do_ReleaseTokens;
  
  FreeAndNil(FTokenList);

  inherited;
end;

procedure TTokenBuilder.do_CreateTokens;
begin
  FTokenList.Add(TLeftParen.Create(ttWhiteSpace, ''));
  FTokenList.Add(TNumber.Create(ttWhiteSpace, ''));
  FTokenList.Add(TOperatorPower.Create(ttWhiteSpace, ''));
  FTokenList.Add(TOperatorMul.Create(ttWhiteSpace, ''));
  FTokenList.Add(TOperatorDiv.Create(ttWhiteSpace, ''));
  FTokenList.Add(TOperatorPlus.Create(ttWhiteSpace, ''));
  FTokenList.Add(TOperatorMinus.Create(ttWhiteSpace, ''));
  FTokenList.Add(TRightParen.Create(ttWhiteSpace, ''));
  FTokenList.Add(TFunction.Create(ttWhiteSpace, ''));
end;

procedure TTokenBuilder.do_ReleaseTokens;
var
  Loop: Integer;
begin
  for Loop := 0 to FTokenList.Count-1 do FTokenList.Items[Loop].Free;
  FTokenList.Clear;
end;

function TTokenBuilder.Execute(AIndex:integer; ATokenType: TScannerTokenType;
  AText: string): TToken;
var
  Loop: Integer;
begin
  Result := nil;

  if LowerCase(AText) = 'pi' then begin
    Result := TNumber.Create(ATokenType, FloatToStr(Pi));
    Exit;
  end;
  
  for Loop := 0 to FTokenList.Count-1 do begin
    if FTokenList.Items[Loop].IsMyType(ATokenType, AText) then
      Result := FTokenList.Items[Loop].Clone(AIndex, ATokenType, AText);
  end;
end;

end.
