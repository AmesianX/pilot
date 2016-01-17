unit PolishParser;

interface

uses
  ParserUtils, Tokens, TokenList, 
  Classes, SysUtils;

type
  TPolishParser = class
  private
    FStack : TTokenList;
    procedure do_Error(AIndex,ASize:integer; AErrorMsg:string);
    function pop_LastStackItem:TToken;
    procedure push_Stack(AToken:TToken);
    procedure do_Push(AToken:TToken);
  private
    function GetResult: extended;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(AList:TTokenList);

    property Result : extended read GetResult;
  end;  

implementation

{ TPolishParser }

procedure TPolishParser.Clear;
var
  Loop: Integer;
begin
  for Loop := 0 to FStack.Count-1 do FStack.Items[Loop].Free;
  FStack.Clear;
end;

constructor TPolishParser.Create;
begin
  inherited;

  FStack := TTokenList.Create;
end;

destructor TPolishParser.Destroy;
begin
  Clear;
  FreeAndNil(FStack);

  inherited;
end;

procedure TPolishParser.do_Error(AIndex,ASize: integer; AErrorMsg: string);
begin
  raise EParsingError.Create(AErrorMsg, AIndex, ASize);
end;

procedure TPolishParser.do_Push(AToken:TToken);
var
  Token : TToken;
  Left, Right, Result : extended;
begin
  if AToken is TFunction then begin
    Token := pop_LastStackItem;
    if Token = nil then do_Error(AToken.Index, AToken.Size, '연산자와 대응되는 숫자의 개수가 잘못 되었습니다.');
    if not (Token is TNumber) then do_Error(Token.Index, Token.Size, '숫자가 와야할 자리입니다.');

    Result := 0;
    try
      Result := TFunction(AToken).Calc(TNumber(Token).Value);
    except
      do_Error(AToken.Index, AToken.Size, Format('(%s)함수가 존재하지 않습니다.', [AToken.Text]));
    end;

    push_Stack(TNumber.Create(ttNumber, FloatToStr(Result)));
  end else if AToken is TOperator then begin
    Token := pop_LastStackItem;
    if Token = nil then do_Error(AToken.Index, AToken.Size, '연산자와 대응되는 숫자의 개수가 잘못 되었습니다.');
    if not (Token is TNumber) then do_Error(Token.Index, Token.Size, '숫자가 와야할 자리입니다.');
    Right := TNumber(Token).Value;

    Token := pop_LastStackItem;
    if Token = nil then do_Error(AToken.Index, AToken.Size, '연산자와 대응되는 숫자의 개수가 잘못 되었습니다.');
    if not (Token is TNumber) then do_Error(Token.Index, Token.Size, '숫자가 와야할 자리입니다.');
    Left  := TNumber(Token).Value;

    push_Stack(TNumber.Create(ttNumber, FloatToStr(TOperator(AToken).Calc(Left, Right))));
  end else begin
    push_Stack(AToken);
  end;
end;

procedure TPolishParser.Execute(AList: TTokenList);
var
  Loop: Integer;
begin
  Clear;
  for Loop := 0 to AList.Count-1 do do_Push(AList.Items[Loop].Copy);
end;

function TPolishParser.GetResult: extended;
begin
  Result := 0;
  if FStack.Count = 0 then Exit;
  
  if FStack.Count > 1 then do_Error(0, 0, '연산자와 대응되는 숫자의 개수가 잘못 되었습니다.');

  Result := TNumber(FStack.Items[0]).Value;
end;

function TPolishParser.pop_LastStackItem: TToken;
begin
  if FStack.Count = 0 then Result := nil
  else begin
    Result := FStack.Items[FStack.Count-1];
    FStack.Delete(FStack.Count-1);
  end;
end;

procedure TPolishParser.push_Stack(AToken:TToken);
begin
  FStack.Add(AToken);
end;

end.
