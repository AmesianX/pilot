unit Infix2Postfix;

interface

uses
  {$IFDEF DEBUG}
  Dialogs,
  {$ENDIF}
  
  ParserUtils, Tokens, TokenList,
  Classes, SysUtils;

type
  TInfix2Postfix = class
  private
    FStack : TTokenList;
    FPolish : TTokenList;
    FNullToken : TToken;
    FLastToken : TToken;
    procedure do_Error(AIndex,ASize:integer; AErrorMsg:string);
    function is_Sign(AToken:TToken):boolean;
    function is_StackEmpty:boolean;
    function is_PolishEmpty:boolean;
    function get_LastStackItem:TToken;
    function pop_LastStackItem:TToken;
    function check_LowPriority(AToken:TToken):boolean;
    procedure push_Stack(AToken:TToken);
    procedure pop_Stack(AToken:TToken);
    procedure push_Polish(AToken:TToken);
    procedure do_Push(AToken:TToken);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Push(AToken:TToken);
    procedure EndOfLine;

    property Polish : TTokenList read FPolish;
  end;

implementation

{ TInfix2Postfix }

function TInfix2Postfix.check_LowPriority(AToken:TToken): boolean;
var
  LastStackItem : TToken;
begin
  if is_StackEmpty then begin
    Result := false;
    Exit;
  end;

  LastStackItem := get_LastStackItem;

  // '('�� �ٷ� ���̸� �켱������ ������� pop ���� �ʴ´�.
  if LastStackItem is TLeftParen then begin
    // '("�� �ٷ� ���̶� ������ ')'�� POP�� �Ѵ�.
    Result := AToken is TRightParen;
    Exit;
  end;

  Result := AToken.Priority <= LastStackItem.Priority;
end;

procedure TInfix2Postfix.Clear;
var
  Loop: Integer;
begin
  FLastToken := FNullToken;

  for Loop := 0 to FStack.Count-1 do FStack.Items[Loop].Free;
  for Loop := 0 to FPolish.Count-1 do FPolish.Items[Loop].Free;

  FStack.Clear;
  FPolish.Clear;
end;

constructor TInfix2Postfix.Create;
begin
  inherited;

  FNullToken := TToken.Create(ttWhiteSpace, '');
  FStack := TTokenList.Create;
  FPolish := TTokenList.Create;

  FLastToken := FNullToken;
end;

destructor TInfix2Postfix.Destroy;
begin
  FreeAndNil(FNullToken);
  FreeAndNil(FStack);
  FreeAndNil(FPolish);

  inherited;
end;

procedure TInfix2Postfix.do_Error(AIndex,ASize:integer; AErrorMsg:string);
begin
  raise EParsingError.Create(AErrorMsg, AIndex, ASize);
end;

procedure TInfix2Postfix.EndOfLine;
{$IFDEF DEBUG}
var
  Loop: Integer;
  sTemp : string;
{$ENDIF}
begin
  while not is_StackEmpty do pop_Stack(nil);

  {$IFDEF DEBUG}
  for Loop := 0 to Polish.Count-1 do sTemp := sTemp + Polish.Items[Loop].Text + #13#10;
  ShowMessage(sTemp);
  {$ENDIF}
end;

function TInfix2Postfix.get_LastStackItem: TToken;
begin
  if FStack.Count = 0 then Result := FNullToken
  else Result := FStack.Items[FStack.Count-1];
end;

function TInfix2Postfix.is_PolishEmpty: boolean;
begin
  Result := FPolish.Count = 0;
end;

// ���ϱ� ���Ⱑ �ƴ� ��ȣ�� ó���ؾ� �� ���
// (������ ��� �ִµ�, ( �ٷ� ����, ������ ����)�� +, - ��ū�� �� ���
function TInfix2Postfix.is_Sign(AToken: TToken): boolean;
var
  isCondition : boolean;
begin
  isCondition := (AToken is TOperatorPlus) or (AToken is TOperatorMinus);
  if not isCondition then begin
    Result := false;
    Exit;
  end;

  Result := is_StackEmpty and is_PolishEmpty;
  if Result then Exit;

  Result :=
    (FLastToken is TLeftParen) or
    (FLastToken is TOperator);
end;

function TInfix2Postfix.is_StackEmpty: boolean;
begin
  Result := FStack.Count = 0;
end;

function TInfix2Postfix.pop_LastStackItem: TToken;
begin
  if FStack.Count = 0 then Result := FNullToken
  else begin
    Result := FStack.Items[FStack.Count-1];
    FStack.Delete(FStack.Count-1);
  end;
end;

procedure TInfix2Postfix.pop_Stack(AToken:TToken);
var
  LastStackItem : TToken;
begin
  if AToken = nil then begin
    push_Polish(pop_LastStackItem)

  end else if AToken is TRightParen then begin
    repeat
      LastStackItem := pop_LastStackItem;
      if not (LastStackItem is TLeftParen) then push_Polish(LastStackItem);
    until is_StackEmpty or (LastStackItem is TLeftParen);

    if not (LastStackItem is TLeftParen) then do_Error(AToken.Index, AToken.Size, '���� ��ȣ�� ������ ���ڶ��ϴ�.');

  end else begin
    while (not is_StackEmpty) and check_LowPriority(AToken) do begin
      push_Polish(pop_LastStackItem);
    end;
    
    push_Stack(AToken);
  end;  
end;

procedure TInfix2Postfix.do_Push(AToken: TToken);
begin
  if is_StackEmpty and (AToken is TRightParen) then begin
    do_Error(AToken.Index, AToken.Size, '���� ��ȣ�� ������ ���ڶ��ϴ�.');
  end else if (not is_StackEmpty) and check_LowPriority(AToken) then begin
    pop_Stack(AToken)
  end else begin
    push_Stack(AToken);
  end;
end;

procedure TInfix2Postfix.Push(AToken:TToken);
begin
  // ��ü�� �����Ѵ�.  (���� �����͵� ������ ��ü)
  AToken := AToken.Copy;
  
  if is_Sign(AToken) then begin
    // + ��ȣ�� �����ص� ��, - ��ȣ�̸� -1�� ���ع�����.
    if AToken is TOperatorMinus then begin
      do_Push(TNumber.Create(ttNumber, '-1'));
      FLastToken := TOperatorMul.Create(ttSpecialChars, '*');
      do_Push(FLastToken);
    end;

    Exit;
  end;

  do_Push(AToken);
  FLastToken := AToken;
end;

procedure TInfix2Postfix.push_Polish(AToken:TToken);
begin
  if AToken is TLeftParen then do_Error(AToken.Index, AToken.Size, '������ ��ȣ�� ������ ���ڶ��ϴ�.');

  FPolish.Add(AToken);
end;

procedure TInfix2Postfix.push_Stack(AToken:TToken);
begin
  FStack.Add(AToken);
end;

end.
