unit Infix2Postfix;

interface

uses
  ValueList, 
  Classes, SysUtils;

type
  TInfix2Postfix = class
  private
    FPriority : TValueList;
    FStack : TValueList;
    FPolish : TValueList;
    function is_StackEmpty:boolean;
    function get_LastStackItem:string;
    function pop_LastStackItem:string;
    function get_Priority(AText:string):integer;
    function check_LowPriority(AText:string):boolean;
    procedure push_Stack(AText:string);
    procedure pop_Stack(AText:string);
    procedure push_Polish(AText:string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Push(AText:string);
    procedure EndOfLine;

    property Polish : TValueList read FPolish;
  end;

implementation

{ TInfix2Postfix }

function TInfix2Postfix.check_LowPriority(AText: string): boolean;
begin
  if is_StackEmpty then begin
    Result := false

  // '('가 바로 앞이면 우선순위에 상관없이 pop 하지 않는다.
  end else if get_LastStackItem = '(' then begin
    Result := false

  end else begin
    Result := get_Priority(AText) <= get_Priority(get_LastStackItem);
  end;
end;

procedure TInfix2Postfix.Clear;
begin
  FStack.Clear;
  FPolish.Clear;
end;

constructor TInfix2Postfix.Create;
begin
  inherited;

  // 우선순위, 피연산자는 4에 해당한다.
  FPriority := TValueList.Create;
  FPriority.LineBreak := sLineBreak;
  FPriority.Integers['('] := 5;
  // 숫자 = 4
  FPriority.Integers['*'] := 3;
  FPriority.Integers['/'] := 3;
  FPriority.Integers['+'] := 2;
  FPriority.Integers['-'] := 2;
  FPriority.Integers[')'] := 1;

  FStack := TValueList.Create;
  FStack.LineBreak := sLineBreak;

  FPolish := TValueList.Create;
  FPolish.LineBreak := sLineBreak;
end;

destructor TInfix2Postfix.Destroy;
begin
  FreeAndNil(FPriority);
  FreeAndNil(FStack);
  FreeAndNil(FPolish);

  inherited;
end;

procedure TInfix2Postfix.EndOfLine;
begin
  while not is_StackEmpty do pop_Stack('');
end;

function TInfix2Postfix.get_LastStackItem: string;
begin
  if FStack.Count = 0 then Result := ''
  else Result := FStack[FStack.Count-1];
end;

function TInfix2Postfix.get_Priority(AText: string): integer;
begin
  Result := FPriority.Integers[AText];
  if Result = 0 then Result := 4;  
end;

function TInfix2Postfix.is_StackEmpty: boolean;
begin
  Result := FStack.Count = 0;
end;

function TInfix2Postfix.pop_LastStackItem: string;
begin
  if FStack.Count = 0 then Result := ''
  else begin
    Result := FStack[FStack.Count-1];
    FStack.Delete(FStack.Count-1);
  end;
end;

procedure TInfix2Postfix.pop_Stack(AText: string);
var
  sLastStackItem : string;
begin
  if AText = '' then begin
    push_Polish(pop_LastStackItem)

  end else if AText = ')' then begin
    repeat
      sLastStackItem := pop_LastStackItem;
      push_Polish(sLastStackItem);
    until is_StackEmpty or (sLastStackItem = '(');

  end else begin
    while (not is_StackEmpty) and check_LowPriority(AText) do begin
      push_Polish(pop_LastStackItem);
    end;
    
    push_Stack(AText);
  end;  
end;

procedure TInfix2Postfix.Push(AText: string);
begin
  if Trim(AText) = '' then Exit;

  if AText = '=' then begin
    EndOfLine;
    Exit;
  end;

  if (not is_StackEmpty) and check_LowPriority(AText) then begin
    pop_Stack(AText)
  end else begin
    push_Stack(AText);
  end;
end;

procedure TInfix2Postfix.push_Polish(AText: string);
begin
  if (AText <> '(') and (AText <> '') then FPolish.Add(AText);
end;

procedure TInfix2Postfix.push_Stack(AText: string);
begin
  FStack.Add(AText);
end;

end.
