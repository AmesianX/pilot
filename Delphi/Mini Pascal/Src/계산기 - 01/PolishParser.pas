unit PolishParser;

interface

uses
  ValueList,
  Classes, SysUtils;

type
  TPolishParser = class
  private
    FStack : TValueList;
    procedure do_Calc(AText:string; ALeft,ARight:double);
    function pop_LastStackItem:string;
    procedure push_Stack(AText:string);
  private
    function GetResult: double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Push(AText:string);
    procedure Assign(AList:TStringList);

    property Result : double read GetResult;
  end;  

implementation

{ TPolishParser }

procedure TPolishParser.Assign(AList: TStringList);
var
  Loop: Integer;
begin
  Clear;
  for Loop := 0 to AList.Count-1 do Push(AList[Loop]);
end;

procedure TPolishParser.Clear;
begin
  FStack.Clear;
end;

constructor TPolishParser.Create;
begin
  inherited;

  FStack := TValueList.Create;
  FStack.LineBreak := sLineBreak;
end;

destructor TPolishParser.Destroy;
begin
  FreeAndNil(FStack);

  inherited;
end;

procedure TPolishParser.do_Calc(AText: string; ALeft, ARight: double);
var
  Temp : double;
begin
       if AText = '+' then Temp := ALeft + ARight
  else if AText = '-' then Temp := ALeft - ARight
  else if AText = '*' then Temp := ALeft * ARight
  else if AText = '/' then Temp := ALeft / ARight
  else raise Exception.Create('TPolishParser.do_Calc: ');

  push_Stack(FloatToStr(Temp));
end;

function TPolishParser.GetResult: double;
begin
  if FStack.Count = 0 then Result := 0
  else Result := StrToFloat(FStack[0]);
end;

function TPolishParser.pop_LastStackItem: string;
begin
  if FStack.Count = 0 then Result := ''
  else begin
    Result := FStack[FStack.Count-1];
    FStack.Delete(FStack.Count-1);
  end;
end;

procedure TPolishParser.Push(AText: string);
var
  Left, Right : double;
begin
  if Trim(AText) = ''  then Exit;
  
  if Pos(AText, '+ - * /') > 0  then begin
    Right := StrToFloat(pop_LastStackItem);
    Left  := StrToFloat(pop_LastStackItem);
    do_Calc(AText, Left, Right);
  end else begin
    push_Stack(AText);
  end;
end;

procedure TPolishParser.push_Stack(AText: string);
begin
  FStack.Add(AText);
end;

end.
