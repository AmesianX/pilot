unit RandomChoice;

interface

uses
  Classes, SysUtils;

type
  TRandomChoice = class(TStringList)
  private
    FSize: integer;
    procedure SetSize(const Value: integer);
  protected
    procedure do_Swap(Index1,Index2:integer);
  public
    constructor Create;

    procedure Init;
    function Get:integer;
  published
    property Size : integer read FSize write SetSize;
  end;  

implementation

{ TRandomChoice }

constructor TRandomChoice.Create;
begin
  FSize:= 46;
  Randomize;
end;

procedure TRandomChoice.do_Swap(Index1, Index2: integer);
var
  stTemp : string;
begin
  stTemp:= Self.Strings[Index1];
  Self.Strings[Index1]:= Self.Strings[Index2];
  Self.Strings[Index2]:= stTemp;
end;

function TRandomChoice.Get: integer;
begin
  if Self.Count < 1 then begin
    Result:= -1;
    Exit;
  end;

  Result:= StrToIntDef(Self.Strings[0], -1);
  Self.Delete(0);
end;

procedure TRandomChoice.Init;
var
  Loop, iIndex1, iIndex2 : integer;
begin
  Self.Clear;
  for Loop := 1 to 46 do Self.Add(IntToStr(Loop));

  for Loop := 1 to 100000 do begin
    iIndex1:= Round(Random(46));
    iIndex2:= Round(Random(46));
    if iIndex1 = iIndex2 then Continue;

    do_Swap(iIndex1, iIndex2);
  end;
end;

procedure TRandomChoice.SetSize(const Value: integer);
begin
  FSize := Value;
end;

end.
