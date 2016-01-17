unit Functions;

interface

uses
  ParserUtils,
  Classes, SysUtils, Math;

type
  TFunctions = class (TComponent)
  private
  public
    class function Obj:TFunctions;
    function Calc(AName:string; AValue:Extended):Extended;
  published
    function _Degree(x:Extended):Extended;
    function _Sin(x:Extended):Extended;
    function _Cos(x:Extended):Extended;
    function _Tan(x:Extended):Extended;
    function _ArcSin(x:Extended):Extended;
    function _ArcCos(x:Extended):Extended;
    function _ArcTan(x:Extended):Extended;
    function _Ln(x:Extended):Extended;
    function _Log(x:Extended):Extended;
    function _Log2(x:Extended):Extended;
  end;

implementation

var
  MyObject : TFunctions = nil;

{ TFunctions }

class function TFunctions.Obj: TFunctions;
begin
  if MyObject = nil then MyObject := TFunctions.Create(nil);
  Result := MyObject;
end;

function TFunctions._ArcCos(x: Extended): Extended;
begin
  Result := ArcCos(x);
end;

function TFunctions._ArcSin(x: Extended): Extended;
begin
  Result := ArcSin(x);
end;

function TFunctions._ArcTan(x: Extended): Extended;
begin
  Result := ArcTan(x);
end;

function TFunctions.Calc(AName: string; AValue: Extended): Extended;
var
  Proc : function (x:Extended):Extended of object;
begin
  TMethod(Proc).Data := Self;
  TMethod(Proc).Code := Self.MethodAddress('_'+AName);

  if not Assigned(Proc) then
    raise Exception.Create(Format('TFunctions.Calc: (%s)메소드를 찾을 수가 없습니다.', [AName]));

  Result := Proc(AValue);
end;

function TFunctions._Cos(x: Extended): Extended;
begin
  Result := Cos(x);
end;

function TFunctions._Degree(x: Extended): Extended;
begin
  Result := x * Pi / 180;
end;

function TFunctions._Ln(x: Extended): Extended;
begin
  Result := Ln(x);
end;

function TFunctions._Log(x: Extended): Extended;
begin
  Result := Log10(x)
end;

function TFunctions._Log2(x: Extended): Extended;
begin
  Result := Log2(x);
end;

function TFunctions._Sin(x: Extended): Extended;
begin
  Result := Sin(x);
end;

function TFunctions._Tan(x: Extended): Extended;
begin
  Result := Tan(x);
end;

end.
