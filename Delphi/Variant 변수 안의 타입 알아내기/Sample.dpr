program Sample;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Variants;

procedure ShowVariantType(Valeus:Variant);
begin
  case varType(Valeus) and VarTypeMask of
//      varArray
      varBoolean : WriteLn('varBoolean');
//      varByRef
//      varCurrency
//      varDate
//      varDispatch
      varDouble : WriteLn('varDouble');
//      varEmpty
//      varError
      varInteger : WriteLn('varInteger');
//      varNull
//      varOleStr
//      varSingle
//      varSmallint
      varString : WriteLn('varString');
//      varTypeMask
//      varUnknown
//      varVariant
      else WriteLn('UnKnown');
  end;
end;

var
  i : integer = 1234;
  V : Variant;

begin
  V:= i;
  ShowVariantType(V);

  V:= '1234';
  ShowVariantType(V);

  ReadLn;
end.
