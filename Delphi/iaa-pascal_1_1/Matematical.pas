{
Wykorzystano kod napisany przez Deti - http://4programmers.net/article.php?id=545
}


unit Matematical;

interface

uses
  SysUtils, Math;

  function AtomIntoValue(Input: string): string;

implementation

const
  H_NULL = '';
  H_PRECISION = '0.00000000000000000000'; // Dok≥adnoúÊ obliczeÒ
  H_RESULT = '0.###'; // Zaokrπglenie wyniku
  H_POINT = ',';
  H_B1 = '('; // Znak otwarcia nawiasa - start nowego "atomu"
  H_B2 = ')'; // Znak zamkniÍcia nawiasa - koniec "atomu"
  H_PI = 3.14159265358979323846; // WartoúÊ liczby Pi
  //H_ERROR = 'B≥πd sk≥adni'; // B≥πd

  Numbers: set of char = ['0'..'9']; // Cyfry
  Letters: set of char = ['a'..'z', 'A'..'Z', '≥', 'π', 'Í', 'ú', 'Ê', 'Ò', 'ü', 'ø', 'Û', '£', '•', ' ', 'å', '∆', '—', 'è', 'Ø', '”']; // Litery alfabetu
  BasicOpr: array [1..5] of Char = ('^', '*', '/', '+', '-'); // Podstawowe operatory matematyczne
  (* Powyøsza kolejnoúÊ elementÛw w tablicy decyduje o kolejnoúci wykonywania dzia≥aÒ! *)
  MathFunc: array [1..11] of string = ('arcsin', 'arccos', 'arctg', 'arcctg', 'sin', 'cos', 'tg', 'ctg', 'ln', 'exp', 'sqrt'); // Funkcje
  Atom_Begin: array [1..2] of Char = ('[', '{'); // Nawiasy otwierajπce
  Atom_End: array [1..2] of Char = (']', '}'); // Nawiasy zamykajπce

function Optimize(Input: string): string;
var
   i: Integer;
begin
   Input := Trim(AnsiLowerCase(Input));
   Input := StringReplace(Input, ' ', H_NULL, [rfReplaceAll]);
   for i := Low(Atom_Begin) to High(Atom_Begin) do
      Input := StringReplace(Input, Atom_Begin[i], H_B1, [rfReplaceAll]);
   for i := Low(Atom_End) to High(Atom_End) do
      Input := StringReplace(Input, Atom_End[i], H_B2, [rfReplaceAll]);
   Input := StringReplace(Input, '.', ',', [rfReplaceAll]);
   Result := Input;
end;

function Valid(Input: string): Boolean;
var
   i, B1_Count, B2_Count: Integer;
begin
   Result := True;
   B1_Count := 0;
   B2_Count := 0;
   for i := 1 to length(Input) do
     if Input[i] = H_B1 then
        Inc(B1_Count)
     else if Input[i] = H_B2 then
        Inc(B2_Count);
   Result := Boolean(B1_Count = B2_Count);

   for i := Low(MathFunc) to High(MathFunc) do
      Input := StringReplace(Input, MathFunc[i], H_NULL, [rfReplaceAll]);
   for i := 1 to length(Input) do
      if Input[i] in Letters then
        Result := False;
end;

function RightValue(Input: string): string;
var
   i: Integer;
begin
   Result := H_NULL;
   for i := 1 to length(Input) do
   begin
      if (Input[i] in Numbers) or (Input[i] = H_POINT) or ((Input[i] = '-') and (i = 1)) then
        Result := Result + Input[i]
      else
        Break;
   end;
end;

function LeftValue(Input: string): string;
var
   i: Integer;
   Reverse: string;
begin
   Reverse := H_NULL;
   Result := H_NULL;
   for i := length(Input) downto 1 do
   begin
     if (Input[i] in Numbers) or (Input[i] = H_POINT) then
        Reverse := Reverse + Input[i]
     else if (Input[i] = '-') then
     begin
        if i < length(Input) then
           if Input[i + 1] in Numbers then
           begin
              Reverse := Reverse + Input[i];
              Break;
           end;
        end
     else
        Break;
     end;
     for i := length(Reverse) downto 1 do
        Result := Result + Reverse[i];
end;

function GetBracket(Input: string): Integer;
var
   i: Integer;
   Counter: ShortInt;
begin
   Counter := 0;
   for i := 1 to length(Input) do
   begin
      if Input[i] = H_B1 then
         Counter := Counter + 1
      else if (Input[i] = H_B2) and (Counter > 0) then
         Counter := Counter - 1
      else if (Input[i] = H_B2) and (Counter = 0) then
      begin
         Result := i;
         Break;
      end;
   end;
end;

function FunctionAtomize(Value: string; FunctionIndex: Integer): string;
var
   V, Return: Extended;
begin
   V := StrToFloat(Value);
   case FunctionIndex of
      1: Return := Arcsin(V); // Sinus [ dla radiana ]
      2: Return := Arccos(V); // Cosinus [ dla radiana ]
      3: Return := Arctan(V); // Tangens [ dla radiana ]
      //4: Return := Arccot(V); // Cotangens [ dla radiana ]
      5: Return := Sin(V); // Arcus sinus [ dla radiana ]
      6: Return := Cos(V); // Arcus cosinus [ dla radiana ]
      7: Return := Tan(V); // Arcus tangens [ dla radiana ]
      8: Return := Cotan(V); // Arcus cotangens [ dla radiana ]
      9: Return := Ln(V); // Logarytm naturalny
      10: Return := Exp(V); // Exponent
      11: Return := sqrt(V); // Pierwiastek
      // Moøna dodaÊ nowe funkcje
   end;
   Result := FormatFloat(H_PRECISION, Return);
end;

function ValueAtomize(Value1, Value2: string; FunctionOperator: Char): string;
var
   V1, V2, Return: Extended;
begin
   V1 := StrToFloat(Value1);
   V2 := StrToFloat(Value2);
   case FunctionOperator of
      '+': Return := V1 + V2;
      '-': Return := V1 - V2;
      '*': Return := V1 * V2;
      '/': Return := V1 / V2;
      '^': Return := Power(V1, V2);
   end;
   Result := FormatFloat(H_PRECISION, Return);
end;

function SimplyCount(Input: string): string;
var
   i: Integer;
   Value: string;
begin
   while pos('--', Input) > 0 do
      Input := StringReplace(Input, '--', '+', []);
   for i := Low(MathFunc) to High(MathFunc) do
   begin
      while pos(MathFunc[i], Input) > 0 do
      begin
         Value := RightValue(Copy(Input, pos(MathFunc[i], Input) + length(MathFunc[i]), length(Input) - pos(MathFunc[i], Input) - length(MathFunc[i]) + 1));
         Input := StringReplace(Input, MathFunc[i] + Value, FunctionAtomize(Value, i), []);
         while pos('--', Input) > 0 do
            Input := StringReplace(Input, '--', '+', []);
      end;
      Result := Input;
   end;
end;

function OnlyBasic(Input: string): string;
var
   Value1, Value2: string;
   i: Integer;
begin
    while pos('--', Input) > 0 do
       Input := StringReplace(Input, '--', '+', []);
    for i := Low(BasicOpr) to High(BasicOpr) do
       while pos(BasicOpr[i], Input) > 1 do
       begin
          Value1 := LeftValue(Copy(Input, 1, pos(BasicOpr[i], Input) - 1));
          Value2 := RightValue(Copy(Input, pos(BasicOpr[i], Input) + 1, length(Input) - pos(BasicOpr[i], Input)));
          Input := StringReplace(Input, Value1 + BasicOpr[i] + Value2, ValueAtomize(Value1, Value2, BasicOpr[i]), []);
          while pos('--', Input) > 0 do
             Input := StringReplace(Input, '--', '+', []);
       end;
    Result := FormatFloat(H_PRECISION, StrToFloat(Input));
end;

function AtomIntoValue(Input: string): string;
begin
    while pos(H_B1, Input) > 0 do
       Input := StringReplace(Input, H_B1 + Copy(Input,  pos(H_B1, Input) + 1, GetBracket(Copy(Input, pos(H_B1, Input) + 1, length(Input) - pos(H_B1, Input))) - 1) + H_B2, AtomIntoValue(Copy(Input, pos(H_B1, Input) + 1, GetBracket(Copy(Input, pos(H_B1, Input) + 1, length(Input) - pos(H_B1, Input))) - 1)), []);
    Result := SimplyCount(Input);
    Result := OnlyBasic(Result);
end;

end.
