program Factorial;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Math, SysUtils;

var
  i, d : integer;
  r, e : double;
begin
  r := 0;
  for i := 1 to 100000000 do r := r + Log10(i);

  d := Floor(r);
  e := r - d;

  WriteLn( Format('%e x 10^%d', [Power(10, e), d]) );

  ReadLn;
end.
