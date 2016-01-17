program Quiz20060925;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  HourGlass in 'HourGlass.pas';

var
  HourGlass7 : THourGlass;

begin
  HourGlass7:= THourGlass.Create;
  HourGlass7.Interval:= 7;
  HourGlass7.UpsideDown;

  while HourGlass7.TimeLeft > 0 do begin
    WriteLn(Format('TimeLeft : %d', [HourGlass7.TimeLeft]));
    HourGlass7.Tick;
  end;

  ReadLn;
end.
