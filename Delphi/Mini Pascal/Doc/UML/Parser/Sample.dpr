program Sample;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Base in 'Base.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
