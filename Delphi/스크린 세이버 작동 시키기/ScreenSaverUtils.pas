(*
   * Author : Ryu
   * Web Site : http://www.codeway.co.kr/
   * Date : 2007.02.06 (yyyy.mm.dd)
   * License : free
*)

unit ScreenSaverUtils;

interface

uses
  Classes, SysUtils, Windows, Registry;

function GetSystemPath:string;
procedure SetScreenSaverOn(FileName:string; Interval:integer; NeedPassword:boolean);
procedure SetScreenSaverOff;
  
implementation

function GetSystemPath:string;
var
   Buff : packed Array [0..144] of Char;
begin
  GetSystemDirectory(Buff, SizeOf(Buff));
  Result:= StrPas(Buff);
  if Result[Length(Result)] <> '\' then Result:= Result+'\';
end;

procedure SetScreenSaverOn(FileName:string; Interval:integer; NeedPassword:boolean);
var
  Reg : TRegistry;
begin
  if Interval < 60 then raise Exception.Create('Interval은 60보다 커야 합니다.');  

  Reg:= TRegistry.Create;
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    Reg.OpenKey('Control Panel\Desktop', true);
    Reg.WriteString('ScreenSaveTimeOut', IntToStr(Interval));
    Reg.WriteString('SCRNSAVE.EXE', FileName);

//    Reg.WriteString('ScreenSaverIsSecure', IntToStr(Byte(NeedPassword)))
    if NeedPassword then
      Reg.WriteString('ScreenSaverIsSecure', '1')
    else
      Reg.WriteString('ScreenSaverIsSecure', '0');

    Reg.WriteString('ScreenSaveActive', '1');
    Reg.CloseKey;
  finally
    Reg.Free;
  end;

  SystemParametersInfo(SPI_SETSCREENSAVETIMEOUT, Interval, nil, 0);
  SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, 0, nil, 0);
  ChangeDisplaySettings(TDevMode(Nil^), CDS_UPDATEREGISTRY);
end;

procedure SetScreenSaverOff;
var
  Reg : TRegistry;
begin
  Reg:= TRegistry.Create;
  try
    Reg.RootKey:= HKEY_CURRENT_USER;
    Reg.OpenKey('Control Panel\Desktop', true);
    Reg.DeleteValue('SCRNSAVE.EXE');
    Reg.WriteString('ScreenSaverIsSecure', '0');
    Reg.WriteString('ScreenSaveActive', '0');
    Reg.CloseKey;
  finally
    Reg.Free;
  end;

  SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, 0, nil, 0);
  ChangeDisplaySettings(TDevMode(Nil^), CDS_UPDATEREGISTRY);
end;

end.
