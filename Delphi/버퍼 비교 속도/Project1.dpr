program Project1;

{$APPTYPE CONSOLE}

uses
  SysUtils, Windows, CompareBytes;

type
  TItem = packed array [0..1023] of byte;
  TBuffer = packed array [0..$FFFFF] of TItem;
  PBuffer = ^TBuffer;

var
  X : integer;
  Y : ^integer;
  Loop, ExTick : integer;
  Buffer1, Buffer2 : packed array [0..$FFFF] of TItem;

procedure Inc(var X:integer);
asm
  mov eax, X
  mov edx, [eax]
  inc edx
  mov [eax], edx
end;

procedure Inc2(var X:integer);
begin
  asm
    mov eax, X
    mov edx, [eax]
    inc edx
    mov [eax], edx
  end;
end;

begin
  FillChar(Buffer1, $FFFFF+1, 0);
  FillChar(Buffer2, $FFFFF+1, 0);

  ExTick:= GetTickCount;
  for Loop := 0 to $FFFF do
    CompareMem(@Buffer1[Loop], @Buffer2[Loop], 1024);
  WriteLn('CompareMem : ', GetTickCount-ExTick);

  ExTick:= GetTickCount;
  for Loop := 0 to $FFFF do
    CompareFastBytes(@Buffer1[Loop], @Buffer2[Loop], 1024);
  WriteLn('CompareFastBytes : ', GetTickCount-ExTick);

  X:= 0;
  ExTick:= GetTickCount;
  for Loop := 0 to $FFFFFFF do Inc(X);
  WriteLn('Inc : ', GetTickCount-ExTick);

  X:= 0;
  ExTick:= GetTickCount;
  for Loop := 0 to $FFFFFFF do Inc2(X);
  WriteLn('Inc2 : ', GetTickCount-ExTick);

  X:= 0;
  Y:= @X;
  ExTick:= GetTickCount;
  for Loop := 0 to $FFFFFFF do
    asm
      mov eax, Y
      mov edx, [eax]
      inc edx
      mov [eax], edx
    end;
  WriteLn('asm : ', GetTickCount-ExTick);

  ReadLn;
end.
