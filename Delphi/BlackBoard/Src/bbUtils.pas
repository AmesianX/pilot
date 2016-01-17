unit bbUtils;

interface

uses
  Messages, Graphics;

const
  bbBlockSize : Integer = 32;

const
  WM_FINISHWORK = WM_USER + 1;
  WM_ALLFINISH = WM_USER + 2;
  WM_CAPTURECOMPLETE = WM_USER + 3;

type //Event
  TImageEvent = procedure (Sender : TObject; Image : TBitmap) of object;

type //Record
  TbbARGB = packed record
    B : Byte;
    G : Byte;
    R : Byte;
    A : Byte;
  end;
  PbbARGB = ^TbbARGB;

  Tbb32bitBlock = record
    X : Integer;
    Y : Integer;
    Buffer : Array [0 .. 32 * 32 - 1] of TbbARGB;

    function ScanLine(X : Integer):PbbARGB;
  end;
  Pbb32bitBlock = ^Tbb32bitBlock;
  
implementation

{ Tbb32bitBlock }

function Tbb32bitBlock.ScanLine(X: Integer): PbbARGB;
begin
  Result := @Buffer[X * bbBlockSize];
end;

end.
