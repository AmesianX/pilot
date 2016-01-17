unit LockFreeObject;

interface

uses
  SysUtils, SpinLock, Windows;

type
  TlfBufferItem = record
    Data : Pointer;
    Size : Integer;
  end;

  TLockFreeObject = class
  private
    FWriteLock : TSpinLock;
    FBufferCount : Integer;
    FPosition : Integer;
    FBufferList : Array of TlfBufferItem;
  public
    Constructor Create(BufferCount : Integer = 10);
    destructor Destroy; virtual;

    procedure SetObject(Data : Pointer; Size : Integer);
    procedure GetObject(var Data : Pointer; var Size : Integer);
  end;
implementation

{ TLockFreeObject }

constructor TLockFreeObject.Create(BufferCount : Integer);
var
  I: Integer;
begin
  FWriteLock := TSpinLock.Create;
  FBufferCount := BufferCount;
  FPosition := -1;
  SetLength(FBufferList, FBufferCount);

  for I := 0 to FBufferCount - 1 do begin
    FBufferList[i].Data := nil;
    FBufferList[i].Size := 0;
  end;
end;

destructor TLockFreeObject.Destroy;
begin
  SetLength(FBufferList, 0);
  FWriteLock.Free;
end;

procedure TLockFreeObject.GetObject(var Data: Pointer; var Size: Integer);
begin
  Data := FBufferList[FPosition].Data;
  Size := FBufferList[FPosition].Size;
end;

procedure TLockFreeObject.SetObject(Data: Pointer; Size: Integer);
var
  NewPosition : Integer;
begin
  FWriteLock.Enter;
  try
    NewPosition := (FPosition + 1) mod FBufferCount;
    if FBufferList[NewPosition].Data <> nil then FreeMem(FBufferList[NewPosition].Data);
    FBufferList[NewPosition].Data := GetMemory(Size);
    CopyMemory(FBufferList[NewPosition].Data, Data, Size);
    FBufferList[NewPosition].Size := Size;
    FPosition := NewPosition;
  finally
    FWriteLock.Leave;
  end;
end;

end.
