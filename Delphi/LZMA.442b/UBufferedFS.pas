unit UBufferedFS;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, Math;

const
  BufferSize = $10000; // 64K

type
  TBFSMode = (BFMRead, BFMWrite);

  TBufferedFS = class(TFileStream)
  private
    membuffer: array [0 .. BufferSize - 1] of byte;
    bytesinbuffer: integer;
    bufferpos: integer;
    bufferdirty: boolean;
    Mode: TBFSMode;
    procedure Init;
    procedure Flush;
    procedure ReadBuffer;
  public
    constructor Create(const FileName: string; Mode: Word); overload;
    constructor Create(const FileName: string; Mode: Word;
      Rights: Cardinal); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

type
  TByteArray = array of byte;
  PByteArray = ^TByteArray;

implementation

function MovePointer(const P: pointer; const dist: integer): pointer;
begin
  result := pointer(integer(P) + dist);
end;

procedure TBufferedFS.Init;
begin
  bytesinbuffer := 0;
  bufferpos := 0;
  bufferdirty := false;
  Mode := BFMWrite;
end;

procedure TBufferedFS.Flush;
begin
  if bufferdirty then
    inherited Write(membuffer[0], bufferpos);
  bufferdirty := false;
  bytesinbuffer := 0;
  bufferpos := 0;
end;

constructor TBufferedFS.Create(const FileName: string; Mode: Word);
begin
  inherited;
  Init;
end;

constructor TBufferedFS.Create(const FileName: string; Mode: Word;
  Rights: Cardinal);
begin
  inherited;
  Init;
end;

destructor TBufferedFS.Destroy;
begin
  Flush;
  inherited;
end;

procedure TBufferedFS.ReadBuffer;
begin
  Flush;
  bytesinbuffer := inherited Read(membuffer, BufferSize);
  bufferpos := 0;
end;

function TBufferedFS.Read(var Buffer; Count: Longint): Longint;
var
  P: PByteArray;
  bytestoread: integer;
  b: integer;
begin
  if Mode = BFMWrite then
    Flush;
  Mode := BFMRead;
  result := 0;
  if Count <= bytesinbuffer then
  begin
    // all data already in buffer
    move(membuffer[bufferpos], Buffer, Count);
    bytesinbuffer := bytesinbuffer - Count;
    bufferpos := bufferpos + Count;
    result := Count;
  end
  else
  begin
    bytestoread := Count;
    if (bytestoread <> 0) and (bytesinbuffer <> 0) then
    begin
      // read data remaining in buffer and increment data pointer
      b := Read(Buffer, bytesinbuffer);
      P := PByteArray(@(TByteArray(Buffer)[b]));
      bytestoread := bytestoread - b;
      result := b;
    end
    else
      P := @Buffer;
    if bytestoread >= BufferSize then
    begin
      // data to read is larger than the buffer, read it directly
      result := result + inherited Read(P^, bytestoread);
    end
    else
    begin
      // refill buffer
      ReadBuffer;
      // recurse
      result := result + Read(P^, Math.Min(bytestoread, bytesinbuffer));
    end;
  end;
end;

function TBufferedFS.Write(const Buffer; Count: Longint): Longint;
var
  P: pointer;
  bytestowrite: integer;
  b: integer;
begin
  if Mode = BFMRead then
  begin
    Seek(-BufferSize + bufferpos, soFromCurrent);
    bytesinbuffer := 0;
    bufferpos := 0;
  end;
  Mode := BFMWrite;
  result := 0;
  if Count <= BufferSize - bytesinbuffer then
  begin
    // all data fits in buffer
    bufferdirty := true;
    move(Buffer, membuffer[bufferpos], Count);
    bytesinbuffer := bytesinbuffer + Count;
    bufferpos := bufferpos + Count;
    result := Count;
  end
  else
  begin
    bytestowrite := Count;
    if (bytestowrite <> 0) and (bytesinbuffer <> BufferSize) and
      (bytesinbuffer <> 0) then
    begin
      // write data to remaining space in buffer and increment data pointer
      b := Write(Buffer, BufferSize - bytesinbuffer);
      P := MovePointer(@Buffer, b);
      bytestowrite := bytestowrite - b;
      result := b;
    end
    else
      P := @Buffer;
    if bytestowrite >= BufferSize then
    begin
      // empty buffer
      Flush;
      // data to write is larger than the buffer, write it directly
      result := result + inherited Write(P^, bytestowrite);
    end
    else
    begin
      // empty buffer
      Flush;
      // recurse
      result := result + Write(P^, bytestowrite);
    end;
  end;
end;

function TBufferedFS.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Origin = soCurrent) and (Offset = 0) then
    result := inherited Seek(Offset, Origin) + bufferpos
  else
  begin
    Flush;
    result := inherited Seek(Offset, Origin);
  end;
end;

end.
