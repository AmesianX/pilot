unit MegaCastUtils;

interface

uses
  Windows, Classes, SysUtils, Graphics;

const
  _CellSize = 128;
  _BlockSize = 32;
  _PixelFormat = pf24bit;
  _PixelSize = 3;
  _BlockLineSize = _BlockSize * _PixelSize; // 블록의 이미지의 한 줄의 데이터 크기
  _MagaCastPortOffset = 60000;

type
  TDataArray = array [0.._CellSize-1, 0.._CellSize-1] of pointer;
  TSizeArray = array [0.._CellSize-1, 0.._CellSize-1] of integer;
  PDataArray = ^TDataArray;
  PSizeArray = ^TSizeArray;

type
  TFrameSize = packed record
    Width, Height : word;
  end;

  function FrameSize(AWidth,AHeight:word):TFrameSize;

type  
  (*
    * [MegaCastPacket] = [DataSize][MegaCastPacketData]
    * [MegaCastPacketData] = [Command][etc]
      - Command = mccNeedFromServer|mccData|mccEndFromClient|mccNeedScreenShot|mccFrameSize]
    * [etc]
      - [mccNeedFromServer] | [mccEndFromClient] | [mccNeedScreenShot]
      - [mccData][BlockUnit]
      - [mccFrameSize][FrameSize]
    * [BlockUnit] = [BlockUnitHeader][BlockData]
    * [BlockUnitHeader] = [X][Y][BlockType]
    * [FrameSize] = [Width][Height]
  *)

  TMegaCastCommand = (mccNeedFromServer, mccBlockUnit, mccEndFromClient, mccNeedScreenShot, mccFrameSize);
  TMegaCastBlockType = (mcdtBitmap, mcdtPBlock, mcdtZBlock, mcdtJBlock);

  TMegaCastBlockUnitHeader = packed record
    X,Y : word;
    BlockType : TMegaCastBlockType;
  end;

  TMegaCastPacketHeader = packed record
    Command : TMegaCastCommand;
    BlockHeader : TMegaCastBlockUnitHeader;
  end;

  TMegaCastFrameSize = packed record
    Command : TMegaCastCommand;
    FrameSize : TFrameSize;
  end;

  TMegaCastDataHeader = packed record
    Tick : DWord;
    Command : TMegaCastCommand;
    BlockHeader : TMegaCastBlockUnitHeader;
  end;

  TNewBlockUnitEvent = procedure (Sender:TObject; ABlockUnit:pointer; AUnitSize:integer) of object;
  TFrameSizeChangedEvent = procedure (Sender:TObject; AFrameSize:TFrameSize) of object;

const
  _BlockUnitSize = _BlockSize * _BlockLineSize + SizeOf(TMegaCastBlockUnitHeader);

  procedure BlockUnitToMegaCastPacket(BlockUnit:pointer; UnitSize:integer;
    var AData:pointer; var ASize:integer);

  procedure MegaCastPacketToBlockUnit(AData:pointer; ASize:integer;
    var BlockUnit:pointer; var UnitSize:integer);

//
procedure DrawBlockOnBitmap(ABlockData:pointer; ABlockSize:integer; AX,AY:integer; ABitmap:TBitmap); overload;
procedure DrawBlockOnBitmap(ABlockUnit:pointer; AUnitSize:integer; ABitmap:TBitmap); overload;

implementation

function FrameSize(AWidth,AHeight:word):TFrameSize;
begin
  Result.Width  := AWidth;
  Result.Height := AHeight;
end;

procedure BlockUnitToMegaCastPacket(BlockUnit:pointer; UnitSize:integer;
  var AData:pointer; var ASize:integer);
var
  pHeader : ^TMegaCastPacketHeader;
  pBlockData : ^byte;
begin
  ASize := UnitSize + SizeOf(TMegaCastPacketHeader) - SizeOf(TMegaCastBlockUnitHeader);

  GetMem(AData, ASize);

  pHeader := AData;
  pHeader^.Command := mccBlockUnit;

  pBlockData := AData;
  Inc(pBlockData, SizeOf(TMegaCastPacketHeader) - SizeOf(TMegaCastBlockUnitHeader));
  Move(BlockUnit^, pBlockData^, UnitSize);
end;

procedure MegaCastPacketToBlockUnit(AData:pointer; ASize:integer;
  var BlockUnit:pointer; var UnitSize:integer);
var
  pBlockUnit : ^byte;
begin
  UnitSize := ASize - (SizeOf(TMegaCastPacketHeader) - SizeOf(TMegaCastBlockUnitHeader));
  Assert(UnitSize >= 0, 'MegaCastPacketToBlockUnit: UnitSize가 0보다 이상이어야 합니다.');

  if UnitSize = 0 then begin
    BlockUnit := nil;
  end else begin
    pBlockUnit := AData;
    Inc(pBlockUnit, SizeOf(TMegaCastPacketHeader) - SizeOf(TMegaCastBlockUnitHeader));
    BlockUnit := pBlockUnit;
  end;
end;

procedure DrawBlockOnBitmap(ABlockData:pointer; ABlockSize:integer; AX,AY:integer; ABitmap:TBitmap);
var
  pBlock, pScanLine : ^byte;
  Line, Index : integer;
begin
  Line := _BlockLineSize*_BlockSize;
//  Assert(ABlockSize=Line, 'DrawBlockOnBitmap: 데이터 크기 에러');

  pBlock := ABlockData;

  for Line := 0 to _BlockSize-1 do begin
    pScanLine := ABitmap.ScanLine[ABitmap.Height-1];

    Index := AX*_BlockSize*_PixelSize + (AY*_BlockSize + Line)*ABitmap.Width*_PixelSize;
    Inc(pScanLine, Index);

    Move(pBlock^, pScanLine^, _BlockLineSize);

    Inc(pBlock, _BlockLineSize);
  end;
end;

procedure DrawBlockOnBitmap(ABlockUnit:pointer; AUnitSize:integer; ABitmap:TBitmap);
var
  X, Y : integer;
  pBlock : ^byte;
  pHeader : ^TMegaCastBlockUnitHeader;
begin
  pHeader := ABlockUnit;
  X := pHeader^.X;
  Y := pHeader^.Y;

  pBlock := ABlockUnit;
  Inc(pBlock, SizeOf(TMegaCastBlockUnitHeader));

  DrawBlockOnBitmap(pBlock, AUnitSize-SizeOf(TMegaCastBlockUnitHeader), X, Y, ABitmap);
end;

end.

