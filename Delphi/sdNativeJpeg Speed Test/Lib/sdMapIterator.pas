//  unit sdMapIterator
//
//  Generic 2D map iterators (can be used with any memory-based map that has at
//  least one byte of data per cell)
//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 Simdesign B.V.
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
//  Please visit http://www.simdesign.nl for more information.
//
unit sdMapIterator;

interface

uses
  Windows, SysUtils;

type

  TsdIteratorMethod = (
    imReaderX,    // From x=0  , y=0   to x=W-1, y=0  , then to x=0  , y=1   and on
    imReaderXInv, // From x=W-1, y=0   to x=0  , y=0  , then to x=W-1, y=1   and on
    imReaderXBtm, // From x=0  , y=H-1 to x=W-1, y=H-1, then to X=1  , y=H-1 and on
    imReaderY,    // From x=0  , y=0   to x=0  , y=H-1, then to x=1  , y=0   and on
    imReaderYInv, // From x=0  , y=H-1 to x=0  , y=0  , then to x=1  , y=H-1 and on
    imZigZag,     // Zig zag: top line from left to right, then next from right to left, etc
    imLineByLine, // Move to next cell to the right, until end of the scanline
    imColByCol    // Move to next cell down, until last row
  );

  TsdMapIterator = class
  private
    FCount: integer;
    FDelta: integer;
    FLine: integer;
    FFirst: pbyte;
    FThis: pbyte;
    FMap: pbyte;
    FWidth: integer;
    FHeight: integer;
    FScanStride: integer;
    FCellStride: integer;
    FMethod: TsdIteratorMethod;
    function IsDirect: boolean;
    function GetDirection: integer;
  protected
  public
    procedure Assign(Source: TsdMapIterator);
    // Call First to get a pointer to the first cell
    function First: pbyte;
    // Call Next iteratively to get the next cell, until it returns nil
    function Next: pbyte;
    // Get the pointer to the cell at X,Y. Next does *not* work from this
    // position, the First/Next paradigm cannot be mixed.
    function At(X, Y: integer): pbyte;
    // Same thing, but checks dimensions and returns nil if not within them
    function SafeAt(X, Y: integer): pbyte;
    // Increment the Map pointer so it points to a different channel (e.g.
    // 0 for Blue, 1 for Red, 2 for Green in RGB 24bpp bitmap).
    procedure IncrementMap(AChannel: integer);
    // Distance between cell 0 of scanline 0 and cell 0 of scanline 1 in bytes
    property ScanStride: integer read FScanStride write FScanStride;
    // Distance between cell 0 and cell 1
    property CellStride: integer read FCellStride write FCellStride;
    // Pointer to cell 0, 0
    property Map: pbyte read FMap write FMap;
    // Width of map in pixels
    property Width: integer read FWidth write FWidth;
    // Height of map in pixels
    property Height: integer read FHeight write FHeight;
    // Iterator method, see TsdIteratorMethod for an explanation.
    property Method: TsdIteratorMethod read FMethod write FMethod;
    // Current line number (if multi-line iteration)
    property Line: integer read FLine;
    // Current direction (if zig-zag)
    property Direction: integer read GetDirection;
  end;

  TsdMapOperation = (
    moRotate90,
    moRotate180,
    moRotate270,
    moTranspose,
    moMirror,
    moFlip,
    moCopy
  );

// Perform the operation in AOperation on the maps referenced by the Src and
// Dst iterators; putting the result in Dst. Make sure to create valid iterators
// with valid map sizes before calling this routine! (no checks)
procedure PerformMapOperation(Src, Dst: TsdMapIterator; AOperation: TsdMapOperation);

implementation

{ TsdMapIterator }

procedure TsdMapIterator.Assign(Source: TsdMapIterator);
begin
  FCount := Source.FCount;
  FDelta := Source.FDelta;
  FLine := Source.FLine;
  FFirst := Source.FFirst;
  FThis := Source.FThis;
  FMap := Source.FMap;
  FWidth := Source.FWidth;
  FHeight := Source.FHeight;
  FScanStride := Source.FScanStride;
  FCellStride := Source.FCellStride;
  FMethod := Source.FMethod;
end;

function TsdMapIterator.At(X, Y: integer): pbyte;
begin
  Result := FMap;
  inc(Result, X * FCellStride + Y * FScanStride);
end;

function TsdMapIterator.First: pbyte;
begin
  FThis := FMap;
  FLine := 0;
  case FMethod of
  imReaderX:
    begin
      if IsDirect then
        FCount := FWidth * FHeight
      else
        FCount := FWidth;
      FDelta := FCellStride;
    end;
  imReaderXInv:
    begin
      FCount := FWidth;
      FDelta := -FCellStride;
      inc(FThis, (FWidth - 1) * FCellStride);
    end;
  imReaderXBtm:
    begin
      FCount := FWidth;
      FDelta := FCellStride;
      inc(FThis, (FHeight - 1) * FScanStride);
    end;
  imReaderY, imColByCol:
    begin
      FCount := FHeight;
      FDelta := FScanStride;
    end;
  imReaderYInv:
    begin
      FCount := FHeight;
      FDelta := -FScanStride;
      inc(FThis, (FHeight - 1) * FScanStride);
    end;
  imLineByLine, imZigZag:
    begin
      FCount := FWidth;
      FDelta := FCellStride;
    end;
  else
    FThis := nil;
  end;
  FFirst := FThis;
  Result := FThis;
end;

function TsdMapIterator.GetDirection: integer;
begin
  if FDelta > 0 then
    Result := 1
  else
    Result := -1;
end;

procedure TsdMapIterator.IncrementMap(AChannel: integer);
begin
  inc(FMap, AChannel);
end;

function TsdMapIterator.IsDirect: boolean;
begin
  Result := (FCellStride * FWidth = FScanStride);
end;

function TsdMapIterator.Next: pbyte;
begin
  dec(FCount);
  if FCount > 0 then
  begin
    // increment
    inc(FThis, FDelta);
  end else
  begin
    case FMethod of
    imLineByLine, imColByCol:
      begin
        Result := nil;
        exit;
      end;
    imReaderX, imReaderXInv:
      begin
        inc(FLine);
        if (FLine = FHeight) or IsDirect then begin
          Result := nil;
          exit;
        end;
        inc(FFirst, FScanStride);
        FThis := FFirst;
        FCount := FWidth;
      end;
    imReaderXBtm:
      begin
        inc(FLine);
        if FLine = FHeight then begin
          Result := nil;
          exit;
        end;
        dec(FFirst, FScanStride);
        FThis := FFirst;
        FCount := FWidth;
      end;
    imReaderY, imReaderYInv:
      begin
        inc(FLine);
        if FLine = FWidth then begin
          Result := nil;
          exit;
        end;
        inc(FFirst, FCellStride);
        FThis := FFirst;
        FCount := FHeight;
      end;
    imZigZag:
      begin
        inc(FLine);
        if FLine = FHeight then begin
          Result := nil;
          exit;
        end;
        inc(FFirst, FScanStride);
        FThis := FFirst;
        if odd(FLine) then
          inc(FThis, FCellStride * (FWidth - 1));
        FDelta := -FDelta;
        FCount := FWidth;
      end;
    end;
  end;
  Result := FThis;
end;

procedure PerformMapOperation(Src, Dst: TsdMapIterator; AOperation: TsdMapOperation);
var
  Stride: integer;
  S, D: Pbyte;
begin
  Stride := Src.CellStride;
  case AOperation of
  moRotate90:
    begin
      Src.Method := imReaderYInv;
      Dst.Method := imReaderX;
    end;
  moRotate180:
    begin
      Src.Method := imReaderXInv;
      Dst.Method := imReaderXBtm;
    end;
  moRotate270:
    begin
      Src.Method := imReaderX;
      Dst.Method := imReaderYInv;
    end;
  moTranspose:
    begin
      Src.Method := imReaderX;
      Dst.Method := imReaderY;
    end;
  moMirror:
    begin
      Src.Method := imReaderX;
      Dst.Method := imReaderXInv;
    end;
  moFlip:
    begin
      Src.Method := imReaderX;
      Dst.Method := imReaderXBtm;
    end;
  moCopy:
    begin
      Src.Method := imColByCol;
      Dst.Method := imColByCol;
      Stride := Src.ScanStride;
      if Dst.ScanStride < Stride then
        Stride := Dst.ScanStride;
    end;
  else
    raise Exception.Create('Invalid map operation');
  end;
  S := Src.First;
  D := Dst.First;
  while S <> nil do
  begin
    Move(S^, D^, Stride);
    S := Src.Next;
    D := Dst.Next;
  end;
end;

function TsdMapIterator.SafeAt(X, Y: integer): pbyte;
begin
  Result := nil;

  // Check dimensions
  if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then
    exit;

  // Dimensions ok, so call At
  Result := At(X, Y);
end;

end.
