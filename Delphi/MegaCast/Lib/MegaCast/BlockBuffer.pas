unit BlockBuffer;

interface

uses
  MegaCastUtils,
  Windows, Classes, SysUtils, SyncObjs;

type
  TBlockBufferBase = class
  private
    FDatas : PDataArray;
    FSizes : PSizeArray;
    FFrameSize : TFrameSize;
    FCellSize : TFrameSize;
    FIndex : integer;
    FDataCount : cardinal;
    FCS : TCriticalSection;
    procedure do_Clear;
    procedure dec_Index;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddBlockUnit(ABlockUnit:pointer; AUnitSize:integer);

    procedure LoadFromStream(AStream:TStream);

    procedure SetFrameSize(AFrameSize:TFrameSize);
    procedure GetFrameSize(var AFrameSize:TFrameSize);
  end;

  TBlockBuffer = class (TBlockBufferBase)
  private
  public
    function GetBlockUnit(var ABlockUnit:pointer; var AUnitSize:integer):boolean;
    function SaveToStream(AStream:TStream; ASizeLimit:integer):boolean;
  end;

  TScreenShot = class (TBlockBufferBase)
  private
  public
    function SaveToStream(AStream:TStream):boolean;
  end;

implementation

{ TBlockBufferBase }

procedure TBlockBufferBase.AddBlockUnit(ABlockUnit: pointer;
  AUnitSize: integer);
var
  X, Y : word;
  pHeader : ^TMegaCastBlockUnitHeader;
begin
  Assert(AUnitSize > 0, ClassName+'.AddBlockUnit: 데이터 크기에 이상이 있습니다.');

  pHeader := ABlockUnit;
  X := pHeader^.X;
  Y := pHeader^.Y;

  FCS.Enter;
  try
    if FSizes^[X, Y] > 0 then begin
      FreeMem(FDatas^[X, Y]);
    end else begin
      Inc(FDataCount);
      {$IFDEF DEBUG}
//        OutputDebugString(PChar(Format(ClassName+'.AddBlockUnit: FDataCount=%d', [FDataCount])));
      {$ENDIF}
    end;

    FSizes^[X, Y] := AUnitSize;
    GetMem(FDatas^[X, Y], AUnitSize);
    Move(ABlockUnit^, FDatas^[X, Y]^, AUnitSize);
  finally
    FCS.Leave;
  end;
end;

procedure TBlockBufferBase.Clear;
begin
  FCS.Enter;
  try
    do_Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TBlockBufferBase.Create;
begin
  inherited;

  FIndex := 0;
  FDataCount := 0;

  FFrameSize.Width  := _CellSize * _BlockSize;
  FFrameSize.Height := _CellSize * _BlockSize;

  FCellSize.Width  := _CellSize;
  FCellSize.Height := _CellSize;

  New(FDatas);

  New(FSizes);
  FillChar(FSizes^, SizeOf(TSizeArray), 0);

  FCS := TCriticalSection.Create;
end;

destructor TBlockBufferBase.Destroy;
begin
  Clear;
  
  Dispose(FDatas);
  Dispose(FSizes);
  FreeAndNil(FCS);

  inherited;
end;

procedure TBlockBufferBase.do_Clear;
var
  LoopX, LoopY : word;
begin
  FIndex := 0;
  FDataCount := 0;

  for LoopY := 0 to _CellSize-1 do
  for LoopX := 0 to _CellSize-1 do
    if FSizes^[LoopX, LoopY] > 0 then begin
      FreeMem(FDatas^[LoopX, LoopY]);
    end;

  FillChar(FSizes^, SizeOf(TSizeArray), 0);
end;

procedure TBlockBufferBase.GetFrameSize(var AFrameSize: TFrameSize);
begin
  FCS.Enter;
  try
    AFrameSize := FFrameSize;
  finally
    FCS.Leave;
  end;
end;

procedure TBlockBufferBase.dec_Index;
begin
  Dec(FIndex);
  if FIndex < 0 then FIndex := FCellSize.Width*FCellSize.Height;
end;

procedure TBlockBufferBase.LoadFromStream(AStream: TStream);
var
  X, Y : integer;
  BlockUnit : pointer;
  UnitSize : integer;
  pHeader : ^TMegaCastBlockUnitHeader;
begin
  FCS.Enter;
  try
    AStream.Position := 0;
    while AStream.Position < AStream.Size do begin
      AStream.Read(UnitSize, SizeOf(UnitSize));
      Assert(UnitSize > 0, ClassName+'.LoadFromStream: UnitSize가 0 또는 그 이하일 수 없다.');

      GetMem(BlockUnit, UnitSize);
      AStream.Read(BlockUnit^, UnitSize);

      pHeader := BlockUnit;
      X := pHeader^.X;
      Y := pHeader^.Y;

      if FSizes^[X, Y] > 0 then begin
        FreeMem(FDatas^[X, Y]);
      end else begin
        Inc(FDataCount);
      end;

      FSizes^[X, Y] := UnitSize;
      FDatas^[X, Y] := BlockUnit;
    end;
  finally
    FCS.Leave;
  end;        

  {$IFDEF DEBUG}
    OutputDebugString(PChar(Format(ClassName+'.LoadFromStream: FDataCount=%d', [FDataCount])));
  {$ENDIF}
end;

procedure TBlockBufferBase.SetFrameSize(AFrameSize: TFrameSize);
begin
  FCS.Enter;
  try
    FFrameSize := AFrameSize;

    FCellSize.Width  := AFrameSize.Width  div _BlockSize;
    if (AFrameSize.Width mod _BlockSize) <> 0 then Inc(FCellSize.Width);
    FCellSize.Height := AFrameSize.Height div _BlockSize;
    if (AFrameSize.Height mod _BlockSize) <> 0 then Inc(FCellSize.Height);
  finally
    FCS.Leave;
  end;
end;

{ TBlockBuffer }

function TBlockBuffer.GetBlockUnit(var ABlockUnit: pointer;
  var AUnitSize: integer): boolean;
var
  Loop, X, Y : integer;
begin
  ABlockUnit := nil;
  AUnitSize  := 0;
  Result := false;

  if FDataCount = 0 then Exit;

  FCS.Enter;
  try
    for Loop := 1 to FCellSize.Width * FCellSize.Height do begin
      X := FIndex mod FCellSize.Width;
      Y := FIndex div FCellSize.Width;

      if FSizes^[X, Y] > 0 then begin
        Result := true;

        AUnitSize := FSizes^[X, Y];
        GetMem(ABlockUnit, AUnitSize);
        Move(FDatas^[X, Y]^, ABlockUnit^, AUnitSize);

        FreeMem(FDatas^[X, Y]);
        FSizes^[X, Y] := 0;

        Dec(FDataCount);

        dec_Index;

        Break;
      end;

      dec_Index;
    end;
  finally
    FCS.Leave;
  end;

  {$IFDEF DEBUG}
//    OutputDebugString(PChar(Format(ClassName+'.GetBlockUnit: FDataCount=%d', [FDataCount])));
  {$ENDIF}
end;

function TBlockBuffer.SaveToStream(AStream: TStream;
  ASizeLimit: integer): boolean;
var
  Data : pointer;
  Loop, X, Y : integer;
begin
  Result := false;
  if ASizeLimit = 0 then Exit;  
  if FDataCount = 0 then Exit;  

  FCS.Enter;
  try
    for Loop := 1 to FCellSize.Width * FCellSize.Height do begin
      X := FIndex mod FCellSize.Width;
      Y := FIndex div FCellSize.Width;
      
      if FSizes^[X, Y] > 0 then begin
        Result := true;

        Data := FDatas^[X, Y];

        AStream.Write(FSizes^[X, Y], SizeOf(Integer));
        AStream.Write(Data^, FSizes^[X, Y]);

        FreeMem(Data);
        FSizes^[X, Y] := 0;

        Dec(FDataCount);
        if FDataCount = 0 then Break;

        if AStream.Size >= ASizeLimit then begin
          dec_Index;
          Break;
        end;
      end;

      dec_Index;
    end;
  finally
    FCS.Leave;
  end;
end;

{ TScreenShot }

function TScreenShot.SaveToStream(AStream: TStream): boolean;
var
  X, Y, Count : integer;
begin
  Result := false;

  FCS.Enter;
  try
    Count := 0;
    for Y := 0 to FCellSize.Height-1 do
    for X := 0 to FCellSize.Width-1 do begin
      if FSizes^[X, Y] > 0 then begin
        Result := true;
        Inc(Count);

        AStream.Write(FSizes^[X, Y], SizeOf(Integer));
        AStream.Write(FDatas^[X, Y]^, FSizes^[X, Y]);
      end;
    end;

    Assert(FDataCount = Count, ClassName+'.SaveToStream: 저장된 블록의 숫자가 잘못되었습니다.');
  finally
    FCS.Leave;
  end;
end;

end.
