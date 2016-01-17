unit BlockSync;

interface

uses
  MegaCastUtils, TickCount, BlockBuffer,
  Windows, Classes, SysUtils, SyncObjs;

type
  TBlockSync = class
  private
    FTick : int64;
    FList : TList;
    FBlockBuffer : TBlockBuffer;
    FCS : TCriticalSection;
    function get_BlockUnit(var ABlockUnit:pointer; var AUnitSize:integer):boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AudioSync(ATick:int64);

    procedure Clear;
    procedure AddBlockUnit(ABlockUnit:pointer; AUnitSize:integer);
    function GetBlockUnit(var ABlockUnit:pointer; var AUnitSize:integer):boolean;

    procedure SetFrameSize(AFrameSize:TFrameSize);
  end;

implementation

type
  TBlock = class
  private
    FTick  : int64;
    FBlockUnit : pointer;
    FUnitSize : integer;
  public
    constructor Create(ATick:int64; ABlockUnit:pointer; AUnitSize:integer); reintroduce;
    procedure MakeBlockUnitEmpty;
  end;

{ TBlock }

constructor TBlock.Create(ATick: int64; ABlockUnit: pointer;
  AUnitSize: integer);
begin
  inherited Create;

  FTick := ATick;
  FUnitSize := AUnitSize;

  if AUnitSize > 0 then begin
    GetMem(FBlockUnit, AUnitSize);
    Move(ABlockUnit^, FBlockUnit^, AUnitSize);
  end else begin
    FBlockUnit := nil;
  end;
end;

procedure TBlock.MakeBlockUnitEmpty;
begin
  if FBlockUnit <> nil then FreeMem(FBlockUnit);
end;

{ TBlockSync }

procedure TBlockSync.AddBlockUnit(ABlockUnit: pointer; AUnitSize: integer);
begin
  FCS.Enter;
  try
    FList.Add(TBlock.Create(TTickCount.Obj.Get, ABlockUnit, AUnitSize));
  finally
    FCS.Leave;
  end;
end;

procedure TBlockSync.AudioSync(ATick: int64);
var
  BlockUnit : pointer;
  UnitSize : integer;
begin
  FCS.Enter;
  try
    FTick := ATick;

    while get_BlockUnit(BlockUnit, UnitSize) do begin
      try
        FBlockBuffer.AddBlockUnit(BlockUnit, UnitSize);
      finally
        if BlockUnit <> nil then FreeMem(BlockUnit);
      end;
    end;    
  finally
    FCS.Leave;
  end;
end;

constructor TBlockSync.Create;
begin
  inherited;

  FList := TList.Create;
  FBlockBuffer := TBlockBuffer.Create;
  FCS := TCriticalSection.Create;
end;

destructor TBlockSync.Destroy;
begin
  Clear;

  FreeAndNil(FCS);
  FreeAndNil(FBlockBuffer);
  FreeAndNil(FList);

  inherited;
end;

function TBlockSync.GetBlockUnit(var ABlockUnit: pointer;
  var AUnitSize: integer): boolean;
begin
  Result := FBlockBuffer.GetBlockUnit(ABlockUnit, AUnitSize);
end;

procedure TBlockSync.Clear;
var
  Loop : integer;
  Block : TBlock;
begin
  FCS.Enter;
  try
    for Loop := FList.Count-1 downto 0 do begin
      Block := Pointer(FList[Loop]);
      Block.MakeBlockUnitEmpty;
      Block.Free;
    end;
    
    FList.Clear;
  finally
    FCS.Leave;
  end;
end;

function TBlockSync.get_BlockUnit(var ABlockUnit: pointer;
  var AUnitSize: integer): boolean;
var
  Block : TBlock;
begin
  ABlockUnit := nil;
  AUnitSize  := 0;
  Result := false;
  if FList.Count = 0 then Exit;

  Block := Pointer(FList[0]);
  if Block.FTick > FTick then Exit;

  ABlockUnit := Block.FBlockUnit;
  AUnitSize  := Block.FUnitSize;

  Block.Free;
  FList.Delete(0);

  Result := true;
end;

procedure TBlockSync.SetFrameSize(AFrameSize: TFrameSize);
begin
  FBlockBuffer.SetFrameSize(AFrameSize);
end;

end.
