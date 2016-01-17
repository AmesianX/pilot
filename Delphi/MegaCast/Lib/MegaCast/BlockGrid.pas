unit BlockGrid;

interface

uses
  MegaCastUtils, CompareBytes, 
  Windows, Classes, SysUtils, Graphics;

type
  TBlockGrid = class (TComponent)
  private
    FChanged : boolean;
    FCells : PDataArray;
    FWidth, FHeight : integer;
    FFrameSize : TFrameSize;
    procedure do_Clear;
    procedure do_Init;
  private
    FIsEmpty: boolean;
    FOnNewBlockUnit: TNewBlockUnitEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetLine(AX,AY,ALine:integer; AData:pointer);
    procedure CompareTo(ADest:TBlockGrid);

    function GetBitmap(ABitmap:TBitmap):boolean;

    procedure SetFrameSize(AFrameSize:TFrameSize);
  published
    property IsEmpty : boolean read FIsEmpty write FIsEmpty;
    property OnNewBlockUnit : TNewBlockUnitEvent read FOnNewBlockUnit write FOnNewBlockUnit;
  end;

implementation

{ TBlockGrid }

constructor TBlockGrid.Create(AOwner: TComponent);
var
  X, Y : integer;
begin
  inherited;

  FWidth  := 0;
  FHeight := 0;
  FIsEmpty := true;
  FChanged := false;

  New(FCells);
  for Y := 0 to FHeight-1 do
  for X := 0 to FWidth-1 do FCells^[X, Y] := nil;
end;

destructor TBlockGrid.Destroy;
begin
  do_Clear;

  Dispose(FCells);

  inherited;
end;

procedure TBlockGrid.do_Clear;
var
  X, Y : integer;
begin
  for Y := 0 to FHeight-1 do
  for X := 0 to FWidth-1 do
    if FCells^[X, Y] <> nil then begin
      FreeMem(FCells^[X, Y]);
      FCells^[X, Y] := nil;
    end;

  FIsEmpty := true;
end;

procedure TBlockGrid.CompareTo(ADest: TBlockGrid);
var
  pSrc, pDst : ^byte;
  X, Y : integer;
  pHeader : ^TMegaCastBlockUnitHeader;
begin
  if ADest.IsEmpty then begin
    for Y := 0 to FHeight-1 do
    for X := 0 to FWidth-1 do begin
      pHeader := FCells^[X, Y];
      if (pHeader^.X <> X) or (pHeader^.Y <> Y) then
        raise Exception.Create(ClassName+'.CompareTo');
      
      if Assigned(FOnNewBlockUnit) then FOnNewBlockUnit(Self, FCells^[X, Y], _BlockUnitSize);
    end;
  end else begin
    for Y := 0 to FHeight-1 do
    for X := 0 to FWidth-1 do begin
      pHeader := FCells^[X, Y];
      if (pHeader^.X <> X) or (pHeader^.Y <> Y) then
        raise Exception.Create(ClassName+'.CompareTo');

      pSrc := FCells^[X, Y];
      Inc(pSrc, SizeOf(TMegaCastBlockUnitHeader));

      pDst := ADest.FCells^[X, Y];
      Inc(pDst, SizeOf(TMegaCastBlockUnitHeader));

      if not CompareFastBytes(pSrc, pDst, _BlockSize*_BlockLineSize) then begin
        if Assigned(FOnNewBlockUnit) then FOnNewBlockUnit(Self, FCells^[X, Y], _BlockUnitSize);
      end;
    end;
  end;
end;

procedure TBlockGrid.do_Init;
var
  X, Y : integer;
  pHeader : ^TMegaCastBlockUnitHeader;
begin
  for Y := 0 to FHeight-1 do
  for X := 0 to FWidth-1 do begin
    GetMem(FCells^[X, Y], _BlockUnitSize);
    pHeader := FCells^[X, Y];
    pHeader^.X := X;
    pHeader^.Y := Y;
    pHeader^.BlockType := mcdtBitmap;
  end;
end;

function TBlockGrid.GetBitmap(ABitmap: TBitmap): boolean;
var
  X, Y, Line : integer;
  pBlock, pScanLine : ^byte;
begin
  Result := FChanged;
  if not Result then Exit;

  FChanged := false;
  
  pScanLine := ABitmap.ScanLine[ABitmap.Height-1];
  for Y := 0 to FHeight-1 do begin
    for Line := 0 to _BlockSize-1 do begin
      for X := 0 to FWidth-1 do begin
        pBlock := FCells^[X, Y];
        Inc(pBlock, SizeOf(TMegaCastBlockUnitHeader) + Line*_BlockLineSize);

        Move(pBlock^, pScanLine^, _BlockLineSize);
        Inc(pScanLine, _BlockLineSize);
      end;
    end;
  end;
end;

procedure TBlockGrid.SetFrameSize(AFrameSize: TFrameSize);
begin
  FFrameSize := AFrameSize;

  do_Clear;

  FWidth := FFrameSize.Width div _BlockSize;
  if (FFrameSize.Width mod _BlockSize) <> 0 then Inc(FWidth);

  FHeight := FFrameSize.Height div _BlockSize;
  if (FFrameSize.Height mod _BlockSize) <> 0 then Inc(FHeight);

  // _BlockSize에 나누어 떨어지도록 크기를 확장한다.
  FFrameSize := FrameSize(FWidth*_BlockSize, FHeight*_BlockSize);

  do_Init;
end;

procedure TBlockGrid.SetLine(AX, AY, ALine: integer; AData: pointer);
var
  pByte : ^byte;
begin
  FIsEmpty := false;
  FChanged := true;

  pByte := FCells^[AX, AY];
  Inc(pByte, ALine*_BlockLineSize + SizeOf(TMegaCastBlockUnitHeader));

  Move(AData^, pByte^, _BlockLineSize);
end;

end.
