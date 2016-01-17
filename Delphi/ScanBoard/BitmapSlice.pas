unit BitmapSlice;

interface

uses
  Windows, Classes, SysUtils, Graphics;

type
  TBitmapList = class (TPersistent)
  private
  private
    FSize: integer;
    FWidth: integer;
    FHeight: integer;
    FBlocks : array of TBitmap;
    FBlockSize: integer;
    FPixelFormat: TPixelFormat;
    function GetBlocks(X, Y: integer): TBitmap;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(APixelFormat:TPixelFormat; ABlockSize:integer); reintroduce;
    destructor Destroy; override;

    procedure Clear;
    procedure SetSize(AWidth,AHeight:integer);

    property Size : integer read FSize;
    property Width : integer read FWidth;
    property Height : integer read FHeight;
    property PixelFormat : TPixelFormat read FPixelFormat;
    property BlockSize : integer read FBlockSize;
    property Blocks[X,Y:integer] : TBitmap read GetBlocks;
  end;

  TBitmapSlice = class
  private
    FPixelSize: integer;
  private
    FBlockSize : integer;
    FPixelFormat: TPixelFormat;
    FBitmapList : TBitmapList;
  public
    constructor Create(APixelFormat:TPixelFormat; ABlockSize:integer); reintroduce;
    destructor Destroy; override;

    procedure Clear;
    procedure Slice(ABitmap:TBitmap);

    property PixelFormat : TPixelFormat read FPixelFormat;
    property BlockSize : integer read FBlockSize;
    property BitmapList : TBitmapList read FBitmapList write FBitmapList;
  end;

implementation

{ TBitmapList }

procedure TBitmapList.AssignTo(Dest: TPersistent);
var
  Dst : TBitmapList;
  LoopX, LoopY : integer;
begin
  Dst := Pointer(Dest);

  Dst.Clear;
  Dst.FPixelFormat := PixelFormat;
  Dst.FBlockSize := BlockSize;
  Dst.SetSize(Width, Height);

  for LoopY := 0 to Height - 1 do
  for LoopX := 0 to Width - 1 do
    Dst.Blocks[LoopX, LoopY].Assign(Blocks[LoopX, LoopY]);
end;

procedure TBitmapList.Clear;
var
  Loop : integer;
begin
  for Loop := Low(FBlocks) to High(FBlocks) do FBlocks[Loop].Free;

  SetLength(FBlocks, 0);

  FSize := 0;
  FWidth := 0;
  FHeight := 0;
end;

constructor TBitmapList.Create(APixelFormat:TPixelFormat; ABlockSize:integer);
begin
  inherited Create;

  FPixelFormat := APixelFormat;
  FBlockSize := ABlockSize;

  Clear;
end;

destructor TBitmapList.Destroy;
begin
  Clear;

  inherited;
end;

function TBitmapList.GetBlocks(X, Y: integer): TBitmap;
begin
  Result := FBlocks[Y*Width + X];
end;

procedure TBitmapList.SetSize(AWidth, AHeight: integer);
var
  Loop : integer;
begin
  if (AWidth = Width) and (AHeight = Height) then Exit;

  Clear;

  FWidth := AWidth;
  FHeight := AHeight;
  FSize := (AWidth shl 8) or AHeight;

  SetLength(FBlocks, Width*Height);
  for Loop := Low(FBlocks) to High(FBlocks) do begin
    FBlocks[Loop] := TBitmap.Create;
    FBlocks[Loop].PixelFormat := PixelFormat;
    FBlocks[Loop].Width := BlockSize;
    FBlocks[Loop].Height := BlockSize;
    FBlocks[Loop].Canvas.TextOut(10, 10, IntToStr(Loop));
  end;
end;

{ TBitmapSlice }

procedure TBitmapSlice.Clear;
begin
  FBitmapList.Clear;
end;

constructor TBitmapSlice.Create(APixelFormat:TPixelFormat; ABlockSize:integer);
begin
  inherited Create;

  FPixelFormat := APixelFormat;
  FBlockSize := ABlockSize;

  case FPixelFormat of
    pf16bit: FPixelSize := 2;
    pf32bit: FPixelSize := 4;
    else
      raise Exception.Create('TBitmapSlice.Create: 지원되지 않는 PixelFormat 입니다.');
  end;
end;

destructor TBitmapSlice.Destroy;
begin

  inherited;
end;

procedure TBitmapSlice.Slice(ABitmap: TBitmap);
var
  pSrc : ^byte;
  Bitmap : TBitmap;
  BlockX, BlockY, BlockRow, iWidth, iHeight : integer;
begin
  iWidth := ABitmap.Width div BlockSize;
  if (ABitmap.Width mod BlockSize) <> 0 then iWidth := iWidth + 1;

  iHeight := ABitmap.Height div BlockSize;
  if (ABitmap.Height mod BlockSize) <> 0 then iHeight := iHeight + 1;

  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := FPixelFormat;

    Bitmap.Canvas.Brush.Color := clBlack;
    Bitmap.Width :=  iWidth * BlockSize;
    Bitmap.Height := iHeight * BlockSize;
    Bitmap.Canvas.Draw(0, 0, ABitmap);

    FBitmapList.SetSize(iWidth, iHeight);

    // Block으로 조각 내기
    pSrc := Bitmap.ScanLine[Bitmap.Height-1];
    for BlockY := iHeight-1 downto 0 do begin
      for BlockRow := BlockSize-1 downto 0 do begin
        for BlockX := 0 to iWidth-1 do begin
          Move(pSrc^, FBitmapList.Blocks[BlockX, BlockY].ScanLine[BlockRow]^, BlockSize*FPixelSize);
          Inc(pSrc, BlockSize*FPixelSize);
        end;
      end;
    end;
  finally
    Bitmap.Free;
  end;
end;

end.
