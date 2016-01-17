unit bbSlice;

interface

uses
  bbUtils,
  HandleComponent,
  SysUtils, Windows, Classes, Graphics, Math;

type
  TbbSlice = class
  public
    function SetColorBitmap(ABitmap : TBitmap):Integer;
    function GetMonoBitmap(ABitmap : TBitmap):Integer;
    procedure Clear;
    
    constructor Create(EventHandleWindow : THandleComponent);
    destructor Destroy; override;
  protected
  private
    FBitmap : TBitmap;
    FRow, FCol : Integer;
    FSliceColorBuffer : Pointer;
    FSliceMonoBuffer: Pointer;
  public
    property ColorBuffer : Pointer read FSliceColorBuffer;
    property MonoBuffer : Pointer read FSliceMonoBuffer;
  end;

implementation

{ TbbSlice }

procedure TbbSlice.Clear;
begin
  FCol := 0;
  FRow := 0;

  FBitmap.FreeImage;

  if FSliceColorBuffer <> nil then FreeMem(FSliceColorBuffer);
end;

constructor TbbSlice.Create(EventHandleWindow : THandleComponent);
begin
  FBitmap := TBitmap.Create;
  FSliceColorBuffer := nil;
  FSliceMonoBuffer := nil;

  FCol := 0;
  FRow := 0;
end;

destructor TbbSlice.Destroy;
begin
  if FBitmap <> nil then FBitmap.Free;
  if FSliceColorBuffer <> nil then FreeMem(FSliceColorBuffer);
  if FSliceMonoBuffer <> nil then FreeMem(FSliceMonoBuffer);

  inherited;
end;

function TbbSlice.GetMonoBitmap(ABitmap: TBitmap): Integer;

var
  i, j, k : Integer;
  MonoBufferPointer : Pbb32bitBlock;
  SourcePointer : PbbARGB;
  DestPointer : PbbARGB;
begin
  ABitmap.PixelFormat := pf32bit;
  ABitmap.Width := FBitmap.Width;
  ABitmap.Height := FBitmap.Height;

  MonoBufferPointer := FSliceMonoBuffer;
  for I := 0 to FCol - 1 do begin //가로
    for J := 0 to FRow - 1 do begin //세로
      for k := 0 to bbBlockSize - 1 do begin
        SourcePointer := MonoBufferPointer.ScanLine(k);

        DestPointer := ABitmap.ScanLine[k + j * bbBlockSize];
        Inc(DestPointer, i * bbBlockSize);
        MoveMemory(DestPointer, SourcePointer, 4 * bbBlockSize);
      end;
      Inc(MonoBufferPointer);
    end;
  end;
  Result := FCol * FRow;
end;

function TbbSlice.SetColorBitmap(ABitmap : TBitmap):Integer;
{
  Return Value : BlockCount;
}
var
  i, j, k : Integer;
  ColorBufferPointer : Pbb32bitBlock;
  MonoBufferPointer : Pbb32bitBlock;
  SourcePointer : PbbARGB;
  BeforeBufferSize : Integer;
  DestPointer : PbbARGB;
begin
  FBitmap.Assign(ABitmap);
  FBitmap.PixelFormat := pf32bit;

  BeforeBufferSize := FCol * FRow;
  FCol := Floor(ABitmap.Width / bbBlockSize);
  FRow := Floor(ABitmap.Height / bbBlockSize);

  if (FCol * FRow) <> BeforeBufferSize then begin
    if FSliceColorBuffer <> nil then FreeMem(FSliceColorBuffer);
    FSliceColorBuffer := GetMemory(FCol * FRow * sizeof(Tbb32bitBlock));
    if FSliceMonoBuffer <> nil then FreeMem(FSliceMonoBuffer);
    FSliceMonoBuffer := GetMemory(FCol * FRow * sizeof(Tbb32bitBlock));
  end;

  ColorBufferPointer := FSliceColorBuffer;
  MonoBufferPointer := FSliceMonoBuffer;
  for I := 0 to FCol - 1 do begin //가로
    for J := 0 to FRow - 1 do begin //세로
      ColorBufferPointer.X := i;
      ColorBufferPointer.Y := j;
      MonoBufferPointer.X := i;
      MonoBufferPointer.Y := j;
      for k := 0 to bbBlockSize - 1 do begin
        SourcePointer := FBitmap.ScanLine[k + j * bbBlockSize];
        Inc(SourcePointer, + i * bbBlockSize);
        DestPointer := ColorBufferPointer.ScanLine(k);
        MoveMemory(DestPointer, SourcePointer, 4 * bbBlockSize);
      end;
      Inc(ColorBufferPointer);
      Inc(MonoBufferPointer);
    end;
  end;
  Result := FCol * FRow;
end;

end.
