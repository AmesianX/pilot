unit FrameSlice;

interface

uses
  MegaCastUtils, ThreadRepeater, CompareBytes, BlockGrid,
  Windows, Classes, SysUtils, SyncObjs;

type
  TNeedFrameEvent = function (Sender:TObject; var AData:pointer; var ASize:integer; var AFrameSize:TFrameSize):boolean of object;

  TFrameSlice = class (TComponent)
  private
    FOldGrid, FNewGrid : TBlockGrid;
    FFrameSize : TFrameSize;
    FRepeater : TThreadRepeater;
    FCS : TCriticalSection;
    procedure on_Execute(Sender:TObject);
    procedure do_Slice(AData:Pointer; ASize:integer; AFrameSize:TFrameSize);
    procedure do_CompareNewAndOldGrid;
    procedure do_SwapGrid;
  private
    FOnNeedFrame: TNeedFrameEvent;
    function GetOnNewBlockUnit: TNewBlockUnitEvent;
    procedure SetOnNewBlockUnit(const Value: TNewBlockUnitEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure SetFrameSize(AFrameSize:TFrameSize);
    procedure GetFrameSize(var AFrameSize:TFrameSize);
  published
    property OnNeedFrame : TNeedFrameEvent read FOnNeedFrame write FOnNeedFrame;
    property OnNewBlockUnit : TNewBlockUnitEvent read GetOnNewBlockUnit write SetOnNewBlockUnit;
  end;

implementation

{ TFrameSlice }

constructor TFrameSlice.Create(AOwner: TComponent);
begin
  inherited;

  FOldGrid := TBlockGrid.Create(Self);
  FNewGrid := TBlockGrid.Create(Self);
  FRepeater := TThreadRepeater.Create(Self);
  FCS := TCriticalSection.Create;
end;

destructor TFrameSlice.Destroy;
begin
  Stop;

  FreeAndNil(FOldGrid);
  FreeAndNil(FNewGrid);
  FreeAndNil(FRepeater);
  FreeAndNil(FCS);

  inherited;
end;

procedure TFrameSlice.do_CompareNewAndOldGrid;
begin
  FNewGrid.CompareTo(FOldGrid);
end;

procedure TFrameSlice.do_Slice(AData: Pointer; ASize: integer; AFrameSize: TFrameSize);
var
  pByte : ^byte;
  X, Y : integer;
  Loop, LineCount, BlockWidth, Line : integer;
begin
  pByte := AData;

  // 이미지 전체를 Line으로 나눴을 때의 라인 수
  LineCount := ASize div _BlockLineSize;
  Assert((ASize mod _BlockLineSize) = 0, 'TFrameSlice.do_Slice: 이미지가 블록단위로 나눌 수 없는 크기입니다.');

  // 이미지를 블록으로 나웠을 때, 해당 셀의 넓이
  BlockWidth := AFrameSize.Width div _BlockSize;
  Assert((AFrameSize.Width mod _BlockSize) = 0, 'TFrameSlice.do_Slice: 이미지가 블록단위로 나눌 수 없는 크기입니다.');

  Line := 0;
  X := 0;
  Y := 0;
  for Loop := 1 to LineCount do begin
    FNewGrid.SetLine(X, Y, Line, pByte);

    Inc(X);
    if X >= BlockWidth then begin
      X := 0;

      // 이미지를 블록으로 나눴기 때문에, 이미지가 _BlockSize 단위로 라인을 읽을 때마다 아래 블록으로 내려간다.
      Inc(Line);
      if Line >= _BlockSize then begin
        Line := 0;
        Inc(Y);
      end;
    end;

    Inc(pByte, _BlockLineSize);
  end;
end;

procedure TFrameSlice.do_SwapGrid;
var
  Temp : TBlockGrid;
begin
  Temp := FOldGrid;
  FOldGrid := FNewGrid;
  FNewGrid := Temp;
end;

procedure TFrameSlice.GetFrameSize(var AFrameSize: TFrameSize);
begin
  FCS.Enter;
  try
    AFrameSize := FFrameSize;
  finally
    FCS.Leave;
  end;
end;

function TFrameSlice.GetOnNewBlockUnit: TNewBlockUnitEvent;
begin
  Result := FOldGrid.OnNewBlockUnit;
end;

procedure TFrameSlice.on_Execute(Sender: TObject);
var
  Data : pointer;
  Size : integer;
  FrameSize : TFrameSize;
begin
  if Assigned(FOnNeedFrame) then begin
    if not FOnNeedFrame(Self, Data, Size, FrameSize) then begin
      Sleep(1);
      Exit;
    end;

    try
      FCS.Enter;
      try
        do_Slice(Data, Size, FrameSize);
        do_CompareNewAndOldGrid;
        do_SwapGrid;
      finally
        FCS.Leave;
      end;
    finally
      FreeMem(Data);
    end;
  end;
end;

procedure TFrameSlice.SetFrameSize(AFrameSize: TFrameSize);
begin
  FCS.Enter;
  try
    FFrameSize := AFrameSize;

    FOldGrid.SetFrameSize(AFrameSize);
    FNewGrid.SetFrameSize(AFrameSize);
  finally
    FCS.Leave;
  end;
end;

procedure TFrameSlice.SetOnNewBlockUnit(const Value: TNewBlockUnitEvent);
begin
  FOldGrid.OnNewBlockUnit := Value;
  FNewGrid.OnNewBlockUnit := Value;
end;

procedure TFrameSlice.Start;
begin
  FOldGrid.IsEmpty := true;
  FNewGrid.IsEmpty := true;

  FRepeater.Execute(on_Execute);
end;

procedure TFrameSlice.Stop;
begin
  FRepeater.Stop;
end;

end.
