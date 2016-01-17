unit MegaCastRecorder;

interface

uses
  MegaCastUtils, BlockBuffer, DeskTopCapture, FrameSlice, MegaCastEncoder,
  Windows, Classes, SysUtils, Forms;

type
  TMegaCastRecorder = class(TComponent)
  private
    FBlockBuffer : TBlockBuffer;
    FCapture : TDeskTopCapture;
    FFrameSlice : TFrameSlice;
    FEncoder : TMegaCastEncoder;
    function on_NeedFrame(Sender:TObject; var AData:pointer; var ASize:integer; var AFrameSize:TFrameSize):boolean;
    procedure on_NewBlockUnit(Sender:TObject; ABlockUnit:pointer; AUnitSize:integer);
  private
    FFrameSize: TFrameSize;
    FCaptureRect : TRect;
    FOnFrameSizeChanged: TFrameSizeChangedEvent;
    FMonitorNo: integer;
    procedure SetMonitorNo(const Value: integer);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    function GetBlockUnit(var ABlockUnit:pointer; var AUnitSize:integer):boolean;

    procedure SetCaptureRect(const ARect:TRect);
  published
    property FrameSize : TFrameSize read FFrameSize;
    property MonitorNo : integer read FMonitorNo write SetMonitorNo;
    property OnFrameSizeChanged : TFrameSizeChangedEvent read FOnFrameSizeChanged write FOnFrameSizeChanged;
  end;

implementation

{ TMegaCastRecorder }

constructor TMegaCastRecorder.Create(AOwner: TComponent);
begin
  inherited;

  FBlockBuffer := TBlockBuffer.Create;
  FCapture := TDeskTopCapture.Create;

  FFrameSlice := TFrameSlice.Create(Self);
  FFrameSlice.OnNeedFrame := on_NeedFrame;
  FFrameSlice.OnNewBlockUnit := on_NewBlockUnit;

  FEncoder := TMegaCastEncoder.Create(Self);

  SetCaptureRect(Rect(0, 0, 1024, 768));
end;

destructor TMegaCastRecorder.Destroy;
begin
  Stop;

  FreeAndNil(FBlockBuffer);
  FreeAndNil(FCapture);
  FreeAndNil(FFrameSlice);
  FreeAndNil(FEncoder);

  inherited;
end;

function TMegaCastRecorder.GetBlockUnit(var ABlockUnit: pointer; var AUnitSize: integer): boolean;
var
  Data : pointer;
  Size : integer;
begin
  ABlockUnit := nil;
  AUnitSize  := 0;

  Result := FBlockBuffer.GetBlockUnit(Data, Size);
  if not Result then Exit;

  try
    FEncoder.Execute(Data, Size);

    AUnitSize := FEncoder.DataOut.Size;
    GetMem(ABlockUnit, AUnitSize);
    Move(FEncoder.DataOut.Memory^, ABlockUnit^, AUnitSize);
  finally
    FreeMem(Data);
  end;
end;

function TMegaCastRecorder.on_NeedFrame(Sender: TObject; var AData: pointer;
  var ASize: integer; var AFrameSize: TFrameSize): boolean;
begin
  Result := FCapture.GetFrame(AData, ASize, AFrameSize);
end;

procedure TMegaCastRecorder.on_NewBlockUnit(Sender: TObject;
  ABlockUnit: pointer; AUnitSize: integer);
begin
  FBlockBuffer.AddBlockUnit(ABlockUnit, AUnitSize);
end;

procedure TMegaCastRecorder.SetCaptureRect(const ARect: TRect);
begin
  FCaptureRect := ARect;
  FFrameSize.Width  := Abs(ARect.Right  - ARect.Left);
  FFrameSize.Height := Abs(ARect.Bottom - ARect.Top);

  FCapture.SetCaptureRect(FCaptureRect);
  FBlockBuffer.SetFrameSize(FFrameSize);
  FFrameSlice.SetFrameSize(FFrameSize);

  if Assigned(FOnFrameSizeChanged) then FOnFrameSizeChanged(Self, FFrameSize);
end;

procedure TMegaCastRecorder.SetMonitorNo(const Value: integer);
begin
  FMonitorNo := Value;

  SetCaptureRect(Rect(
    Screen.Monitors[FMonitorNo].Left,
    Screen.Monitors[FMonitorNo].Top,
    Screen.Monitors[FMonitorNo].Left + Screen.Monitors[FMonitorNo].Width,
    Screen.Monitors[FMonitorNo].Top + Screen.Monitors[FMonitorNo].Height
  ));
end;

procedure TMegaCastRecorder.Start;
begin
  FBlockBuffer.Clear;
  FCapture.Start;
  FFrameSlice.Start;
end;

procedure TMegaCastRecorder.Stop;
begin
  FCapture.Stop;
  FFrameSlice.Stop;
  FBlockBuffer.Clear;
end;

end.
