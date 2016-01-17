unit BlackBoard;

interface

uses
  bbSnapShot, BBSlice, bbThreadPool, bbUtils,
  HandleComponent,
  Windows, Graphics, Messages;

type
  TBlackBoard = class(THandleComponent)
  private
    FSlice : TbbSlice;
    FSnapShot : TbbSnapShot;
    FThreadPool : TbbThreadPool;
    FBitmap : TBitmap;
    FOnMonoImage: TImageEvent;

    procedure OnCaptureComplete(var Msg : TMessage); Message WM_CAPTURECOMPLETE;
    procedure OnAllFinish(var Msg : TMessage); Message WM_ALLFINISH;
    procedure SetOnMonoImage(const Value: TImageEvent);
    function GetCamaraNo: Integer;
    procedure SetCameraNo(const Value: Integer);
    function GetResolution: Integer;
    procedure SetResolution(const Value: Integer);
    function GetBrightnessThreshold: double;
    procedure SetBrightnessThreshold(const Value: double);
  protected
  public
    function GetBitmap:Boolean;

    constructor Create; reintroduce;
    destructor Destroy; override;

    property CameraNo : Integer read GetCamaraNo write SetCameraNo;
    property Resolution : Integer read GetResolution write SetResolution;
    property BrightnessThreshold : double read GetBrightnessThreshold write SetBrightnessThreshold;

    property OnMonoImage : TImageEvent read FOnMonoImage write SetOnMonoImage;
  end;

implementation

uses
  Unit1;
{ TBlackBoard }

constructor TBlackBoard.Create;
begin
  inherited Create(Nil);

  FSnapShot := TbbSnapShot.Create(Self);
  FSlice := TbbSlice.Create(Self);

  //Todo : Thread 는 임시로 1개
  FThreadPool := TbbThreadPool.Create(4, Self);
  FThreadPool.EventHandleWindow := Self;

  FBitmap := TBitmap.Create;
end;

destructor TBlackBoard.Destroy;
begin
  FThreadPool.Free;
  FSlice.Free;
  FSnapShot.Free;
  FBitmap.Free;

  inherited;
end;

function TBlackBoard.GetBitmap: Boolean;
begin
  Result := false;
  if FThreadPool.isWorking = true then exit;

  FSnapShot.Capture(FBitmap);
  Result := true;

  //ASync --> OnCaptureComplete
end;

function TBlackBoard.GetBrightnessThreshold: double;
begin
  Result := FThreadPool.BrightnessThresHold;
end;

function TBlackBoard.GetCamaraNo: Integer;
begin
  Result := FSnapShot.CameraNo;
end;

function TBlackBoard.GetResolution: Integer;
begin
  Result := FSnapShot.Resolution;
end;

procedure TBlackBoard.OnAllFinish(var Msg: TMessage);
var
  B : TBitmap;
begin
  if Assigned(FOnMonoImage) then begin
    B := TBitmap.Create;
    try
      FSlice.GetMonoBitmap(B);
      OnMonoImage(Self, B);
    finally
      B.Free;
    end;
  end;
end;

procedure TBlackBoard.OnCaptureComplete(var Msg: TMessage);
var
  BlockCount : Integer;
begin
  BlockCount := FSlice.SetColorBitmap(FBitmap);
  //BlockCount := FSlice.SetColorBitmap(Form1.Image1.Picture.Bitmap);

  FThreadPool.ColorBuffer := FSlice.ColorBuffer;
  FThreadPool.MonoBuffer := FSlice.MonoBuffer;

  FThreadPool.BlockCount := BlockCount;

  FThreadPool.Start;
end;

procedure TBlackBoard.SetBrightnessThreshold(const Value: double);
begin
  FThreadPool.BrightnessThresHold := Value;
end;

procedure TBlackBoard.SetCameraNo(const Value: Integer);
begin
  FSnapShot.CameraNo := Value;
end;

procedure TBlackBoard.SetOnMonoImage(const Value: TImageEvent);
begin
  FOnMonoImage := Value;
end;

procedure TBlackBoard.SetResolution(const Value: Integer);
begin
  FSnapShot.Resolution := Value;
end;

end.
