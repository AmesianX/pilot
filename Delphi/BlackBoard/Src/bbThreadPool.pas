unit bbThreadPool;

interface

uses
  HandleComponent, SpinLock, bbUtils,
  Windows, Classes, Graphics, Messages, Controls;

type
  Tbb32bitToMonoThread = class(TThread)
  public
    constructor Create(ThreadNumber, MaxThread : Integer);
    destructor Destroy; override;
  protected
    procedure Execute; override;
    procedure Process(AColorBlock, AMonoBlock : Pbb32bitBlock); virtual;
  private
    FThreadNumber : Integer; //0 ~ N
    FMaxThread : Integer;
    FColorBuffer : Pbb32bitBlock;
    FBlockCount: Integer;  //총 처리해야할 블럭의 개수
    FisWorking : Boolean;
    FMonoBuffer: Pbb32bitBlock;
    FEventOwner : THandleComponent;
    FBrightnessThreshold: double;
  public
    property ColorBuffer : Pbb32bitBlock read FColorBuffer write FColorBuffer;
    property MonoBuffer : Pbb32bitBlock read FMonoBuffer write FMonoBuffer;
    property BlockCount : Integer read FBlockCount write FBlockCount;
    property BrightnessThreshold : double read FBrightnessThreshold write FBrightnessThreshold;
    property isWorking : Boolean read FisWorking write FisWorking;
    property EventOwner : THandleComponent read FEventOwner write FEventOwner;
  end;

  TbbThreadPool = Class(THandleComponent)
  public
    function Start:Boolean;

    constructor Create(ThreadCount : Integer;EventHandleWindow : THandleComponent); reintroduce;
    destructor Destroy; override;
  protected
  private
    FThreadList : TList;
    FCS : TSpinLock; //상태변수 공유용
    FisWorking : Boolean;
    FColorBuffer : Pbb32bitBlock; // Block 들
    FMonoBuffer: Pbb32bitBlock;

    FFinishedThread : Integer; //일이 끝난 Thread 수
    FBlockCount : Integer; //Block 갯수
    FEventHandleWindow : THandleComponent;
    FBrightnessThreshold: double;
    
    procedure OnWMFinishWork(var Msg : TMessage); Message WM_FINISHWORK;
    function GetThreadCount: Integer;
    procedure SetBlockCount(const Value: Integer);
    procedure SetEventHandleWindow(const Value: THandleComponent);
    procedure SetBrightnessThreshold(const Value: double);
  public
    property BrightnessThreshold : double read FBrightnessThreshold write SetBrightnessThreshold;
    property ThreadCount : Integer read GetThreadCount;
    property ColorBuffer : Pbb32bitBlock read FColorBuffer write FColorBuffer;
    property MonoBuffer : Pbb32bitBlock read FMonoBuffer write FMonoBuffer;
    property BlockCount : Integer read FBlockCount write SetBlockCount;
    property isWorking : boolean read FisWorking;
    property EventHandleWindow : THandleComponent read FEventHandleWindow write SetEventHandleWindow;
  End;

implementation

{ TbbThreadPool }

constructor TbbThreadPool.Create(ThreadCount : Integer; EventHandleWindow : THandleComponent);
var
  I: Integer;
  TempThread : Tbb32bitToMonoThread;
begin
  inherited Create(Nil);

  FCS := TSpinLock.Create;
  FisWorking := false;
  FThreadList := TList.Create;
  FEventHandleWindow := EventHandleWindow;
  FBrightnessThreshold := 0.85; 

  for I := 0 to ThreadCount - 1 do begin
    TempThread := Tbb32bitToMonoThread.Create(I, ThreadCount);
    FThreadList.Add(TempThread);
    TempThread.EventOwner := Self;
  end;
end;

destructor TbbThreadPool.Destroy;
var
  I: Integer;
begin
  for I := 0 to ThreadCount - 1 do begin
    Tbb32bitToMonoThread(FThreadList.Items[i]).Terminate;
  end;

  for I := 0 to ThreadCount - 1 do begin
    Tbb32bitToMonoThread(FThreadList.Items[i]).WaitFor;
  end;

  FThreadList.Free;

  FCS.Free;
  inherited;
end;

function TbbThreadPool.GetThreadCount: Integer;
begin
  FCS.Enter;
  try
    Result := FThreadList.Count;
  finally
    FCS.Leave;
  end;
end;

procedure TbbThreadPool.OnWMFinishWork(var Msg: TMessage);
begin
  FCS.Enter;
  try
    Inc(FFinishedThread);
    if FFinishedThread = ThreadCount then begin
      FFinishedThread := 0;
      PostMessage(FEventHandleWindow.Handle, WM_ALLFINISH,0 , 0);
      FisWorking := False;
    end;    
  finally
    FCS.Leave;
  end;
end;

procedure TbbThreadPool.SetBlockCount(const Value: Integer);
var
  I: Integer;
begin
  FBlockCount := Value;
  FCS.Enter;
  try
    for I := 0 to FThreadList.Count - 1 do begin
      Tbb32bitToMonoThread(FThreadList.Items[i]).BlockCount := Value;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TbbThreadPool.SetBrightnessThreshold(const Value: double);
begin
  FBrightnessThreshold := Value;
end;

procedure TbbThreadPool.SetEventHandleWindow(const Value: THandleComponent);
begin
  FEventHandleWindow := Value;
end;

function TbbThreadPool.Start: Boolean;
var
  i : Integer;
begin
  FCS.Enter;
  try
    Result := (FisWorking = false);
    if Result = false then Exit;

    for I := 0 to ThreadCount - 1 do begin
      Tbb32bitToMonoThread(FThreadList.Items[i]).ColorBuffer := Self.ColorBuffer;
      Tbb32bitToMonoThread(FThreadList.Items[i]).MonoBuffer := Self.MonoBuffer;
      Tbb32bitToMonoThread(FThreadList.Items[i]).BlockCount := Self.BlockCount;
      Tbb32bitToMonoThread(FThreadList.Items[i]).BrightnessThreshold := FBrightnessThreshold;
      Tbb32bitToMonoThread(FThreadList.Items[i]).isWorking := true;
    end;
  finally
    FCS.Leave;
  end;
end;

{ Tbb32bitToMonoThread }

constructor Tbb32bitToMonoThread.Create(ThreadNumber, MaxThread : Integer);
begin

  FThreadNumber := ThreadNumber;
  FMaxThread := MaxThread;
  FisWorking := false;

  inherited Create(false);
end;

destructor Tbb32bitToMonoThread.Destroy;
begin

  inherited;
end;

procedure Tbb32bitToMonoThread.Execute;
var
  I: Integer;
  TmpColorBuff : Pbb32bitBlock;
  TmpMonoBuff : Pbb32bitBlock;
begin
  while not Terminated do begin
    while FisWorking do begin
      i := FThreadNumber;
      TmpColorBuff := FColorBuffer;
      TmpMonoBuff := FMonoBuffer;
      inc(TmpColorBuff, i);
      inc(TmpMonoBuff, i);
      while i < FBlockCount do begin
        Process(TmpColorBuff, TmpMonoBuff);

        Inc(TmpColorBuff, FMaxThread);
        Inc(TmpMonoBuff, FMaxThread);
        Inc(i, FMaxThread);
      end;
      FisWorking := false;
      PostMessage(EventOwner.Handle, WM_FINISHWORK, 0, 0);
    end;
    Sleep(5);
  end;
end;

procedure Tbb32bitToMonoThread.Process(AColorBlock, AMonoBlock : Pbb32bitBlock);
//컬러 블럭을 지지던지 볶던지 알아서 하시게나..
var
  i : Integer;
  j : Integer;
  Avr : Integer;
  CPtr, MPtr : PbbARGB;
  YSum : Int64;
  Y : Integer;
begin
  YSum := 0;
  for I := 0 to bbBlockSize - 1 do begin
    CPtr := AColorBlock.ScanLine(i);
    for j := 0 to bbBlockSize - 1 do begin
      Y := Round((CPtr.R * 0.299) + (CPtr.G * 0.587) + (CPTR.B * 0.114));
      YSum := Y + YSum;
      Inc(CPtr);
    end;
  end;

  Avr := YSum div (bbBlockSize * bbBlockSize);

  for I := 0 to bbBlockSize - 1 do begin
    CPtr := AColorBlock.ScanLine(i);
    MPtr := AMonoBlock.ScanLine(i);
    for j := 0 to bbBlockSize - 1 do begin
      Y := Round((CPtr.R * 0.299) + (CPtr.G * 0.587) + (CPTR.B * 0.114));

      if Avr * FBrightnessThreshold < Y then begin
        MPtr.R := 255;
        Mptr.G := 255;
        mptr.B := 255;
      end
      else begin
        MPtr.R := 0;
        Mptr.G := 0;
        mptr.B := 0;
      end;
      Inc(CPtr);
      Inc(MPtr);
    end;
  end;
end;

end.
