unit DeskTopCapture;

interface

uses
  MegaCastUtils, Capture, ScreenCapture,
  Windows, Classes, SysUtils, ExtCtrls, SyncObjs, Graphics;

type
  TDeskTopCapture = class
  private
    FTimer : TTimer;
    FBitmap : TBitmap;
    FScreenCapture : TScreenCapture;
    FCS : TCriticalSection;
    procedure on_Timer(Sender:TObject);
    procedure do_Capture;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    function GetFrame(var AData:pointer; var ASize:integer; var AFrameSize:TFrameSize):boolean;

    procedure SetCaptureRect(ARect:TRect);
  end;

implementation

{ TDeskTopCapture }

constructor TDeskTopCapture.Create;
begin
  inherited;

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := _PixelFormat;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 150;
  FTimer.Enabled := true;

  FScreenCapture := TScreenCapture.Create(nil);
  FScreenCapture.CaptureType := ctRect;
  FScreenCapture.PixelFormat := _PixelFormat;
  FScreenCapture.WithCursor := true;

  FCS := TCriticalSection.Create;

  SetCaptureRect(Rect(0, 0, 1024, 768));
end;

destructor TDeskTopCapture.Destroy;
begin
  Stop;

  FreeAndNil(FBitmap);
  FreeAndNil(FTimer);
  FreeAndNil(FScreenCapture);
  FreeAndNil(FCS);

  inherited;
end;

procedure TDeskTopCapture.do_Capture;
begin
  FCS.Enter;
  try
    FScreenCapture.Capture;
    FBitmap.Canvas.Draw(0, 0, FScreenCapture.Bitmap);
  finally
    FCS.Leave;
  end;
end;

function TDeskTopCapture.GetFrame(var AData: pointer; var ASize: integer; var AFrameSize: TFrameSize): boolean;
begin
  FCS.Enter;
  try
    AData := nil;
    ASize := FBitmap.Width * FBitmap.Height * _PixelSize;
    AFrameSize.Width  := FBitmap.Width;
    AFrameSize.Height := FBitmap.Height;

    Result := ASize > 0;
    if not Result then Exit;

    GetMem(AData, ASize);
    Move(FBitmap.ScanLine[FBitmap.Height-1]^, AData^, ASize);
  finally
    FCS.Leave;
  end;
end;

procedure TDeskTopCapture.on_Timer(Sender: TObject);
begin
  FTimer.Enabled := false;
  try
    do_Capture;
  finally
    FTimer.Enabled := true;
  end;
end;

procedure TDeskTopCapture.SetCaptureRect(ARect: TRect);
var
  bWidth, bHeight, iWidth, iHeight : integer;
begin
  FCS.Enter;
  try
    FScreenCapture.X      := ARect.Left;
    FScreenCapture.Y      := ARect.Top;
    FScreenCapture.Width  := ARect.Right;
    FScreenCapture.Height := ARect.Bottom;

    bWidth  := Abs(ARect.Right  - ARect.Left);
    bHeight := Abs(ARect.Bottom - ARect.Top);

    iWidth := bWidth div _BlockSize;
    if (bWidth mod _BlockSize) <> 0 then Inc(iWidth);

    iHeight := bHeight div _BlockSize;
    if (bHeight mod _BlockSize) <> 0 then Inc(iHeight);

    FBitmap.Width  := iWidth  * _BlockSize;
    FBitmap.Height := iHeight * _BlockSize;
  finally
    FCS.Leave;
  end;
end;

procedure TDeskTopCapture.Start;
begin
  FTimer.OnTimer := on_Timer;
end;

procedure TDeskTopCapture.Stop;
begin
  FTimer.OnTimer := nil;
end;

end.
