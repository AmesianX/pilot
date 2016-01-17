(*
    Video Grabber 가 설치 되어 있어야 합니다.
*)
unit WebCam;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, VidGrab;

type
  TfrCam = class(TFrame)
    VideoGrabber: TVideoGrabber;
    procedure VideoGrabberFrameBitmap(Sender: TObject; FrameInfo: pFrameInfo;
      BitmapInfo: pFrameBitmapInfo);
    procedure VideoGrabberResize(Sender: TObject);
  private
    procedure CopyFrameToBitmap(BitmapInfo: pFrameBitmapInfo);
  private
    FCaptureTick: Cardinal;
    FBitmap : TBitmap;
    FTick : Cardinal;

    procedure SetDeviceNo(const Value: integer);
    function GetDeviceNum: integer;
    function GetDeviceCount: integer;
    function GetDeviceNames(Index: integer): string;
    procedure SetCaptureTick(const Value: Cardinal);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Start:boolean;
    procedure Stop;

    function GetBitmap(ABitmap:TBitmap):boolean; overload;
    function GetBitmap(ABitmap:TBitmap; Width,Height:integer):boolean; overload;

    function GetJpeg(AStream:TStream; Quality:integer=100):boolean; overload;
    function GetJpeg(AStream:TStream; Width,Height:integer; Quality:integer=100):boolean; overload;

    property DeviceNames[Index:integer] : string read GetDeviceNames;
  published
    property DeviceNo : integer read GetDeviceNum write SetDeviceNo;
    property DeviceCount : integer read GetDeviceCount;
    property CaptureTick : Cardinal read FCaptureTick write SetCaptureTick;
  end;

implementation

uses
  ads_BitmapResize, JpegEncoder;
  
{$R *.dfm}

{ TFrame1 }

constructor TfrCam.Create(AOwner: TComponent);
begin
  inherited;
  
  FBitmap := TBitmap.Create;

  VideoGrabber.Display_AutoSize := true;  
  VideoGrabber.AutoRefreshPreview := true;
  VideoGrabber.UseNearestVideoSize(320, 240, true);

  FTick := GetTickCount;
  FCaptureTick := 200;
end;

destructor TfrCam.Destroy;
begin
  FBitmap.Free;
  
  inherited;
end;

function TfrCam.GetBitmap(ABitmap: TBitmap): boolean;
begin
  Result := not FBitmap.Empty;
  Result := Result and (ABitmap <> nil);
  if Result then begin
    ABitmap.Assign(FBitmap);
  end;
end;

function TfrCam.GetBitmap(ABitmap: TBitmap; Width, Height: integer): boolean;
begin
  Result := not FBitmap.Empty;
  Result := Result and (ABitmap <> nil);
  Result := Result and (Width > 0) and (Height > 0 );

  BitmapResize(FBitmap, ABitmap, Height, Width);
end;

function TfrCam.GetDeviceCount: integer;
var
  List : TStringList;
begin
  List := TStringList.Create;
  try
    List.Text := VideoGrabber.VideoDevices;
    Result := List.Count;
  finally
    List.Free;
  end;
end;

function TfrCam.GetDeviceNames(Index: integer): string;
var
  List : TStringList;
begin
  List := TStringList.Create;
  try
    List.Text := VideoGrabber.VideoDevices;
    Result := List.Strings[Index];
  finally
    List.Free;
  end;
end;

function TfrCam.GetDeviceNum: integer;
begin
  Result := VideoGrabber.VideoDevice
end;

function TfrCam.GetJpeg(AStream: TStream; Quality: integer): boolean;
begin
  Result := not FBitmap.Empty;
  Result := Result and (AStream <> nil);
  BitmapToJpeg(FBitmap, AStream, Quality);
end;

function TfrCam.GetJpeg(AStream: TStream; Width, Height,
  Quality: integer): boolean;
var
  TempBitmap : TBitmap;
begin
  Result := not FBitmap.Empty;
  Result := Result and (AStream <> nil);
  Result := Result and (Width > 0) and (Height > 0 );

  TempBitmap := TBitmap.Create;
  try
    GetBitmap(TempBitmap, Width, Height);
    BitmapToJpeg(TempBitmap, AStream, Quality);
  finally
    TempBitmap.Free;
  end;
end;

procedure TfrCam.SetCaptureTick(const Value: Cardinal);
begin
  FCaptureTick := Value;
end;

procedure TfrCam.SetDeviceNo(const Value: integer);
begin
  VideoGrabber.VideoDevice := Value;
end;

function TfrCam.Start: boolean;
begin
  Result := VideoGrabber.StartPreview;
end;

procedure TfrCam.Stop;
begin
  VideoGrabber.StopPreview;
end;

procedure TfrCam.VideoGrabberFrameBitmap(Sender: TObject; FrameInfo: pFrameInfo;
  BitmapInfo: pFrameBitmapInfo);
var
  NowTick : Cardinal;
begin
  NowTick := GetTickCount;
  if (NowTick - FTick) > CaptureTick  then begin
    CopyFrameToBitmap(BitmapInfo);
    FTick := NowTick;
  end;
end;

procedure TfrCam.VideoGrabberResize(Sender: TObject);
begin
  VideoGrabber.UseNearestVideoSize(VideoGrabber.Width, VideoGrabber.Height, true);
end;

procedure TfrCam.CopyFrameToBitmap (BitmapInfo: pFrameBitmapInfo);
begin
   FBitmap.Canvas.Lock;
   FBitmap.Width := BitmapInfo^.BitmapWidth;
   FBitmap.Height := BitmapInfo^.BitmapHeight;
   case BitmapInfo^.BitmapBitsPerPixel of
      32: FBitmap.PixelFormat := pf32Bit;
      24: FBitmap.PixelFormat := pf24Bit;
      16: FBitmap.PixelFormat := pf16Bit;
      15: FBitmap.PixelFormat := pf15Bit;
       8: FBitmap.PixelFormat := pf8Bit;
    else  FBitmap.PixelFormat := pf24Bit;
   end;
   BitBlt (FBitmap.Canvas.Handle, 0, 0, BitmapInfo^.BitmapWidth, BitmapInfo^.BitmapHeight, BitmapInfo^.BitmapDC, 0, 0, SRCCOPY);
   FBitmap.Canvas.Unlock;
end;

end.
