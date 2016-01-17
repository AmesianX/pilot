unit bbSnapShot;

interface

uses
  bbUtils,
  HandleComponent,
  SysUtils, Windows, Classes, Graphics, VidGrab, Forms, Dialogs;

type
  TbbSnapShot = class
  public
    Procedure Capture(ABitmap : TBitmap);

    constructor Create(EventHandleWindow : THandleComponent);
    destructor Destroy; override;
  private
    FGrabber : TVideoGrabber;
    FTempBitmap : TBitmap;
    FEventHandleWindow : THandleComponent;
    FResolution : Integer;
    
    procedure FrameCaptureComplete(Sender: TObject;
      FrameBitmap: TBitmap; BitmapWidth, BitmapHeight: Integer;
      FrameNumber: Cardinal; FrameTime: Int64; DestType: TFrameCaptureDest;
      FileName: String; Success: Boolean; FrameId: Integer);

    procedure VideoGrabber1VideoDeviceSelected(Sender: TObject);
    procedure VideoGrabber1ResizeVideo(Sender: TObject; SourceWidth,
                 SourceHeight: Integer);
    function GetCamaraNo: Integer;
    procedure SetCameraNo(const Value: Integer);
    function GetResolution: Integer;
    procedure SetResolution(const Value: Integer);
  public
    property CameraNo : Integer read GetCamaraNo write SetCameraNo;
    property Resolution : Integer read GetResolution write SetResolution;
  end;

implementation

{ TbbSnapShot }

constructor TbbSnapShot.Create(EventHandleWindow : THandleComponent);
begin
  FEventHandleWindow := EventHandleWindow;

  FGrabber := TVideoGrabber.Create(nil);
//¼¼ÆÃ
  FGrabber.OnFrameCaptureCompleted := FrameCaptureComplete;
  FGrabber.OnVideoDeviceSelected := VideoGrabber1VideoDeviceSelected;
  FGrabber.OnResizeVideo := VideoGrabber1ResizeVideo;

  FGrabber.VideoDevice := 0;
  //FResolution := FGrabber.VideoSizesCount - 1;
  FGrabber.StartPreview;
end;

destructor TbbSnapShot.Destroy;
begin
  FGrabber.StopPreview;
  FGrabber.Stop;
  FGrabber.Free;

  inherited;
end;

procedure TbbSnapShot.FrameCaptureComplete(Sender: TObject;
  FrameBitmap: TBitmap; BitmapWidth, BitmapHeight: Integer;
  FrameNumber: Cardinal; FrameTime: Int64; DestType: TFrameCaptureDest;
  FileName: String; Success: Boolean; FrameId: Integer);
begin
  FTempBitmap.Assign(FrameBitmap);
  PostMessage(FEventHandleWindow.Handle, WM_CAPTURECOMPLETE, 0, 0)
end;

function TbbSnapShot.GetCamaraNo: Integer;
begin
  Result := FGrabber.VideoDevice;
end;

function TbbSnapShot.GetResolution: Integer;
begin
  Result := FGrabber.VideoSize;
  FResolution := Result;
end;

procedure TbbSnapShot.SetCameraNo(const Value: Integer);
begin
  FGrabber.VideoDevice := Value;
  FGrabber.StartPreview;
end;

procedure TbbSnapShot.SetResolution(const Value: Integer);
begin
  FGrabber.StopPreview;
  FGrabber.VideoSize := Value;
  FResolution := Value;
  FGrabber.StartPreview;
end;

procedure TbbSnapShot.VideoGrabber1ResizeVideo(Sender: TObject; SourceWidth, SourceHeight: Integer);
begin
  //FGrabber.VideoSize := FResolution;
end;

procedure TbbSnapShot.VideoGrabber1VideoDeviceSelected(Sender: TObject);
begin
  FGrabber.VideoSize := FGrabber.VideoSizesCount - 1;
  FResolution := FGrabber.VideoSize;
end;

procedure TbbSnapShot.Capture(ABitmap: TBitmap);
begin
  FTempBitmap := ABitmap;
  FGrabber.VideoSize := FResolution;
  FGrabber.CaptureFrameTo(fc_TBitmap, '');
end;

end.
