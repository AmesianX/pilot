unit VideoDecoder;

interface

uses
  RyuMPEG,
  Classes, SysUtils, Graphics;

type
  TVideoDecoder = class
  private
    FStream : TRyuMPEGStream;
    FVideoBuffer : pointer;
    FBitmap: TBitmap;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open(AStream:TRyuMPEGStream);
    procedure Close;

    function Decode(AData:pointer; ASize:integer):boolean;

    property Bitmap : TBitmap read FBitmap;
  end;

implementation

{ TVideoDecoder }

procedure TVideoDecoder.Close;
begin
  FStream := nil;

  if FVideoBuffer <> nil then begin
    FreeMem(FVideoBuffer);
    FVideoBuffer := nil;
  end;
end;

constructor TVideoDecoder.Create;
begin
  inherited;

  FVideoBuffer := nil;

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;
  FBitmap.Canvas.Brush.Color := clBlack;
end;

function TVideoDecoder.Decode(AData: pointer; ASize: integer): boolean;
begin
  Result := decodeVideoData(FStream.Handle, AData, ASize, FVideoBuffer);
  if Result then BitmapUpsideDown(FVideoBuffer, FBitmap);
end;

destructor TVideoDecoder.Destroy;
begin
  Close;

  FreeAndNil(FBitmap);

  inherited;
end;

procedure TVideoDecoder.Open(AStream: TRyuMPEGStream);
begin
  FStream := AStream;

  FBitmap.Width  := AStream.MediaInfo.VideoInfo.Width;
  FBitmap.Height := AStream.MediaInfo.VideoInfo.Height;

  GetMem(FVideoBuffer, FBitmap.Width * FBitmap.Height * PIXEL_SIZE);
end;

end.
