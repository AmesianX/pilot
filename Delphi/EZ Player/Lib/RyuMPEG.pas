unit RyuMPEG;

interface

uses
  Classes, SysUtils, Graphics;

const
  AVCODEC_MAX_AUDIO_FRAME_SIZE = 192000; // 1 second of 48khz 32bit audio

  PIXEL_FORMAT = pf32bit;
  PIXEL_SIZE = 4;

  UNKNOWN_PACKET = 0;
  VIDEO_PACKET = 1;
  AUDIO_PACKET = 2;

  READ_BUFFER_SIZE = 16 * 1024 * 1024;

type
  TVideoInfo = packed record
	  CodecID, PixelFormat, Width, Height, fps, bitrate, frameSize : integer;
  end;

  TAudioInfo = packed record
	  CodecID, SampleRate, Channels : integer;
  end;

  TMediaInfo = packed record
    VideoInfoString : packed array [0..1024*16-1] of AnsiChar;
    AudioInfoString : packed array [0..1024*16-1] of AnsiChar;

    Duration : int64;

    VideoInfo : TVideoInfo;
    AudioInfo : TAudioInfo;
  end;
  PMediaInfo = ^TMediaInfo;

  TRyuMPEGStream = class
  private
    FHandle : pointer;
    FMediaInfo : TMediaInfo;
    FPacketBuffer : pointer;
    FPacketSize, FPacketType, FPostion : integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Open(AFileName:string):boolean;
    procedure Close;

    function ReadPacket:boolean;

    property Handle : pointer read FHandle;
    property MediaInfo : TMediaInfo read FMediaInfo;
    property PacketBuffer : pointer read FPacketBuffer;
    property PacketSize : integer read FPacketSize;
    property PacketType : integer read FPacketType;
    property Postion : integer read FPostion;
  end;

function  openRyuMPEG(AFileName:PAnsiChar; var AErrorCode:integer):pointer;
          cdecl; external 'libRyuMPEG.dll';

procedure closeRyuMPEG(AHandle:pointer);
          cdecl; external 'libRyuMPEG.dll';

procedure getVideoInfo(AHandle:pointer; var AVideoInfo:TVideoInfo);
          cdecl; external 'libRyuMPEG.dll';

procedure getAudioInfo(AHandle:pointer; var AAudioInfo:TAudioInfo);
          cdecl; external 'libRyuMPEG.dll';

procedure getMediaInfo(AHandle:pointer; var AMediaInfo:TMediaInfo);
          cdecl; external 'libRyuMPEG.dll';

function  readData(AHandle:pointer; AData:pointer; var ASize:integer;
          var APacketType,APostion:integer):boolean;
          cdecl; external 'libRyuMPEG.dll';

function  decodeVideoData(AHandle:pointer; AData:pointer; ASize:integer;
          ABitmap:pointer):boolean;
          cdecl; external 'libRyuMPEG.dll';

function  decodeAudioPacket(AHandle:pointer; AData:pointer; ASize:integer;
          AAudio:pointer; var AAudioSize):boolean;
          cdecl; external 'libRyuMPEG.dll';

procedure BitmapUpsideDown(AData:pointer; ABitmap:TBitmap);

implementation

procedure BitmapUpsideDown(AData:pointer; ABitmap:TBitmap);
var
  pSrc, pDst : PByte;
  Loop, iLineSize : Integer;
begin
  Assert(ABitmap.PixelFormat = PIXEL_FORMAT, 'ABitmap.PixelFormat <> PIXEL_FORMAT');

  pSrc := AData;
  pDst := ABitmap.ScanLine[0];

  iLineSize := ABitmap.Width * PIXEL_SIZE;

  for Loop := 0 to ABitmap.Height - 1 do begin
    Move(pSrc^, pDst^, iLineSize);

    Inc(pSrc, iLineSize);
    Dec(pDst, iLineSize);
  end;
end;

{ TRyuMPEGStream }

procedure TRyuMPEGStream.Close;
begin
  if FHandle <> nil then begin
    closeRyuMPEG(FHandle);
    FHandle := nil;
  end;
end;

constructor TRyuMPEGStream.Create;
begin
  inherited;

  FHandle := nil;
  GetMem(FPacketBuffer, READ_BUFFER_SIZE);
end;

destructor TRyuMPEGStream.Destroy;
begin
  Close;

  inherited;
end;

function TRyuMPEGStream.Open(AFileName: string): boolean;
var
  iErrorCode : integer;
begin
  Close;

  FHandle := openRyuMPEG(PAnsiChar(AnsiString(AFileName)), iErrorCode);
  Result := iErrorCode = 0;

  if Result then begin
    getMediaInfo(FHandle, FMediaInfo);
  end else begin
    FHandle := nil;
  end;
end;

function TRyuMPEGStream.ReadPacket: boolean;
begin
  Result := readData(FHandle, FPacketBuffer, FPacketSize, FPacketType, FPostion);
end;

end.

