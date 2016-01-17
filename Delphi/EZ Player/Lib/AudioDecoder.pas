unit AudioDecoder;

interface

uses
  RyuMPEG, WaveOut, PacketBuffer, SimpleThread,
  Classes, SysUtils, Graphics;

const
  WAVEOUT_INPUT_LIMIT = 4;

type
  TAudioDecoder = class
  private
    FStream : TRyuMPEGStream;
    FAudioBuffer : pointer;
    FWaveOut : TWaveOut;
    FPacketBuffer : TPacketBuffer;
  private
    FSimpleThread : TSimpleThread;
    procedure on_Execute(Sender:TObject);
    function GetIsBusy: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open(AStream:TRyuMPEGStream);
    procedure Close;

    procedure DataIn(AData:pointer; ASize:integer);

    property IsBusy : boolean read GetIsBusy;
  end;

implementation

{ TAudioDecoder }

procedure TAudioDecoder.Close;
begin
  FStream := nil;
  FPacketBuffer.Clear;
  FWaveOut.Stop;
end;

constructor TAudioDecoder.Create;
begin
  inherited;

  GetMem(FAudioBuffer, AVCODEC_MAX_AUDIO_FRAME_SIZE * 2);

  FWaveOut := TWaveOut.Create(nil);
  FPacketBuffer := TPacketBuffer.Create;
  FSimpleThread := TSimpleThread.Create(on_Execute);
end;

procedure TAudioDecoder.DataIn(AData: pointer; ASize: integer);
var
  SizeOut : integer;
begin
  if decodeAudioPacket(FStream.Handle, AData, ASize, FAudioBuffer, SizeOut) then begin
    FPacketBuffer.Add(FAudioBuffer, SizeOut);
    FSimpleThread.WakeUp;
  end;
end;

destructor TAudioDecoder.Destroy;
begin
  FSimpleThread.Terminate;

  FreeMem(FAudioBuffer);

  FreeAndNil(FWaveOut);
  FreeAndNil(FPacketBuffer);
  FreeAndNil(FSimpleThread);

  inherited;
end;

function TAudioDecoder.GetIsBusy: boolean;
begin
  Result := (FPacketBuffer.Count + FWaveOut.DataInBuffer) > WAVEOUT_INPUT_LIMIT;
end;

procedure TAudioDecoder.on_Execute(Sender: TObject);
var
  Data : pointer;
  Size : integer;
begin
  while not FSimpleThread.Terminated do begin
    while FPacketBuffer.GetPacket(Data, Size) do begin
      try
        FWaveOut.Play(Data, Size);
      finally
        if Data <> nil then FreeMem(Data);
      end;
    end;

    FSimpleThread.SleepTight;
  end;
end;

procedure TAudioDecoder.Open(AStream: TRyuMPEGStream);
begin
  FStream := AStream;

  FWaveOut.Channels :=   AStream.MediaInfo.AudioInfo.Channels;
  FWaveOut.SampleRate := AStream.MediaInfo.AudioInfo.SampleRate;
  FWaveOut.Start;
end;

end.
