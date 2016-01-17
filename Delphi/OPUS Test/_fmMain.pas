unit _fmMain;

interface

uses
  WaveOut, WaveIn, SimpleThread, PacketBuffer, Opus,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  FRAMESIZE = 960;
  SAMPLERATE = 48000;
  CHANNELS = 1;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    FHandleIn : pointer;
    FBufferIn : pointer;
    FBufferInSize : integer;
    FWaveIn : TWaveIn;
    procedure on_DataIn_Data(Sender:TObject; AData:pointer; ASize:integer);
  private
    FPacketBufferIn : TPacketBuffer;
    FSimpleThreadIn : TSimpleThread;
    procedure on_In_Repeat(Sender:TObject);
  private
    FPacketBufferOut : TPacketBuffer;
    FSimpleThreadOut : TSimpleThread;
    procedure on_Out_Repeat(Sender:TObject);
  private
    FHandleOut : pointer;
    FBufferOut : pointer;
    FBufferOutSize : integer;
    FWaveOut : TWaveOut;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
var
  iError : integer;
begin
  FHandleIn := openEncoder( iError, SAMPLERATE, CHANNELS, 64000 );
  moMsg.Lines.Add( Format('opus_encoder_create: %d', [iError]) );

  FHandleOut := openDecoder( iError, SAMPLERATE, CHANNELS);
  moMsg.Lines.Add( Format('opus_decoder_create: %d', [iError]) );

  FWaveOut := TWaveOut.Create(Self);
  FWaveOut.Channels := CHANNELS;
  FWaveOut.SampleRate := SAMPLERATE;

  FWaveIn := TWaveIn.Create(Self);
  FWaveIn.SampleRate := SAMPLERATE;
  FWaveIn.Channels := CHANNELS;
  FWaveIn.BufferSize := FRAMESIZE;
  FWaveIn.OnData := on_DataIn_Data;

  FBufferInSize := 1024 * 16;
  GetMem( FBufferIn, FBufferInSize );

  FBufferOutSize := 1024 * 16;
  GetMem( FBufferOut, FBufferOutSize );

  FPacketBufferIn := TPacketBuffer.Create;
  FPacketBufferOut := TPacketBuffer.Create;

  FSimpleThreadIn := TSimpleThread.Create(on_In_Repeat);
  FSimpleThreadOut := TSimpleThread.Create(on_Out_Repeat);

  FWaveIn.Start;
  FWaveOut.Start;
end;

procedure TfmMain.on_DataIn_Data(Sender: TObject; AData: pointer;
  ASize: integer);
begin
  FPacketBufferIn.Add( AData, ASize );
  FSimpleThreadIn.WakeUp;
end;

procedure TfmMain.on_In_Repeat(Sender: TObject);
var
  Data : pointer;
  Size : integer;
  iSizeIn : integer;
begin
  while not FSimpleThreadIn.Terminated do begin
    while FPacketBufferIn.GetPacket(Data, Size) do begin
      try
        iSizeIn := encode( FHandleIn, Data, Size, FBufferIn, FBufferInSize );
        if iSizeIn > 0 then begin
          FPacketBufferOut.Add( FBufferIn, iSizeIn );
          FSimpleThreadOut.WakeUp;
        end;
      finally
        if Data <> nil then FreeMem(Data);
      end;
    end;

    FSimpleThreadIn.SleepTight;
  end;
end;

procedure TfmMain.on_Out_Repeat(Sender: TObject);
var
  Data : pointer;
  Size : integer;
  iSizeOut : integer;
begin
  while not FSimpleThreadOut.Terminated do begin
    while FPacketBufferOut.GetPacket(Data, Size) do begin
      try
        iSizeOut := decode( FHandleOut, Data, Size, FBufferOut, FBufferOutSize);
        if iSizeOut > 0 then FWaveOut.Play( FBufferOut, iSizeOut );
      finally
        if Data <> nil then FreeMem(Data);
      end;
    end;

    FSimpleThreadOut.Sleep(1);
  end;
end;

end.

