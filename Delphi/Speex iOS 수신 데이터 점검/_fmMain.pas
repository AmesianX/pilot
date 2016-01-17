unit _fmMain;

interface

uses
  Speex, SpeexEncoder, SpeexDecoder, WaveOut,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    dlgOpen: TOpenDialog;
    bt1: TButton;
    procedure bt1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSampleRate : integer;
    FChannels : integer;
    FWaveOut : TWaveOut;
    FSpeexDecoder : TSpeexDecoder;
    procedure on_Decode(Sender:TObject; AData:pointer; ASize,AVolume:integer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.bt1Click(Sender: TObject);
var
  Stream : TFileStream;
  Buffer : array [0..1024-1] of byte;
begin
  if not dlgOpen.Execute then Exit;

  Stream := TFileStream.Create(dlgOpen.FileName, fmOpenRead);
  try
    while Stream.Position < Stream.Size do begin
      Stream.Read(Buffer, 62);
      FSpeexDecoder.Excute(@Buffer, 62);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FSampleRate := 8000;
  FChannels := 1;

  FSpeexDecoder := TSpeexDecoder.Create(Self);
  FSpeexDecoder.OnNewData := on_Decode;
  FSpeexDecoder.Start;

  FWaveOut := TWaveOut.Create(Self);
  FWaveOut.SampleRate := FSampleRate;
  FWaveOut.Channels := FChannels;

  FWaveOut.Start;
end;

procedure TfmMain.on_Decode(Sender: TObject; AData: pointer; ASize,
  AVolume: integer);
begin
  FWaveOut.Play(AData, ASize);
end;

end.
