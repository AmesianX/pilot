unit Unit1;

interface

uses
  VoiceRecorder, MicList, VoicePlayer,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, JvProgressBar, JvXPProgressBar, VidGrab;

type
  TForm1 = class(TForm)
    Button1: TButton;
    cbMic: TComboBox;
    cbWebCam: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    pbMic: TJvXPProgressBar;
    tbVolume: TTrackBar;
    TimerPreview: TTimer;
    TimerVolumeGauge: TTimer;
    VideoGrabber: TVideoGrabber;
    procedure FormCreate(Sender: TObject); 
    procedure FormDestroy(Sender: TObject);
    procedure cbMicChange(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure TimerPreviewTimer(Sender: TObject);
    procedure TimerVolumeGaugeTimer(Sender: TObject);
  private
    FIsStarting : Boolean;
    FVoiceRecorder : TVoiceRecorder;
    FVoicePlayer : TVoicePlayer;

    procedure OnSound(Sender:TObject; AData:pointer; ASize:integer);
    procedure DispalyMicList;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.cbMicChange(Sender: TObject);
var
  FMicDeviceID : Cardinal;
begin
  FMicDeviceID := Cardinal(cbMic.Items.Objects[cbMic.ItemIndex]);

  FVoiceRecorder.Stop;
  FVoiceRecorder.Start(FMicDeviceID);
end;

procedure TForm1.DispalyMicList;
var
  MicList: TMicList;
  i: Integer;
begin
  cbMic.Clear;

  MicList := TMicList.Create;
  try
    MicList.Get;

    for i:=0 to MicList.Count-1 do
    begin
      cbMic.Items.AddObject(
        MicList.Items[i].ProductName,
        TObject(MicList.Items[i].DeviceID)
      );
    end;
  finally
    MicList.Free;
  end;

  if cbMic.Items.Count <= 0 then
    cbMic.Items.AddObject('¾øÀ½', TObject(-1));

  cbMic.ItemIndex := 0;
  cbMicChange(Self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIsStarting := false;
  FVoiceRecorder := TVoiceRecorder.Create(Self);
  FVoiceRecorder.OnNewData := OnSound;
  //FVoiceRecorder.Quality := 100;

  FVoicePlayer := TVoicePlayer.Create(Self);
  FVoicePlayer.DeleteLowVolume := false;
  //FVoicePlayer.Mute := false;
  FVoicePlayer.Volume := 50;
  
  cbWebCam.Items.Text := VideoGrabber.VideoDevices;
  if cbWebCam.Items.Count > 0 then cbWebCam.ItemIndex := 0;

  DispalyMicList;

  tbVolumeChange(tbVolume);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FVoiceRecorder.Free;
end;

procedure TForm1.OnSound(Sender: TObject; AData: pointer; ASize: integer);
begin
  //FVoicePlayer.DataIn(AData, ASize);
end;

procedure TForm1.tbVolumeChange(Sender: TObject);
begin
  FVoiceRecorder.Volume := tbVolume.Position;
end;

procedure TForm1.TimerVolumeGaugeTimer(Sender: TObject);
const
  MAXIMUM_OF_WORD_TYPE = 32768;
var
  Volume : Integer;
begin
  Volume := Round(Abs(FVoiceRecorder.HighVolume / MAXIMUM_OF_WORD_TYPE * 100));
  Volume := Round(Volume * (tbVolume.Position / 100));

  pbMic.Position := Volume;

  if pbMic.Position > 90 then begin
     pbMic.BarColorFrom := clRed;
     pbMic.BarColorTo := clFuchsia;
  end
  else if pbMic.Position > 80 then begin
    pbMic.BarColorFrom := clYellow;
    pbMic.BarColorTo := clYellow + $00200000;
  end
  else begin
    pbMic.BarColorFrom := clGreen;
    pbMic.BarColorTo := clLime;
  end
end;

procedure TForm1.TimerPreviewTimer(Sender: TObject);
begin
  if FIsStarting = true then exit;
  FIsStarting := VideoGrabber.StartPreview;
end;

end.
