unit _frMicSetting;

interface

uses
  ValueList, View, MicList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Gauges;

type
  TfrMicSetting = class(TFrame)
    Gauge: TGauge;
    ImageMic: TImage;
    lblAmp: TLabel;
    sbVolume: TScrollBar;
    seBooster: TSpinEdit;
    cbMicSelect: TComboBox;
    lblMic: TLabel;
    procedure VolumeChange(Sender: TObject);
    procedure cbMicSelectChange(Sender: TObject);
  private
    FTimer: TTimer;
    FMicDeviceID: Integer;
    procedure on_Timer(Sender: TObject);
    function GetVolume: Integer;
    procedure SetVolume(const Value: Integer);
    procedure DispalyMicList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Volume: Integer read GetVolume write SetVolume;
    property MicDeviceID: Integer read FMicDeviceID;

    procedure rp_SetMic(APacket: TValueList);
  end;

implementation

uses
  Global, Option;

{$R *.dfm}

procedure TfrMicSetting.cbMicSelectChange(Sender: TObject);
begin
  FMicDeviceID := Cardinal(cbMicSelect.Items.Objects[cbMicSelect.ItemIndex]);
  TOption.Obj.MicDeviceID := FMicDeviceID;

  if TGlobal.Obj.VoiceClient.IsStarted then
  begin
    TGlobal.Obj.VoiceClient.Stop;
    TGlobal.Obj.VoiceClient.Start(TOption.Obj.MicDeviceID);
  end;
end;

constructor TfrMicSetting.Create(AOwner: TComponent);
begin
  inherited;

  TView.Obj.Add(Self);

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 25;
  FTimer.Enabled := False;
  FTimer.OnTimer := on_Timer;

  DispalyMicList;

  sbVolume.Position := 100;
  seBooster.Value := 1;
end;

destructor TfrMicSetting.Destroy;
begin
  FTimer.Free;

  TView.Obj.Remove(Self);

  inherited;
end;

procedure TfrMicSetting.DispalyMicList;
var
  MicList: TMicList;
  i: Integer;
begin
  cbMicSelect.Clear;

  MicList := TMicList.Create;
  try
    MicList.Get;

    for i:=0 to MicList.Count-1 do
    begin
      cbMicSelect.Items.AddObject(
        MicList.Items[i].ProductName,
        TObject(MicList.Items[i].DeviceID)
      );
    end;
  finally
    MicList.Free;
  end;

  if cbMicSelect.Items.Count <= 0 then
    cbMicSelect.Items.AddObject('¾øÀ½', TObject(-1));

  cbMicSelect.ItemIndex := 0;
  cbMicSelectChange(Self);
end;

function TfrMicSetting.GetVolume: Integer;
begin
  Result := sbVolume.Position;
end;

procedure TfrMicSetting.rp_SetMic(APacket: TValueList);
var
  UserID: string;
  OnOff: Boolean;
begin
  UserID := APacket.Values['UserID'];
  OnOff := APacket.Boolean['OnOff'];

  if UpperCase(UserID) = UpperCase(TOption.Obj.UserID) then
  begin
    FTimer.Enabled := OnOff;
    if not OnOff then
      Gauge.Progress := 0;
  end;
end;

procedure TfrMicSetting.VolumeChange(Sender: TObject);
begin
  TGlobal.Obj.VoiceClient.Recorder.Volume := (sbVolume.Position * seBooster.Value) / 100;
end;

procedure TfrMicSetting.SetVolume(const Value: Integer);
begin
  sbVolume.Position := Value;
end;

procedure TfrMicSetting.on_Timer(Sender: TObject);
begin
  Gauge.Progress := Round(Abs(TGlobal.Obj.VoiceClient.Recorder.HighVolume * 100 / 32768));

  if Gauge.Progress > 90 then Gauge.ForeColor := clRed
  else if Gauge.Progress > 80 then Gauge.ForeColor := clYellow
  else Gauge.ForeColor := clGreen;
end;

end.
