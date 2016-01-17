unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  DXSUtil, DirectShow9, WaveInDeviceList;

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
var
  i: Integer;
  AudioDevEnum: TSysDevEnum;
  WaveInDeviceList : TWaveInDeviceList;
begin
  Memo1.Lines.Add('TSysDevEnum');
  AudioDevEnum := TSysDevEnum.Create(CLSID_AudioInputDeviceCategory);
  if AudioDevEnum.CountFilters > 0 then
  begin
    for i := 0 to AudioDevEnum.CountFilters - 1 do
    begin
      Memo1.Lines.Add(AudioDevEnum.Filters[i].FriendlyName);
    end;
  end;
  Memo1.Lines.Add('');

  Memo1.Lines.Add('TWaveInDeviceList');
  WaveInDeviceList := TWaveInDeviceList.Create;
  try
    for i := 0 to WaveInDeviceList.Count-1 do Memo1.Lines.Add(WaveInDeviceList.Items[i].ProductName);
  finally
    WaveInDeviceList.Free;
  end;
end;

end.
