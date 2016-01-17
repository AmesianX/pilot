unit _fmMain;

interface

uses
  Utils, PushClient, ThreadRepeater,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
  private
    FCount : integer;
    FPing : byte;
    FDeskBuffer : packed array [0..1024] of byte;
    FVoiceBuffer : packed array [0..62] of byte;
  private
    FSocket : TPushClient;
    procedure on_Received(Sender:TObject; AData:pointer; ASize:integer);
    procedure on_Disconnected(Sender:TObject);
  private
    FRepeater : TThreadRepeater;
    procedure on_Execute(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FRepeater.Stop;
  FSocket.Disconnect;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FCount := 0;

  FPing := _Ping;
  FVoiceBuffer[0] := _Voice;
  FDeskBuffer[0]  := _DeskTop;
  
  FRepeater := TThreadRepeater.Create(Self);
  FRepeater.TimeOut := 250;

  FSocket := TPushClient.Create(Self);
  FSocket.Host := _ServerHost;
  FSocket.Port := _ServerPort;
  FSocket.OnReceived := on_Received;
  FSocket.OnDisconnected := on_Disconnected;

  if not FSocket.Connect then begin
    Application.Terminate;
    Exit;
  end;

  FRepeater.Execute(on_Execute);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSocket);
  FreeAndNil(FRepeater);
end;

procedure TfmMain.on_Disconnected(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfmMain.on_Execute(Sender: TObject);
var
  Loop: Integer;
begin
  if not FSocket.Connected then begin
    Sleep(1);
    Exit;
  end;

  // DeskTop 화면은 음성 패킷 하나에 최대 64KB로 한정
  for Loop := 1 to 64 do FSocket.Send(@FDeskBuffer, SizeOf(FDeskBuffer));
  FSocket.Send(@FVoiceBuffer, SizeOf(FVoiceBuffer));

  FSocket.Send(@FPing, SizeOf(FPing));
  InterlockedIncrement(FCount);

  // 음성은 20ms 마다 보내야 하나, 패킷 전송 지연으로 인해서 그보다 작은 수만큼 쉰다.
  Sleep(5);
end;

procedure TfmMain.on_Received(Sender: TObject; AData: pointer; ASize: integer);
var
  pPacketFind : ^byte;
begin
  pPacketFind := AData;

  if pPacketFind^ = _Ping then InterlockedDecrement(FCount);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  Caption := Format('Packet Send-Receive: %d', [FCount]);
end;

end.
