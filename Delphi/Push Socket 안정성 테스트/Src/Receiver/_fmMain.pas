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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FCount : integer;
    FPing : byte;
    FTick : cardinal;
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

{ TfmMain }

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FRepeater.Stop;
  FSocket.Disconnect;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FCount := 0;
  FPing := _Ping;
  FTick := GetTickCount;

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
  FreeAndNil(FRepeater);
end;

procedure TfmMain.on_Disconnected(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfmMain.on_Execute(Sender: TObject);
begin
  FSocket.Send(@FPing, SizeOf(FPing));
  InterlockedIncrement(FCount);

  Sleep(20);
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
