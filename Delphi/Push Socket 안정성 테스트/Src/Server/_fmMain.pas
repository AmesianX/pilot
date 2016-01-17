unit _fmMain;

interface

uses
  Utils, PushServer,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FPing : byte;
  private
    FSocket : TPushServer;
    procedure on_Connected(AConnection:TConnection);
    procedure on_Received(AConnection:TConnection; AData:pointer; ASize:integer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSocket.Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FPing := 0;
  
  FSocket := TPushServer.Create(Self);
  FSocket.Port := _ServerPort;
  FSocket.OnConnected := on_Connected;
  FSocket.OnReceived := on_Received;
  FSocket.Start;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSocket);
end;

procedure TfmMain.on_Connected(AConnection: TConnection);
begin
  AConnection.Logined := true;
end;

procedure TfmMain.on_Received(AConnection: TConnection; AData: pointer;
  ASize: integer);
var
  pPacketFind : ^byte;
begin
  pPacketFind := AData;

  if pPacketFind^ = _Ping then begin
//    if ASize > 1 then raise Exception.Create(IntToStr(ASize));
    AConnection.Add(@FPing, SizeOf(FPing));
  end else begin
    FSocket.SendToOther(AConnection, AData, ASize);
  end;
end;

end.
