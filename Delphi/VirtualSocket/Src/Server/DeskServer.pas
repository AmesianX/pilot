unit DeskServer;

interface

uses
  VirtualSocketUtils, VirtualSocketServer,
  Classes, SysUtils;

type
  TDeskServer = class (TComponent)
  private
    FSocket : TVirtualSocketServer;

    procedure on_Connected(Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_BeforeDisconnected(Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_AfterDisconnected(Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_Received(Sender:TObject; AConnection:TConnection; AChannelInfo:TChannelInfo; AData:pointer; ASize:integer);
  public
    constructor Create(AOwner:TComponent; ASocket:TVirtualSocketServer); reintroduce;
    destructor Destroy; override;
  end;  

implementation

{ TDeskServer }

constructor TDeskServer.Create(AOwner: TComponent; ASocket: TVirtualSocketServer);
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnBeforeDisconnected := on_BeforeDisconnected;
  FSocket.OnAfterDisconnected := on_AfterDisconnected;
  FSocket.OnReceived := on_Received;
end;

destructor TDeskServer.Destroy;
begin

  inherited;
end;

procedure TDeskServer.on_AfterDisconnected(Sender: TObject;
  AConnection: TConnection; var AChannelInfo: TChannelInfo);
begin
  FreeAndNil(AChannelInfo);
end;

procedure TDeskServer.on_BeforeDisconnected(Sender: TObject;
  AConnection: TConnection; var AChannelInfo: TChannelInfo);
begin

end;

procedure TDeskServer.on_Connected(Sender: TObject; AConnection: TConnection;
  var AChannelInfo: TChannelInfo);
begin
  AChannelInfo := TChannelInfo.Create(AConnection, FSocket.Channel);
end;

procedure TDeskServer.on_Received(Sender: TObject; AConnection: TConnection;
  AChannelInfo: TChannelInfo; AData: pointer; ASize: integer);
begin

end;

end.

