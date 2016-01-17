unit DeskClient;

interface

uses
  VirtualSocketUtils, VirtualSocketClient,
  Classes, SysUtils;

type
  TDeskClient = class (TComponent)
  private
    FSocket : TVirtualSocketClient;

    procedure on_Connected(Sender:TObject);
    procedure on_Disconnected(Sender:TObject);
    procedure on_Received(Sender:TObject; AData:pointer; ASize:integer);
  public
    constructor Create(AOwner:TComponent; ASocket:TVirtualSocketClient); reintroduce;
    destructor Destroy; override;
  end;
  
implementation

{ TDeskClient }

constructor TDeskClient.Create(AOwner: TComponent;
  ASocket: TVirtualSocketClient);
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnDisconnected := on_Disconnected;
  FSocket.OnReceived := on_Received;
end;

destructor TDeskClient.Destroy;
begin

  inherited;
end;

procedure TDeskClient.on_Connected(Sender: TObject);
begin

end;

procedure TDeskClient.on_Disconnected(Sender: TObject);
begin

end;

procedure TDeskClient.on_Received(Sender: TObject; AData: pointer;
  ASize: integer);
begin

end;

end.
