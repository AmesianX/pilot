unit Base;

interface

uses
  ValueList,
  Classes, SysUtils, IdTCPServer;

type
  TVirtualSocketClientProvider = class abstract (TComponent)
  private
    FList : TList;
    procedure do_RemoveSockets;
  strict protected
    procedure on_Connected(AChannel:byte); virtual; abstract;
    procedure on_Disconnected(AChannel:byte); virtual; abstract;
    procedure on_Received(AChannel:byte; AData:pointer; ASize:integer); virtual; abstract;
  public
    Port: integer;
    Host: string;
    Connected: boolean;
    PacketRateOut: integer;
    PacketRateIn: integer;
    Interval: integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateSocket(InstanceClass:TComponentClass; var Reference);

    function Connect:boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;

    procedure Send(AData:pointer; ASize:integer); virtual; abstract;
  end;

  TVirtualSocketClient = class (TComponent)
  private
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnReceived: TNotifyEvent;
    FProvider: TVirtualSocketClientProvider;
  public
    Channel : byte;
    PacketRateOut: integer;
    PacketRateIn: integer;
  published
    property Provider : TVirtualSocketClientProvider read FProvider;
    property OnConnected : TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected : TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnReceived : TNotifyEvent read FOnReceived write FOnReceived;
  end;

  TChannelInfo = class abstract
  private
  public
    procedure Clear; virtual; abstract;
    procedure Add(AData:pointer; ASize:integer); virtual; abstract;
    procedure SendNow; virtual; abstract;
  end;

  TConnection = class
  private
  public
    Logined : boolean;
    UserID : string;
    Password : string;
    UserName : string;
    PeerThread : TIdPeerThread;
    ChannelInfo : TChannelInfo;

    procedure SendNow;

    procedure ClearIdleCount;
    function CheckIdle:boolean;

    procedure Disconnect;
  end;

  TVirtualSocketServerProvider = class abstract (TComponent)
  private
    FList : TList;
    procedure do_RemoveSockets;
  strict protected
    procedure on_Connected(AChannel:byte; AConnection:TConnection); virtual; abstract;
    procedure on_Disconnected(AChannel:byte; AConnection:TConnection); virtual; abstract;
    procedure on_Received(AChannel:byte; AConnection:TConnection; AData:pointer; ASize:integer); virtual; abstract;
  public
    Port: integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateSocket(InstanceClass:TComponentClass; var Reference);

    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;

    function LockList:TList; virtual; abstract;
    procedure UnlockList; virtual; abstract;

    function FindConnectionByUserID(AList:TList; AUserID:string):TConnection; virtual; abstract;

    procedure SendTo(AConnection:TConnection; AData:pointer; ASize:integer); overload; virtual; abstract;
    procedure SendToUserID(AUserID:string; AData:pointer; ASize:integer); overload; virtual; abstract;
    procedure SendToAll(AData:Pointer; ASize:integer); overload; virtual; abstract;
    procedure SendToOther(AConnection:TConnection; AData:Pointer; ASize:integer); overload; virtual; abstract;

    procedure SendTo(AConnection:TConnection; APacket:TValueList); overload;
    procedure SendToUserID(AUserID:string; APacket:TValueList); overload;
    procedure SendToAll(APacket:TValueList); overload;
    procedure SendToOther(AConnection:TConnection; APacket:TValueList); overload;
  end;

  TVirtualSocketServer = class(TComponent)
  private
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnReceived: TNotifyEvent;
    FProvider: TVirtualSocketServerProvider;
  public
    Channel : byte;
  published
    property Provider : TVirtualSocketServerProvider read FProvider;
    property OnConnected : TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected : TNotifyEvent read FOnDisconnected write FOnDisconnected;  
    property OnReceived : TNotifyEvent read FOnReceived write FOnReceived;  
  end;

implementation

{ TVirtualSocketClientProvider }

constructor TVirtualSocketClientProvider.Create(AOwner: TComponent);
begin
  inherited;

  FList := TList.Create;
end;

procedure TVirtualSocketClientProvider.CreateSocket(
  InstanceClass: TComponentClass; var Reference);
var
  Instance : TVirtualSocketClient;
begin
  Instance := TVirtualSocketClient(InstanceClass.NewInstance);
  TVirtualSocketClient(Reference) := Instance;
  TVirtualSocketClient(Reference).FProvider := Self;
  FList.Add(Instance);
end;

destructor TVirtualSocketClientProvider.Destroy;
begin
  Disconnect;
  do_RemoveSockets;

  // Todo : Socket 들을 먼저 제거하지 않도록 방어
  FList.Free;

  inherited;
end;

procedure TVirtualSocketClientProvider.do_RemoveSockets;
begin

end;

{ TVirtualSocketClient }

constructor TVirtualSocketClient.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TVirtualSocketClient.Destroy;
begin

  inherited;
end;

{ TConnection }

function TConnection.CheckIdle: boolean;
begin

end;

procedure TConnection.ClearIdleCount;
begin

end;

procedure TConnection.Disconnect;
begin

end;

procedure TConnection.SendNow;
begin

end;

{ TVirtualSocketServerProvider }

constructor TVirtualSocketServerProvider.Create(AOwner: TComponent);
begin
  inherited;

  FList := TList.Create;
end;

procedure TVirtualSocketServerProvider.CreateSocket(
  InstanceClass: TComponentClass; var Reference);
var
  Instance : TVirtualSocketServer;
begin
  Instance := TVirtualSocketServer(InstanceClass.NewInstance);
  TVirtualSocketServer(Reference) := Instance;
  TVirtualSocketServer(Reference).FProvider := Self;
  FList.Add(Instance);
end;

destructor TVirtualSocketServerProvider.Destroy;
begin
  Stop;
  do_RemoveSockets;
  
  FList.Free;

  inherited;
end;

procedure TVirtualSocketServerProvider.do_RemoveSockets;
begin

end;

procedure TVirtualSocketServerProvider.SendTo(AConnection: TConnection;
  APacket: TValueList);
begin

end;

procedure TVirtualSocketServerProvider.SendToAll(APacket: TValueList);
begin

end;

procedure TVirtualSocketServerProvider.SendToOther(AConnection: TConnection;
  APacket: TValueList);
begin

end;

procedure TVirtualSocketServerProvider.SendToUserID(AUserID: string;
  APacket: TValueList);
begin

end;

{ TVirtualSocketServer }

constructor TVirtualSocketServer.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TVirtualSocketServer.Destroy;
begin

  inherited;
end;

end.

