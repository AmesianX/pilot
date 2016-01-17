unit VirtualSocketClient;

interface

uses
  VirtualSocketUtils, HandleComponent, ValueList, PacketBuffer, SpinLock,
  Windows, Classes, SysUtils, IdTCPServer;

type
  TVirtualSocketClient = class;
  
  TVirtualSocketClientProvider = class abstract (THandleComponent)
  private
    procedure do_RemoveSockets;
  private
    FPacketRateOut: integer;
    FPacketRateIn: integer;
    FInterval: integer;
  protected
    FSocketList : TList;
    procedure on_Connected;
    procedure on_Disconnected;
    procedure on_Received(AChannel:byte; AData:pointer; ASize:integer);

    procedure do_Send(AChannel:byte; AData:pointer; ASize:integer); virtual; abstract;

    function GetConnected: boolean; virtual; abstract;
    function GetHost: string; virtual; abstract;
    function GetPort: integer; virtual; abstract;
    procedure SetHost(const Value: string); virtual; abstract;
    procedure SetPort(const Value: integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateSocket:TVirtualSocketClient;

    function Connect:boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
  published
    property Host : string read GetHost write SetHost;
    property Port : integer read GetPort write SetPort;
    property Connected : boolean read GetConnected;
    property Interval : integer read FInterval write FInterval;
    property PacketRateOut : integer read FPacketRateOut;
    property PacketRateIn : integer read FPacketRateIn;
  end;

  TVirtualSocketClientReceivedEvent = procedure (Sender:TObject; AData:pointer; ASize:integer) of object;

  TVirtualSocketClient = class (TComponent)
  private
    constructor Create(AProvider:TVirtualSocketClientProvider); reintroduce; virtual;
    destructor Destroy; override;
  private
    FProvider: TVirtualSocketClientProvider;
    FChannel : byte;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnReceived: TVirtualSocketClientReceivedEvent;
    FPacketRateOut: integer;
    FPacketRateIn: byte;
  public
    procedure Send(AData:pointer; ASize:integer); overload;
    procedure Send(AText:string); overload;
  published
    property Provider : TVirtualSocketClientProvider read FProvider;
    property Channel : byte read FChannel;
    property PacketRateOut : integer read FPacketRateOut;
    property PacketRateIn : byte read FPacketRateIn;
    property OnConnected : TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected : TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnReceived : TVirtualSocketClientReceivedEvent read FOnReceived write FOnReceived;
  end;

implementation

{ TVirtualSocketClientProvider }

constructor TVirtualSocketClientProvider.Create(AOwner: TComponent);
begin
  inherited;

  FSocketList := TList.Create;
end;

function TVirtualSocketClientProvider.CreateSocket: TVirtualSocketClient;
begin
  Result := TVirtualSocketClient.Create(Self);
  Result.FChannel := FSocketList.Count;
  FSocketList.Add(Result);
end;

destructor TVirtualSocketClientProvider.Destroy;
begin
  Disconnect;
  do_RemoveSockets;
  
  FSocketList.Free;

  inherited;
end;

procedure TVirtualSocketClientProvider.do_RemoveSockets;
var
  Loop: Integer;
  Socket : TVirtualSocketClient;
begin
  for Loop := 0 to FSocketList.Count - 1 do begin
    Socket := Pointer(FSocketList[Loop]);
    Socket.Free;
  end;
  FSocketList.Clear;
end;

procedure TVirtualSocketClientProvider.on_Connected;
var
  Loop : Integer;
  Socket : TVirtualSocketClient;
begin
  for Loop := 0 to FSocketList.Count - 1 do begin
    Socket := Pointer(FSocketList[Loop]);
    if Assigned(Socket.FOnConnected) then Socket.FOnConnected(Socket);
  end;
end;

procedure TVirtualSocketClientProvider.on_Disconnected;
var
  Loop : Integer;
  Socket : TVirtualSocketClient;
begin
  for Loop := 0 to FSocketList.Count - 1 do begin
    Socket := Pointer(FSocketList[Loop]);
    if Assigned(Socket.FOnDisconnected) then Socket.FOnDisconnected(Socket);
  end;
end;

procedure TVirtualSocketClientProvider.on_Received(AChannel: byte;
  AData: pointer; ASize: integer);
var
  Socket : TVirtualSocketClient;
begin
  Socket := Pointer(FSocketList[AChannel]);
  if Assigned(Socket.FOnReceived) then Socket.FOnReceived(Socket, AData, ASize);
end;

{ TVirtualSocketClient }

constructor TVirtualSocketClient.Create(
  AProvider: TVirtualSocketClientProvider);
begin
  inherited Create(AProvider);

  FProvider := AProvider;
end;

destructor TVirtualSocketClient.Destroy;
begin

  inherited;
end;

procedure TVirtualSocketClient.Send(AData: pointer; ASize: integer);
begin
  Provider.do_Send(Channel, AData, ASize);
end;

procedure TVirtualSocketClient.Send(AText: string);
var
  Data : pointer;
  ssData : TStringStream;
begin
  if AText = '' then Exit;

  ssData := TStringStream.Create('');
  try
    ssData.WriteString(AText);
    ssData.Position := 0;

    GetMem(Data, ssData.Size);
    try
      ssData.Read(Data^, ssData.Size);
      Send(Data, ssData.Size);
    finally
      FreeMem(Data);
    end;
  finally
    ssData.Free;
  end;
end;

end.
