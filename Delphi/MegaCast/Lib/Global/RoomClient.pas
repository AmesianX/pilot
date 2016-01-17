unit RoomClient;

interface

uses
  MegaCastUtils, VirtualSocketClient, VirtualSocketClientProviderIndy9,
  Classes, SysUtils;

type
  TRoomClient = class(TComponent)
  private
    FClientProvider: TVirtualSocketClientProvider;
  private
    function GetHost: string;
    function GetRoomNo: integer;
    procedure SetHost(const Value: string);
    procedure SetRoomNo(const Value: integer);
    function GetConnected: boolean;
    function GetOnConnected: TNotifyEvent;
    function GetOnDisconnected: TNotifyEvent;
    procedure SetOnConnected(const Value: TNotifyEvent);
    procedure SetOnDisconnected(const Value: TNotifyEvent);
  protected
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    function CreateSocket:TVirtualSocketClient;

    function Connect:boolean;
    procedure Disconnect;
  published
    property Connected : boolean read GetConnected;
    property Host : string read GetHost write SetHost;
    property RoomNo : integer read GetRoomNo write SetRoomNo;
    property OnConnected : TNotifyEvent read GetOnConnected write SetOnConnected;
    property OnDisconnected : TNotifyEvent read GetOnDisconnected write SetOnDisconnected;
  end;

implementation

{ TRoomClient }

function TRoomClient.Connect: boolean;
begin
  Result := FClientProvider.Connect;
end;

constructor TRoomClient.Create(AOwner: TComponent);
begin
  inherited;

  FClientProvider := TVirtualSocketClientProviderIndy9.Create(Self);
end;

function TRoomClient.CreateSocket: TVirtualSocketClient;
begin
  Result := FClientProvider.CreateSocket;
end;

destructor TRoomClient.Destroy;
begin
  FreeAndNil(FClientProvider);

  inherited;
end;

procedure TRoomClient.Disconnect;
begin
  FClientProvider.Disconnect;
end;

function TRoomClient.GetConnected: boolean;
begin
  Result := FClientProvider.Connected;
end;

function TRoomClient.GetHost: string;
begin
  Result := FClientProvider.Host;
end;

function TRoomClient.GetOnConnected: TNotifyEvent;
begin
  Result := FClientProvider.OnConnected;
end;

function TRoomClient.GetOnDisconnected: TNotifyEvent;
begin
  Result := FClientProvider.OnDisconnected;
end;

function TRoomClient.GetRoomNo: integer;
begin
  Result := FClientProvider.Port;
end;

procedure TRoomClient.SetHost(const Value: string);
begin
  FClientProvider.Host := Value;
end;

procedure TRoomClient.SetOnConnected(const Value: TNotifyEvent);
begin
  FClientProvider.OnConnected := Value;
end;

procedure TRoomClient.SetOnDisconnected(const Value: TNotifyEvent);
begin
  FClientProvider.OnDisconnected := Value;
end;

procedure TRoomClient.SetRoomNo(const Value: integer);
begin
  FClientProvider.Port := Value + _MagaCastPortOffset;
end;

end.
