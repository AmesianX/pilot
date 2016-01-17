unit VirtualSocketServer;

interface

uses
  VirtualSocketUtils, ValueList, PacketBuffer, SpinLock,
  Windows, Classes, SysUtils, IdTCPServer;

type
  TConnection = class;
  TVirtualSocketServerProvider = class;
  TVirtualSocketServer = class;

  TChannelInfo = class
  private
    FBuffer : TPacketBuffer;
  private
    FConnection : TConnection;
    FChannel : byte;
  protected
     procedure do_Send(AData:pointer; ASize:integer);
  public
    constructor Create(AConnection:TConnection; AChannel:byte); reintroduce; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure Add(AData:pointer; ASize:integer); virtual;
    procedure SendNow; virtual;

    property Connection : TConnection read FConnection;
    property Channel : byte read FChannel;
  end;

  TConnection = class
  private
    FChannelInfos : array [0..64-1] of TChannelInfo;
    FTickCount : cardinal;
    FCSDisconnect : TSpinLock;
    procedure do_RemoveChannelInfos;
  private
    FLines : TValueList;
    function GetUserID: string;
    function GetUserName: string;
    procedure SetUserID(const Value: string);
    procedure SetUserName(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    function GetTChannelInfo(AIndex: integer): TChannelInfo;
  public
    Logined : boolean;

    // Todo : Indy와의 의존성 문제가 남은 부분
    // 아직은 제거 필요성이 없기 때문에 쓸 때없이 복잡도를 높이고 싶지 않아서 방치함
    PeerThread : TIdPeerThread;

    constructor Create;
    destructor Destroy; override;

    procedure ClearIdleCount;
    function CheckIdle:boolean;

    procedure Send(AChannel:byte; AData:pointer; ASize:integer);
    procedure Disconnect;

    property ChannelInfos[AIndex:integer] : TChannelInfo read GetTChannelInfo;
    property UserID : string read GetUserID write SetUserID;
    property Password : string read GetPassword write SetPassword;
    property UserName : string read GetUserName write SetUserName;
    property Lines : TValueList read FLines;
  end;

  TVirtualSocketServerProvider = class abstract (TComponent)
  private
    procedure do_RemoveSockets;
  protected
    FSocketList : TList;

    procedure on_Connected(AConnection:TConnection);
    procedure on_Disconnected(AConnection:TConnection);
    procedure on_Received(AChannel:byte; AConnection:TConnection; AData:pointer; ASize:integer);

    function do_LockList:TList; virtual; abstract;
    procedure do_UnlockList; virtual; abstract;
    function do_FindConnectionByUserID(AList:TList; AUserID:string):TConnection; virtual; abstract;

    // 채널별로 다뤄야하는 넘들
    procedure do_SendTo(AChannel:byte; AConnection:TConnection; AData:pointer; ASize:integer);
    procedure do_SendToUserID(AChannel:byte; AUserID:string; AData:pointer; ASize:integer);
    procedure do_SendToAll(AChannel:byte; AData:Pointer; ASize:integer);
    procedure do_SendToOther(AChannel:byte; AConnection:TConnection; AData:Pointer; ASize:integer);

    function GetPort: integer; virtual; abstract;
    procedure SetPort(const Value: integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;

    function CreateSocket:TVirtualSocketServer;
  published
    property Port : integer read GetPort write SetPort;
  end;

  TVirtualSocketServerEvent = procedure (Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo) of object;
  TVirtualSocketServerReceivedEvent = procedure (Sender:TObject; AConnection:TConnection; AChannelInfo:TChannelInfo; AData:pointer; ASize:integer) of object;

  TVirtualSocketServer = class(TComponent)
  private
    constructor Create(AProvider:TVirtualSocketServerProvider); reintroduce; virtual;
    destructor Destroy; override;
  private
    FProvider: TVirtualSocketServerProvider;
    FChannel: byte;
    FOnConnected: TVirtualSocketServerEvent;
    FOnAfterDisconnected: TVirtualSocketServerEvent;
    FOnBeforeDisconnected: TVirtualSocketServerEvent;
    FOnReceived: TVirtualSocketServerReceivedEvent;
  public
    function LockList:TList;
    procedure UnlockList;

    function FindConnectionByUserID(AList:TList; AUserID:string):TConnection;

    procedure SendTo(AConnection:TConnection; AData:pointer; ASize:integer); overload;
    procedure SendToUserID(AUserID:string; AData:pointer; ASize:integer); overload;
    procedure SendToAll(AData:Pointer; ASize:integer); overload;
    procedure SendToOther(AConnection:TConnection; AData:Pointer; ASize:integer); overload;

    procedure SendTo(AConnection:TConnection; APacket:TValueList); overload;
    procedure SendToUserID(AUserID:string; APacket:TValueList); overload;
    procedure SendToAll(APacket:TValueList); overload;
    procedure SendToOther(AConnection:TConnection; APacket:TValueList); overload;
  published
    property Provider : TVirtualSocketServerProvider read FProvider;
    property Channel : byte read FChannel;
    property OnConnected : TVirtualSocketServerEvent read FOnConnected write FOnConnected;
    property OnBeforeDisconnected : TVirtualSocketServerEvent read FOnBeforeDisconnected write FOnBeforeDisconnected;  
    property OnAfterDisconnected : TVirtualSocketServerEvent read FOnAfterDisconnected write FOnAfterDisconnected;  
    property OnReceived : TVirtualSocketServerReceivedEvent read FOnReceived write FOnReceived;
  end;

implementation

{ TChannelInfo }

procedure TChannelInfo.Add(AData: pointer; ASize: integer);
begin
  FBuffer.Add(AData, ASize);
end;

procedure TChannelInfo.Clear;
begin
  FBuffer.Clear;
end;

constructor TChannelInfo.Create(AConnection:TConnection; AChannel:byte);
begin
  inherited Create;

  FConnection := AConnection;
  FChannel := AChannel;

  FBuffer := TPacketBuffer.Create;
end;

destructor TChannelInfo.Destroy;
begin
  Clear;
  FBuffer.Free;

  inherited;
end;

procedure TChannelInfo.do_Send(AData: pointer; ASize: integer);
begin
  Connection.Send(Channel, AData, ASize);
end;

procedure TChannelInfo.SendNow;
var
  Data : pointer;
  Size : integer;
begin
  while FBuffer.GetPacket(Data, Size) do begin
    try
      do_Send(Data, Size);
    finally
      if Data <> nil then FreeMem(Data);
    end;
  end;
end;

{ TConnection }

function TConnection.CheckIdle: boolean;
var
  iTick : cardinal;
begin
  Result := false;

  iTick := GetTickCount;
  if iTick > FTickCount then
    Result := (iTick-FTickCount) >= _IdleCountLimit;
end;

procedure TConnection.ClearIdleCount;
begin
  FTickCount := GetTickCount;
end;

constructor TConnection.Create;
var
  Loop : Integer;
begin
  inherited;

  Logined := false;
  FTickCount := GetTickCount;

  for Loop := Low(FChannelInfos) to High(FChannelInfos) do FChannelInfos[Loop] := nil;

  FLines := TValueList.Create;
  FCSDisconnect := TSpinLock.Create;
end;

destructor TConnection.Destroy;
begin
  do_RemoveChannelInfos;

  FLines.Free;
  FCSDisconnect.Free;

  inherited;
end;

procedure TConnection.Disconnect;
begin
  FCSDisconnect.Enter;
  try
    TIdPeerThread(PeerThread).Connection.Disconnect;
    TIdPeerThread(PeerThread).Terminate;
  finally
    FCSDisconnect.Leave;
  end;
end;

procedure TConnection.do_RemoveChannelInfos;
var
  Loop : Integer;
begin
  for Loop := Low(FChannelInfos) to High(FChannelInfos) do begin
    if FChannelInfos[Loop] <> nil then FreeAndNil(FChannelInfos[Loop]);
  end;
end;

procedure TConnection.Send(AChannel: byte; AData: pointer; ASize: integer);
begin
  PeerThread.Connection.WriteBuffer(AChannel, SizeOf(AChannel));
  PeerThread.Connection.WriteBuffer(ASize, SizeOf(ASize));
  if ASize > 0 then PeerThread.Connection.WriteBuffer(AData^, ASize);
end;

function TConnection.GetPassword: string;
begin
  Result := FLines.Values['Password'];
end;

function TConnection.GetTChannelInfo(AIndex: integer): TChannelInfo;
begin
  Result := FChannelInfos[AIndex];
end;

function TConnection.GetUserID: string;
begin
  Result := FLines.Values['UserID'];
end;

function TConnection.GetUserName: string;
begin
  Result := FLines.Values['UserName'];
end;

procedure TConnection.SetPassword(const Value: string);
begin
  FLines.Values['Password'] := Value;
end;

procedure TConnection.SetUserID(const Value: string);
begin
  FLines.Values['UserID'] := Value;
end;

procedure TConnection.SetUserName(const Value: string);
begin
  FLines.Values['UserName'] := Value;
end;

{ TVirtualSocketServerProvider }

constructor TVirtualSocketServerProvider.Create(AOwner: TComponent);
begin
  inherited;

  FSocketList := TList.Create;
end;

function TVirtualSocketServerProvider.CreateSocket: TVirtualSocketServer;
begin
  Result := TVirtualSocketServer.Create(Self);
  Result.FChannel := FSocketList.Count;
  FSocketList.Add(Result);
end;

destructor TVirtualSocketServerProvider.Destroy;
begin
  Stop;
  do_RemoveSockets;
  
  FSocketList.Free;

  inherited;
end;

procedure TVirtualSocketServerProvider.do_RemoveSockets;
var
  Loop: Integer;
  VirtualSocketServer : TVirtualSocketServer;
begin
  for Loop := 0 to FSocketList.Count - 1 do begin
    VirtualSocketServer := Pointer(FSocketList[Loop]);
    VirtualSocketServer.Free;
  end;
  FSocketList.Clear;
end;

procedure TVirtualSocketServerProvider.do_SendTo(AChannel: byte;
  AConnection: TConnection; AData: pointer; ASize: integer);
begin
  if AConnection.FChannelInfos[AChannel] <> nil then
    AConnection.FChannelInfos[AChannel].Add(AData, ASize);
end;

procedure TVirtualSocketServerProvider.do_SendToAll(AChannel: byte;
  AData: Pointer; ASize: integer);
var
  List : TList;
  Loop : integer;
  Connection : TConnection;
begin
  List := do_LockList;
  try
    for Loop := 0 to List.Count - 1 do begin
      Connection := Pointer(TIdPeerThread(List[Loop]).Data);
      if Connection = nil then Continue;
      if Connection.Logined = false then Continue;

      if Connection.CheckIdle then Connection.Disconnect
      else do_SendTo(AChannel, Connection, AData, ASize);
    end;
  finally
    do_UnlockList;
  end;
end;

procedure TVirtualSocketServerProvider.do_SendToOther(AChannel: byte;
  AConnection: TConnection; AData: Pointer; ASize: integer);
var
  List : TList;
  Loop : integer;
  Connection : TConnection;
begin
  List := do_LockList;
  try
    for Loop := 0 to List.Count - 1 do begin
      Connection := Pointer(TIdPeerThread(List[Loop]).Data);
      if Connection = nil then Continue;
      if Connection = AConnection then Continue;
      if Connection.Logined = false then Continue;

      if Connection.CheckIdle then Connection.Disconnect
      else do_SendTo(AChannel, Connection, AData, ASize);
    end;
  finally
    do_UnlockList;
  end;
end;

procedure TVirtualSocketServerProvider.do_SendToUserID(AChannel: byte;
  AUserID: string; AData: pointer; ASize: integer);
var
  List : TList;
  Loop : integer;
  Connection : TConnection;
begin
  List := do_LockList;
  try
    for Loop := 0 to List.Count - 1 do begin
      Connection := Pointer(TIdPeerThread(List[Loop]).Data);
      if Connection = nil then Continue;
      if Pos('<'+UpperCase(Connection.UserID)+'>', UpperCase(AUserID)) = 0 then Continue;
      if Connection.Logined = false then Continue;

      if Connection.CheckIdle then Connection.Disconnect
      else do_SendTo(AChannel, Connection, AData, ASize);
    end;
  finally
    do_UnlockList;
  end;
end;

procedure TVirtualSocketServerProvider.on_Connected(AConnection: TConnection);
var
  Loop : Integer;
  Socket : TVirtualSocketServer;
begin
  for Loop := 0 to FSocketList.Count - 1 do begin
    Socket := Pointer(FSocketList[Loop]);
    if Assigned(Socket.FOnConnected) then
      Socket.FOnConnected(Socket, AConnection, AConnection.FChannelInfos[Socket.Channel]);
  end;
end;

procedure TVirtualSocketServerProvider.on_Disconnected(
  AConnection: TConnection);
var
  Loop : Integer;
  Socket : TVirtualSocketServer;
begin
  for Loop := 0 to FSocketList.Count - 1 do begin
    Socket := TVirtualSocketServer(FSocketList[Loop]);

    if Assigned(Socket.FOnBeforeDisconnected) then
      Socket.FOnBeforeDisconnected(Socket, AConnection, AConnection.FChannelInfos[Socket.Channel]);

    AConnection.PeerThread.Data := nil;
    AConnection.Logined := false;

    if Assigned(Socket.FOnAfterDisconnected) then
      Socket.FOnAfterDisconnected(Socket, AConnection, AConnection.FChannelInfos[Socket.Channel]);
  end;
end;

procedure TVirtualSocketServerProvider.on_Received(AChannel: byte;
  AConnection: TConnection; AData: pointer; ASize: integer);
var
  Socket : TVirtualSocketServer;
begin
  Socket := TVirtualSocketServer(FSocketList[AChannel]);
  if Assigned(Socket.FOnReceived) then
    Socket.FOnReceived(Socket, AConnection, AConnection.ChannelInfos[Socket.Channel], AData, ASize);
end;

{ TVirtualSocketServer }

constructor TVirtualSocketServer.Create(AProvider: TVirtualSocketServerProvider);
begin
  inherited Create(AProvider);

  FProvider := AProvider;
end;

destructor TVirtualSocketServer.Destroy;
begin

  inherited;
end;

function TVirtualSocketServer.FindConnectionByUserID(AList: TList;
  AUserID: string): TConnection;
begin
  Result := Provider.do_FindConnectionByUserID(AList, AUserID);
end;

function TVirtualSocketServer.LockList: TList;
begin
  Result := Provider.do_LockList;
end;

procedure TVirtualSocketServer.SendTo(AConnection: TConnection;
  APacket: TValueList);
var
  Data : pointer;
  Size : integer;
begin
  APacket.SaveToBuffer(Data, Size);
  try
    SendTo(AConnection, Data, Size);
  finally
    if Data <> nil then FreeMem(Data);
  end;
end;

procedure TVirtualSocketServer.SendTo(AConnection: TConnection; AData: pointer;
  ASize: integer);
begin
  Provider.do_SendTo(Channel, AConnection, AData, ASize);
end;

procedure TVirtualSocketServer.SendToAll(APacket: TValueList);
var
  Data : pointer;
  Size : integer;
begin
  APacket.SaveToBuffer(Data, Size);
  try
    SendToAll(Data, Size);
  finally
    if Data <> nil then FreeMem(Data);
  end;
end;

procedure TVirtualSocketServer.SendToAll(AData: Pointer; ASize: integer);
begin
  Provider.do_SendToAll(Channel, AData, ASize);
end;

procedure TVirtualSocketServer.SendToOther(AConnection: TConnection;
  AData: Pointer; ASize: integer);
begin
  Provider.do_SendToOther(Channel, AConnection, AData, ASize);
end;

procedure TVirtualSocketServer.SendToOther(AConnection: TConnection;
  APacket: TValueList);
var
  Data : pointer;
  Size : integer;
begin
  APacket.SaveToBuffer(Data, Size);
  try
    SendToOther(AConnection, Data, Size);
  finally
    if Data <> nil then FreeMem(Data);
  end;
end;

procedure TVirtualSocketServer.SendToUserID(AUserID: string; AData: pointer;
  ASize: integer);
begin
  Provider.do_SendToUserID(Channel, AUserID, AData, ASize);
end;

procedure TVirtualSocketServer.SendToUserID(AUserID: string;
  APacket: TValueList);
var
  Data : pointer;
  Size : integer;
begin
  APacket.SaveToBuffer(Data, Size);
  try
    SendToUserID(AUserID, Data, Size);
  finally
    if Data <> nil then FreeMem(Data);
  end;
end;

procedure TVirtualSocketServer.UnlockList;
begin
  Provider.do_UnlockList;
end;

end.

