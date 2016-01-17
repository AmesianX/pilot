unit DeskCamServer;

interface

uses
  VirtualSocketServer, FrameBuffer, DeskCamUtils, DeskCamAddonUtils,
  DeskFrameData, UniqueIDList,
  Windows, Classes, SysUtils, IdTCPServer;

type
  TChannelInfoForDeskCam = class (TChannelInfo)
  private
    FFrameBuffer: TFrameBuffer;
    FUniqueIDList: TUniqueIDList;
    FIsSender: Boolean;
    FConnectionID: Integer;
  public
    constructor Create(AConnection:TConnection; AChannel:byte); override;
    destructor Destroy; override;

    property FrameBuffer: TFrameBuffer read FFrameBuffer;
    property UniqueIDList: TUniqueIDList read FUniqueIDList;
    property IsSender: Boolean read FIsSender write FIsSender;
    property ConnectionID: Integer read FConnectionID write FConnectionID;
  end;

  TDeskCamServer = class (TComponent)
  private
    FSocket: TVirtualSocketServer;
    FSenderConnection: TConnection;
    FDeskFrameData: TDeskFrameData;
    FFrameSize: DWORD;

    procedure on_Connected(Sender: TObject; AConnection: TConnection; var AChannelInfo: TChannelInfo);
    procedure on_BeforeDisconnected(Sender: TObject; AConnection: TConnection; var AChannelInfo: TChannelInfo);
    procedure on_AfterDisconnected(Sender: TObject; AConnection: TConnection; var AChannelInfo: TChannelInfo);
    procedure on_Received(Sender: TObject; AConnection: TConnection; AChannelInfo: TChannelInfo; AData: pointer; ASize: integer);

    procedure AddFrameBufferToAll(const ADataType: TDataType;
      const AXIndex: Word; const AYIndex: Word; const AUniqueID: TUniqueID;
      const AData: Pointer; const ASize: Integer);
    procedure AddFrameBuffer(const AConnectionID: Integer; const ADataType: TDataType;
      const AXIndex: Word; const AYIndex: Word; const AUniqueID: TUniqueID;
      const AData: Pointer; const ADataSize: Integer);

    // Process
    procedure ProcessFrame(const AConnection: TConnection;
      const AChannelInfo: TChannelInfo; const AData: Pointer);
    procedure ProcessUserInfo(const AConnection: TConnection; const AData: Pointer);

    // Send
    procedure SendNeedFrameToSender;
    procedure SendDeskDataToReceiver(const AConnection: TConnection);
    procedure SendFrameToReceiver(const AConnection: TConnection;
      const ADataType: TDataType; const AXIndex: Word; const AYIndex: Word;
      const AData: Pointer; const ADataSize: Integer);
    procedure SendEndOfFrameToReceiver(const AConnection: TConnection);
    procedure SendConnectionID(const AConnection: TConnection; const AConnectionID: Integer);
    procedure SendAlreadySenderConnected(const AConnection: TConnection);
    procedure SendFrameSize(const AConnection: TConnection;
      const AFrameSize: DWORD);
    procedure SendDeskFrameData(const AConnection: TConnection);

    function FindConnectionByUserID(const AList: TList; const AConnectionID: Integer): TConnection;
    function SenderExists: Boolean;
    procedure NewFace(const AIsSender: Boolean; const AConnection: TConnection;
      const AChannelInfo : TChannelInfoForDeskCam);
  public
    constructor Create(AOwner: TComponent; ASocket: TVirtualSocketServer); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDeskCamServer }

procedure TDeskCamServer.AddFrameBufferToAll(const ADataType: TDataType;
  const AXIndex, AYIndex: Word; const AUniqueID: TUniqueID;
  const AData: Pointer; const ASize: Integer);
var
  List : TList;
  Connection : TConnection;
  ChannelInfo : TChannelInfoForDeskCam;
  i: Integer;
begin
  List := FSocket.LockList;
  try
    for i:=0 to List.Count-1 do
    begin
      Connection := Pointer(TIdPeerThread(List[i]).Data);
      if Connection = nil then Continue;

      ChannelInfo := TChannelInfoForDeskCam(Connection.ChannelInfos[FSocket.Channel]);
      if ChannelInfo = nil then Continue;
      if ChannelInfo.IsSender then Continue;

      ChannelInfo.FrameBuffer.Add(ADataType, AXIndex, AYIndex, AUniqueID, AData, ASize);
    end;
  finally
    FSocket.UnlockList;
  end;
end;

constructor TDeskCamServer.Create(AOwner: TComponent; ASocket: TVirtualSocketServer);
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnBeforeDisconnected := on_BeforeDisconnected;
  FSocket.OnAfterDisconnected := on_AfterDisconnected;
  FSocket.OnReceived := on_Received;

  FDeskFrameData := TDeskFrameData.Create;
end;

destructor TDeskCamServer.Destroy;
begin
  FDeskFrameData.Free;

  inherited;
end;

function TDeskCamServer.FindConnectionByUserID(const AList: TList; 
  const AConnectionID: Integer): TConnection;
var
  i: Integer;
  Connection: TConnection;
  ChannelInfo: TChannelInfoForDeskCam;
begin
  Result := nil;
  if AList.Count <= 0 then Exit;

  for i:=AList.Count-1 downto 0 do
  begin
    Connection := Pointer(TIdPeerThread(AList[i]).Data);
    if Connection = nil then Continue;

    ChannelInfo := TChannelInfoForDeskCam(Connection.ChannelInfos[FSocket.Channel]);
    if ChannelInfo = nil then Continue;

    if ChannelInfo.ConnectionID = AConnectionID then
    begin
      Result := Connection;    
      Break;
    end;
  end;
end;

procedure TDeskCamServer.NewFace(const AIsSender: Boolean;
  const AConnection: TConnection; const AChannelInfo : TChannelInfoForDeskCam);
begin
  if (not AIsSender) and (SenderExists) then
  begin
    SendFrameSize(AConnection, FFrameSize);
    SendDeskFrameData(AConnection);
  end;
end;

procedure TDeskCamServer.on_AfterDisconnected(Sender: TObject;
  AConnection: TConnection; var AChannelInfo: TChannelInfo);
begin
end;

procedure TDeskCamServer.on_BeforeDisconnected(Sender: TObject;
  AConnection: TConnection; var AChannelInfo: TChannelInfo);
begin
  if TChannelInfoForDeskCam(AChannelInfo).IsSender then
    FSenderConnection := nil;
end;

procedure TDeskCamServer.on_Connected(Sender: TObject; AConnection: TConnection;
  var AChannelInfo: TChannelInfo);
begin
  AChannelInfo := TChannelInfoForDeskCam.Create(AConnection, FSocket.Channel);
end;

procedure TDeskCamServer.on_Received(Sender: TObject; AConnection: TConnection;
  AChannelInfo: TChannelInfo; AData: pointer; ASize: integer);
var
  Data: PByte;
  PacketType: TPacketType;
begin
  Data := AData;

  PacketType := PPacketType(Data)^;
  Inc(Data, sizeof(PacketType));

  case PacketType of
    ptSendUserInfo:
    begin
      ProcessUserInfo(AConnection, Data);
    end;

    ptSendFrameToServer:
    begin
      ProcessFrame(AConnection, AChannelInfo, Data);
    end;

    ptSendEndOfFrameToServer:
    begin
      SendNeedFrameToSender;
    end;

    ptNeedFrameToServer:
    begin
      SendDeskDataToReceiver(AConnection);
    end;
  end;
end;

procedure TDeskCamServer.ProcessFrame(const AConnection: TConnection;
  const AChannelInfo: TChannelInfo; const AData: Pointer);
var
  Data: PByte;
  ConnectionID: Integer;
  DataType: TDataType;
  XIndex, YIndex: Word;
  UniqueID: TUniqueID;
  DataSize: Integer;
  FrameData: Pointer;
begin
  Data := AData;

  ConnectionID := PInteger(Data)^;
  Inc(Data, sizeof(ConnectionID));

  DataType := PDataType(Data)^;
  Inc(Data, sizeof(DataType));

  XIndex := PWord(Data)^;
  Inc(Data, sizeof(XIndex));

  YIndex := PWord(Data)^;
  Inc(Data, sizeof(YIndex));

  UniqueID := PInt64(Data)^;
  Inc(Data, sizeof(UniqueID));

  DataSize := PInteger(Data)^;
  Inc(Data, sizeof(DataSize));

  FrameData := nil;
  if DataSize > 0 then FrameData := Data;

  if DataType in [dtFrameSize] then
  begin
    FFrameSize := PDWORD(FrameData)^;
    FDeskFrameData.SetSize(FFrameSize);
  end
  else if DataType in [dtZLibBlock, dtJpegBlock] then
  begin
    FDeskFrameData.SetData(XIndex, YIndex, DataType, 0, FrameData, DataSize);
  end;

  if ConnectionID = 0 then
    AddFrameBufferToAll(DataType, XIndex, YIndex, UniqueID, FrameData, DataSize)
  else
    AddFrameBuffer(ConnectionID, DataType, XIndex, YIndex, UniqueID, FrameData, DataSize);
end;

procedure TDeskCamServer.ProcessUserInfo(const AConnection: TConnection;
  const AData: Pointer);
var
  Data: PByte;
  IsSender: Boolean;
  ChannelInfo: TChannelInfoForDeskCam;
begin
  Data := AData;
  IsSender := PBoolean(Data)^;

  ChannelInfo := TChannelInfoForDeskCam(AConnection.ChannelInfos[FSocket.Channel]);

  if IsSender then
  begin
    // 이미 강사가 있다면, 접속을 끊는다.
    if SenderExists then
    begin
      SendAlreadySenderConnected(AConnection);
      AConnection.Disconnect;
      Exit;
    end;

    FSenderConnection := AConnection;
  end;

  ChannelInfo.IsSender := IsSender;
  ChannelInfo.ConnectionID := Integer(AConnection);

  SendConnectionID(AConnection, ChannelInfo.ConnectionID);
  if IsSender then SendNeedFrameToSender;
  NewFace(IsSender, AConnection, ChannelInfo);
end;

procedure TDeskCamServer.SendAlreadySenderConnected(
  const AConnection: TConnection);
var
  PacketType: TPacketType;
begin
  PacketType := ptAlreadySenderConnected; 
  FSocket.SendTo(AConnection, @PacketType, sizeof(PacketType));
end;

procedure TDeskCamServer.SendConnectionID(const AConnection: TConnection;
  const AConnectionID: Integer);
var
  DeskStream: TMemoryStream;
  PacketType: TPacketType;
begin
  DeskStream := TMemoryStream.Create;
  try
    PacketType := ptSendConnectionID;
    DeskStream.Write(PacketType, sizeof(PacketType));

    DeskStream.Write(AConnectionID, sizeof(AConnectionID));

    FSocket.SendTo(AConnection, DeskStream.Memory, DeskStream.Size);
  finally
    DeskStream.Free;
  end;
end;

procedure TDeskCamServer.SendDeskDataToReceiver(const AConnection: TConnection);
var
  ChannelInfo : TChannelInfoForDeskCam;
  DataType: TDataType;
  XIndex, YIndex: Word;
  Data: Pointer;
  DataSize: Integer;
  UniqueID: TUniqueID;
begin
  ChannelInfo := TChannelInfoForDeskCam(AConnection.ChannelInfos[FSocket.Channel]);
  if ChannelInfo.IsSender then Exit;

  if ChannelInfo.FrameBuffer.Count <= 0 then
  begin
    SendEndOfFrameToReceiver(AConnection);
    Exit;
  end;

  while ChannelInfo.FrameBuffer.Get(DataType, XIndex, YIndex, UniqueID, Data, DataSize) do
  begin
    try
      // 키 보내기
      if ChannelInfo.UniqueIDList.IsExists(UniqueID) then
      begin
        if DataType = dtZLibBlock then DataType := dtZLibBlockKey
        else if DataType = dtJpegBlock then DataType := dtJpegBlockKey;

        SendFrameToReceiver(AConnection, DataType, XIndex, YIndex, @UniqueID, sizeof(UniqueID));
      end else
      // 실제 데이터 보내기
      begin
        SendFrameToReceiver(AConnection, DataType, XIndex, YIndex, Data, DataSize);
        ChannelInfo.UniqueIDList.Add(UniqueID);
      end;
    finally
      if Data <> nil then FreeMem(Data);
    end;
  end;

  SendEndOfFrameToReceiver(AConnection);
end;

procedure TDeskCamServer.SendDeskFrameData(const AConnection: TConnection);
var
  LoopX, LoopY: Integer;
  Block: TBlock;
begin
  for LoopY := 0 to FDeskFrameData.VerticalBlockCount-1 do
  begin
    for LoopX := 0 to FDeskFrameData.HorizonBlockCount -1 do
    begin
      Block := FDeskFrameData.Blocks[LoopX, LoopY];
      SendFrameToReceiver(AConnection, Block.DataType, Block.XIndex, Block.YIndex,
        Block.Data, Block.DataSize);
    end;
  end;
end;

procedure TDeskCamServer.SendEndOfFrameToReceiver(const AConnection: TConnection);
var
  PacketType: TPacketType;
begin
  PacketType := ptSendEndOfFrameToReceiver;
  FSocket.SendTo(AConnection, @PacketType, sizeof(PacketType));
end;

function TDeskCamServer.SenderExists: Boolean;
begin
  Result := FSenderConnection <> nil;
end;

procedure TDeskCamServer.SendFrameSize(const AConnection: TConnection;
  const AFrameSize: DWORD);
var
  DataType: TDataType;
begin
  DataType := dtFrameSize;
  SendFrameToReceiver(AConnection, DataType, 0, 0, @AFrameSize, sizeof(AFrameSize));
end;

procedure TDeskCamServer.SendFrameToReceiver(const AConnection: TConnection;
  const ADataType: TDataType; const AXIndex: Word; const AYIndex: Word;
  const AData: Pointer; const ADataSize: Integer);
var
  DeskStream: TMemoryStream;
  PacketType: TPacketType;
begin
  DeskStream := TMemoryStream.Create;
  try
    PacketType := ptSendFrameToReceiver;

    DeskStream.Write(PacketType, sizeof(PacketType));
    DeskStream.Write(ADataType, sizeof(ADataType));
    DeskStream.Write(AXIndex, sizeof(AXIndex));
    DeskStream.Write(AYIndex, sizeof(AYIndex));
    DeskStream.Write(ADataSize, sizeof(ADataSize));
    DeskStream.Write(AData^, ADataSize);

    FSocket.SendTo(AConnection, DeskStream.Memory, DeskStream.Size);
  finally
    DeskStream.Free;
  end;
end;

procedure TDeskCamServer.AddFrameBuffer(const AConnectionID: Integer;
  const ADataType: TDataType; const AXIndex, AYIndex: Word;
  const AUniqueID: TUniqueID; const AData: Pointer; const ADataSize: Integer);
var
  List: TList;
  Connection: TConnection;
  ChannelInfo: TChannelInfoForDeskCam;
begin
  List := FSocket.LockList;
  try
    Connection := FindConnectionByUserID(List, AConnectionID);
    if Connection = nil then Exit;

    ChannelInfo := TChannelInfoForDeskCam(Connection.ChannelInfos[FSocket.Channel]);
    ChannelInfo.FrameBuffer.Add(ADataType, AXIndex, AYIndex, AUniqueID, AData, ADataSize);
  finally
    FSocket.UnlockList;
  end;
end;

procedure TDeskCamServer.SendNeedFrameToSender;
var
  PacketType: TPacketType;
begin
  if not SenderExists then Exit;

  PacketType := ptNeedFrameToSender;
  FSocket.SendTo(FSenderConnection, @PacketType, sizeof(PacketType));
end;

{ TChannelInfoForDeskCam }

constructor TChannelInfoForDeskCam.Create(AConnection: TConnection;
  AChannel: byte);
begin
  inherited;

  FFrameBuffer := TFrameBuffer.Create;
  FUniqueIDList := TUniqueIDList.Create;
  FIsSender := False;
end;

destructor TChannelInfoForDeskCam.Destroy;
begin
  FUniqueIDList.Free;
  FFrameBuffer.Free;

  inherited;
end;

end.

