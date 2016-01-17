unit DeskCamSender;

interface

uses
  HandleComponent, VirtualSocketClient, DeskCamUtils, FrameGenerator, DeskEncoder,
  FrameBuffer, DeskCamAddonUtils, Sys, Capture,
  Windows, Messages, Classes, SysUtils, Graphics, SystemFolder;

type
  TDeskCamSender = class(THandleComponent)
  private
    FSocket: TVirtualSocketClient;
    FFrameGenerator: TFrameGenerator;
    FDeskEncoder: TDeskEncoder;
    FFrameBuffer: TFrameBuffer;
    FConnectionID: Integer;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FLog: TLog;
    FStarted: Boolean;

    procedure on_Connected(Sender: TObject);
    procedure on_Disconnected(Sender: TObject);
    procedure on_Received(Sender: TObject; AData: Pointer; ASize: Integer);
    procedure do_WMReceived(var Msg: TMessage); message WM_DESK_RECEIVED;
    procedure on_NewData(Sender: TObject; ADataType: TDataType;
      AXIndex: Word; AYIndex: Word; AData: Pointer; ASize: Integer);
    procedure on_DeskEncoderEncodeTime(const ATime: Cardinal);

    // Process
    procedure ProcessConnectionID(const AData: Pointer);

    // Send
    procedure Send(const AData: Pointer; const ASize: Integer);
    procedure SendUserInfo;
    procedure SendDeskDataToServer;
    procedure SendFrameBufferData(
      ADataType: TDataType; const AXIndex: Word; const AYIndex: Word; const ATickCount: Int64;
      const AData: Pointer; const ADataSize: Integer);
    procedure SendFrameToServer(const AConnectionID: Integer;
      const ADataType: TDataType; const AXIndex: Word; const AYIndex: Word;
      const AUniqueID: TUniqueID; const AData: Pointer; const ADataSize: Integer);
    procedure SendEndOfFrameToServer;

    procedure RemoveDeskCamReceived;
  private
    FPause: Boolean;
    function GetMonitorNo: Integer;
    procedure SetMonitorNo(const Value: Integer);
    function GetCaptureType: TCaptureType;
    procedure SetCaptureType(const Value: TCaptureType);
    function GetCaptureRect: TRect;
    procedure SetCaptureRect(const Value: TRect);
    procedure SetPause(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent; ASocket: TVirtualSocketClient); reintroduce;
    destructor Destroy; override;

    procedure Start(HWAcceleration: Boolean = False);
    procedure Stop;

    property Pause: Boolean read FPause write SetPause;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property ConnectionID: Integer read FConnectionID;
    property MonitorNo: Integer read GetMonitorNo write SetMonitorNo;
    property CaptureType: TCaptureType read GetCaptureType write SetCaptureType;
    property CaptureRect: TRect read GetCaptureRect write SetCaptureRect;
    property Log: TLog read FLog;
  end;

implementation

{ TDeskCamSender }

constructor TDeskCamSender.Create(AOwner: TComponent; ASocket: TVirtualSocketClient);
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnDisconnected := on_Disconnected;
  FSocket.OnReceived := on_Received;

  FFrameGenerator := TFrameGenerator.Create(Self);
  FFrameGenerator.MonitorNo := 0;
  FFrameGenerator.OnNewData := on_NewData;

  FDeskEncoder := TDeskEncoder.Create(Self);
  FDeskEncoder.OnEncodeTime := on_DeskEncoderEncodeTime;

  FFrameBuffer := TFrameBuffer.Create;

  FLog := TLog.Create;
end;

destructor TDeskCamSender.Destroy;
begin
  Stop;

  FSocket.OnReceived := nil;
  FFrameGenerator.Free;
  FDeskEncoder.Free;
  FFrameBuffer.Free;
  FLog.Free;

  RemoveDeskCamReceived;

  inherited Destroy;
end;

procedure TDeskCamSender.do_WMReceived(var Msg: TMessage);
var
  DeskStream: TMemoryStream;
  PacketType: TPacketType;
  Data: PByte;
begin
  DeskStream := TMemoryStream(Msg.WParam);
  try
    Inc(FLog.PacketRecvCount);
    Inc(FLog.PacketRecvAmount, DeskStream.Size);

    Data := DeskStream.Memory;

    PacketType := PPacketType(Data)^;
    Inc(Data, sizeof(PacketType));

    case PacketType of
      ptAlreadySenderConnected:
      begin
      end;

      ptSendConnectionID:
      begin
        ProcessConnectionID(Data);
      end;

      ptNeedFrameToSender:
      begin
        Sleep(200);
        SendDeskDataToServer;
      end;
    end;
  finally
    DeskStream.Free;
  end;
end;

function TDeskCamSender.GetCaptureType: TCaptureType;
begin
  Result := FFrameGenerator.CaptureType;
end;

function TDeskCamSender.GetMonitorNo: Integer;
begin
  Result := FFrameGenerator.MonitorNo;
end;

function TDeskCamSender.GetCaptureRect: TRect;
begin
  Result := FFrameGenerator.CaptureRect;
end;

procedure TDeskCamSender.on_Connected(Sender: TObject);
begin
  if Assigned(FOnConnected) then
    FOnConnected(Sender);

  SendUserInfo;
end;

procedure TDeskCamSender.on_Received(Sender: TObject; AData: Pointer;
  ASize: Integer);
var
  DeskStream: TMemoryStream;
begin
  DeskStream := TMemoryStream.Create;
  DeskStream.Write(AData^, ASize);
  DeskStream.Position := 0;

  PostMessage(Handle, WM_DESK_RECEIVED, Integer(DeskStream), 0);
end;

procedure TDeskCamSender.ProcessConnectionID(const AData: Pointer);
var
  Data: PByte;
begin
  Data := AData;
  FConnectionID := PInteger(Data)^;
end;

procedure TDeskCamSender.RemoveDeskCamReceived;
var
  Msg: tagMSG;
  DeskStream: TMemoryStream;
begin
  while PeekMessage(Msg, Handle, 0, 0, PM_REMOVE) do
  begin
    if Msg.message = WM_DESK_RECEIVED then
    begin
      DeskStream := TMemoryStream(Msg.WParam);
      if Assigned(DeskStream) then
        FreeAndNil(DeskStream);
    end;
  end;
end;

procedure TDeskCamSender.Send(const AData: Pointer; const ASize: Integer);
begin
  FSocket.Send(AData, ASize);

  Inc(FLog.PacketSendCount);
  Inc(FLog.PacketSendAmount, ASize);
end;

procedure TDeskCamSender.SendDeskDataToServer;
var
  DataType: TDataType;
  XIndex, YIndex: Word;
  TickCount: Int64;
  Data: Pointer;
  DataSize: Integer;
  UniqueID: TUniqueID;
begin
  if FFrameBuffer.Count <= 0 then
  begin
    SendEndOfFrameToServer;
    Exit;
  end;

  while FFrameBuffer.Get(DataType, XIndex, YIndex, TickCount, UniqueID, Data, DataSize) do
  begin
    try
      SendFrameBufferData(DataType, XIndex, YIndex, TickCount, Data, DataSize);
    finally
      FreeMem(Data);
    end;
  end;

  SendEndOfFrameToServer;
end;

procedure TDeskCamSender.SendEndOfFrameToServer;
var
  PacketType: TPacketType;
begin
  PacketType := ptSendEndOfFrameToServer;
  Send(@PacketType, sizeof(PacketType));
end;

procedure TDeskCamSender.SendFrameBufferData(
  ADataType: TDataType; const AXIndex, AYIndex: Word;
  const ATickCount: Int64; const AData: Pointer; const ADataSize: Integer);
var
  UniqueID: TUniqueID;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Clear;

    // 이미지 블락 데이터를 전송한다.
    if ADataType in [dtBitmapBlock] then
    begin
      UniqueID := MakeUniqueID(AData, ADataSize);
      FDeskEncoder.Encode(ADataType, AData, ADataSize, Stream);
      SendFrameToServer(0, ADataType, AXIndex, AYIndex, UniqueID, Stream.Memory, Stream.Size);
    end else
    // 이미지 블락 외의 데이터를 전송한다.
    begin
      SendFrameToServer(0, ADataType, AXIndex, AYIndex, 0, AData, ADataSize);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TDeskCamSender.SendFrameToServer(const AConnectionID: Integer;
  const ADataType: TDataType; const AXIndex: Word; const AYIndex: Word;
  const AUniqueID: TUniqueID; const AData: Pointer; const ADataSize: Integer);
var
  PacketType: TPacketType;
  DeskStream: TMemoryStream;
begin
  PacketType := ptSendFrameToServer;

  DeskStream := TMemoryStream.Create;
  try
    DeskStream.Write(PacketType, sizeof(PacketType));
    DeskStream.Write(AConnectionID, sizeof(AConnectionID));
    DeskStream.Write(ADataType, sizeof(ADataType));
    DeskStream.Write(AXIndex, sizeof(AXIndex));
    DeskStream.Write(AYIndex, sizeof(AYIndex));
    DeskStream.Write(AUniqueID, sizeof(AUniqueID));
    DeskStream.Write(ADataSize, sizeof(ADataSize));
    DeskStream.Write(AData^, ADataSize);

    Send(DeskStream.Memory, DeskStream.Size);
  finally
    DeskStream.Free;
  end;
end;

procedure TDeskCamSender.SendUserInfo;
var
  DeskStream: TMemoryStream;
  PacketType: TPacketType;
  IsSender: Boolean;
begin
  DeskStream := TMemoryStream.Create;
  try
    PacketType := ptSendUserInfo;
    DeskStream.Write(PacketType, sizeof(PacketType));

    IsSender := True;
    DeskStream.Write(IsSender, sizeof(IsSender));

    DeskStream.Position := 0;
    Send(DeskStream.Memory, DeskStream.Size);
  finally
    DeskStream.Free;
  end;
end;

procedure TDeskCamSender.SetCaptureType(const Value: TCaptureType);
begin
  FFrameGenerator.CaptureType := Value;
end;

procedure TDeskCamSender.SetMonitorNo(const Value: Integer);
begin
  FFrameGenerator.MonitorNo := Value;
end;

procedure TDeskCamSender.SetPause(const Value: Boolean);
begin
  if not FStarted then Exit;
  
  FPause := Value;
  if Value then
  begin
    FFrameGenerator.Stop;
  end
  else begin
    FFrameGenerator.Start;
  end;
end;

procedure TDeskCamSender.SetCaptureRect(const Value: TRect);
begin
  FFrameGenerator.CaptureRect := Value;
end;

procedure TDeskCamSender.Start;
begin
  HardwareAcceleration(5);

  FFrameGenerator.Start;
  FStarted := True;
  FPause := False;
end;

procedure TDeskCamSender.Stop;
begin
  FStarted := False;
  FFrameGenerator.Stop;

  HardwareAcceleration(0);
end;

procedure TDeskCamSender.on_DeskEncoderEncodeTime(const ATime: Cardinal);
begin
  FLog.EncodeTime := ATime;
  Inc(FLog.EncodeTotalTime, ATime);
end;

procedure TDeskCamSender.on_NewData(Sender: TObject; ADataType: TDataType;
  AXIndex, AYIndex: Word; AData: Pointer; ASize: Integer);
begin
  FFrameBuffer.Add(ADataType, AXIndex, AYIndex, 0, AData, ASize);

  FLog.FrameBufferCount := FFrameBuffer.Count;
  FLog.FrameBufferAmount := FFrameBuffer.Size;
end;

procedure TDeskCamSender.on_Disconnected(Sender: TObject);
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Sender);
end;

end.
