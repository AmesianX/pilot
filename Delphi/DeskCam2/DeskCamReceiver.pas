unit DeskCamReceiver;

interface

uses
  HandleComponent, VirtualSocketClient, DeskCamUtils, DeskDecoderBase,
  FrameBlockCache, DeskCamAddonUtils, DeskSyncBuffer, Time,
  Windows, Messages, Classes, SysUtils, ExtCtrls;

type
  TDeskCamReceiver = class(THandleComponent)
  private
    FSocket: TVirtualSocketClient;
    FBuffer: TDeskSyncBuffer;
    FDeskDecoder: TDeskDecoderBase;
    FFrameBlockCache: TFrameBlockCache;
    FImage: TImage;
    FConnectionID: Integer;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FFrameSizeReceived: Boolean;
    FLog: TLog;

    procedure on_Connected(Sender: TObject);
    procedure on_Disconnected(Sender: TObject);
    procedure on_Received(Sender: TObject; AData: Pointer; ASize: Integer);
    procedure do_WMReceived(var Msg: TMessage); message WM_DESK_RECEIVED;
    procedure on_BufferNewData(Sender: TObject);
    procedure on_DeskDecoderDecodeTime(const ATime: Cardinal);

    // Process
    procedure ProcessFrame(const AData: Pointer);
    procedure ProcessUniqueIDFrame(const AUniqueID: TUniqueID;
      ADataType: TDataType; const AXIndex: Word; const AYIndex: Word);
    procedure ProcessDataFrame(const ADataType: TDataType;
      const AXIndex: Word; const AYIndex: Word; const AData: Pointer; const ADataSize: Integer);
    procedure ProcessConnectionID(const AData: Pointer);

    // Send
    procedure Send(const AData: Pointer; const ASize: Integer);
    procedure SendUserInfo;
    procedure SendNeedFrameToServer;

    procedure RepaintImage;
    procedure RemoveDeskReceivedMessage;
  public
    constructor Create(AOwner: TComponent; ASocket: TVirtualSocketClient); reintroduce;
    destructor Destroy; override;

    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property Screen: TImage read FImage write FImage;
    property ConnectionID: Integer read FConnectionID;
    property Log: TLog read FLog;
  end;

implementation

{ TDeskCamReceiver }

constructor TDeskCamReceiver.Create(AOwner: TComponent; ASocket: TVirtualSocketClient);
var
  Capacity: Cardinal;
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnDisconnected := on_Disconnected;
  FSocket.OnReceived := on_Received;

  FBuffer := TDeskSyncBuffer.Create;
  FBuffer.OnNewData := on_BufferNewData;

  FDeskDecoder := TDeskDecoderBase.Create(Self);
  FDeskDecoder.OnDecodeTime := on_DeskDecoderDecodeTime;

  FFrameBlockCache := TFrameBlockCache.Create(True, CacheFileName);
  Capacity := 1024 * 1024;
  Capacity := Capacity * 1024 * 2;  // 2GB
  FFrameBlockCache.Capacity := Capacity;

  FLog := TLog.Create;
  FLog.CacheCapacity := FFrameBlockCache.Capacity;
end;

destructor TDeskCamReceiver.Destroy;
begin
  FSocket.OnReceived := nil;
  FBuffer.Free;
  FDeskDecoder.Free;
  FFrameBlockCache.Free;
  FLog.Free;

  RemoveDeskReceivedMessage;

  inherited Destroy;
end;

procedure TDeskCamReceiver.do_WMReceived(var Msg: TMessage);
var
  DeskStream: TMemoryStream;
  PacketType: TPacketType;
  Data: PByte;
  DataSize: Integer;
begin
  DeskStream := TMemoryStream(Msg.WParam);
  try
    Inc(FLog.PacketRecvCount);
    Inc(FLog.PacketRecvAmount, DeskStream.Size);

    Data := DeskStream.Memory;

    PacketType := PPacketType(Data)^;
    Inc(Data, sizeof(PacketType));

    DataSize := DeskStream.Size - sizeof(PacketType);

    case PacketType of
      ptSendConnectionID:
      begin
        ProcessConnectionID(Data);
      end;

      ptSendFrameToReceiver:
      begin
        FBuffer.Add(Data, DataSize);

        // Todo: AudioSync를 받았을때 값을 넣어주도록 해야 한다.
        // 현재는 무조건 동작하도록 하기 위해 현재 틱 값을 넣어준다.
        FBuffer.VoiceLastTick := Time.QueryPerformanceCounter;
      end;

      ptSendEndOfFrameToReceiver:
      begin
        Sleep(200);
        SendNeedFrameToServer;
      end;
    end;
  finally
    DeskStream.Free;
  end;
end;

procedure TDeskCamReceiver.on_BufferNewData(Sender: TObject);
var
  Data: Pointer;
  DataSize: Integer;
begin
  while FBuffer.Get(Data, DataSize) do
  begin
    try
      ProcessFrame(Data);
    finally
      FreeMem(Data);
    end;
  end;
end;

procedure TDeskCamReceiver.on_Connected(Sender: TObject);
begin
  if Assigned(FOnConnected) then
    FOnConnected(Sender);

  FFrameSizeReceived := False;
  SendUserInfo;
  SendNeedFrameToServer;
end;

procedure TDeskCamReceiver.on_DeskDecoderDecodeTime(const ATime: Cardinal);
begin
  FLog.DecodeTime := ATime;
  Inc(FLog.DecodeTotalTime, ATime);
end;

procedure TDeskCamReceiver.on_Disconnected(Sender: TObject);
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Sender);
end;

procedure TDeskCamReceiver.on_Received(Sender: TObject; AData: Pointer;
  ASize: Integer);
var
  DeskStream: TMemoryStream;
begin
  DeskStream := TMemoryStream.Create;
  DeskStream.Write(AData^, ASize);
  DeskStream.Position := 0;

  PostMessage(Handle, WM_DESK_RECEIVED, Integer(DeskStream), 0);
end;

procedure TDeskCamReceiver.ProcessConnectionID(const AData: Pointer);
var
  Data: PByte;
begin
  Data := AData;
  FConnectionID := PInteger(Data)^;
end;

procedure TDeskCamReceiver.ProcessDataFrame(const ADataType: TDataType;
  const AXIndex, AYIndex: Word; const AData: Pointer; const ADataSize: Integer);
var
  UniqueID: TUniqueID;
begin
  FDeskDecoder.DataIn(ADataType, AXIndex, AYIndex, AData, ADataSize, UniqueID);

  if ADataType in [dtZLibBlock, dtJpegBlock] then
  begin
    FFrameBlockCache.Add(AData, ADataSize, ADataType, UniqueID);

    FLog.CacheUsedAmount := FFrameBlockCache.Size;
    FLog.CacheCount := FFrameBlockCache.Count;

    Inc(FLog.PacketBlockRecvCount);
    Inc(FLog.PacketBlockRecvAmount, ADataSize);
  end;
end;

procedure TDeskCamReceiver.ProcessFrame(const AData: Pointer);
var
  Data: PByte;
  DataType: TDataType;
  XIndex, YIndex: Word;
  DataSize: Integer;
  UniqueID: TUniqueID;
begin
  Data := AData;

  DataType := PDataType(Data)^;
  Inc(Data, sizeof(DataType));

  XIndex := PWord(Data)^;
  Inc(Data, sizeof(XIndex));

  YIndex := PWord(Data)^;
  Inc(Data, sizeof(YIndex));

  DataSize := PInteger(Data)^;
  Inc(Data, sizeof(DataSize));

  if DataType in [dtFrameSize] then
    FFrameSizeReceived := True;

  if not FFrameSizeReceived then
    Exit;

  // 이미지 블락에 대한 키 데이터만 받았을 경우
  if DataType in [dtZLibBlockKey, dtJpegBlockKey] then
  begin
    UniqueID := PUniqueID(Data)^;
    ProcessUniqueIDFrame(UniqueID, DataType, XIndex, YIndex);
  end else
  // 이미지 블락에 대한 실제 데이터를 받았을 경우 (또는 다른 비트맵 사이즈 등의 다른 데이터들)
  begin
    ProcessDataFrame(DataType, XIndex, YIndex, Data, DataSize);

    if DataType = dtEndOfData then
      RepaintImage;
  end;
end;

procedure TDeskCamReceiver.ProcessUniqueIDFrame(const AUniqueID: TUniqueID;
  ADataType: TDataType; const AXIndex, AYIndex: Word);
var
  CacheData: Pointer;
  CacheDataSize: Integer;
  UniqueID: TUniqueID;
  DataType: TDataType;
begin
  if FFrameBlockCache.Get(AUniqueID, DataType, CacheData, CacheDataSize) then
  begin
    try
      if ADataType = dtZLibBlockKey then ADataType := dtZLibBlock
      else if ADataType = dtJpegBlockKey then ADataType := dtJpegBlock;

      FDeskDecoder.DataIn(ADataType, AXIndex, AYIndex, CacheData, CacheDataSize, UniqueID);

      Inc(FLog.CacheReusedCount);
      Inc(FLog.CacheReusedAmount, CacheDataSize);
    finally
      FreeMem(CacheData);
    end;
  end;
end;

procedure TDeskCamReceiver.RemoveDeskReceivedMessage;
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

procedure TDeskCamReceiver.RepaintImage;
begin
  if FDeskDecoder.GetBitmap(FImage.Picture.Bitmap) then
    FImage.Repaint;
end;

procedure TDeskCamReceiver.Send(const AData: Pointer; const ASize: Integer);
begin
  FSocket.Send(AData, ASize);

  Inc(FLog.PacketSendCount);
  Inc(FLog.PacketSendAmount, ASize);
end;

procedure TDeskCamReceiver.SendNeedFrameToServer;
var
  PacketType: TPacketType;
begin
  PacketType := ptNeedFrameToServer;
  Send(@PacketType, sizeof(PacketType));
end;

procedure TDeskCamReceiver.SendUserInfo;
var
  DeskStream: TMemoryStream;
  PacketType: TPacketType;
  IsSender: Boolean;
begin
  DeskStream := TMemoryStream.Create;
  try
    PacketType := ptSendUserInfo;
    DeskStream.Write(PacketType, sizeof(PacketType));

    IsSender := False;
    DeskStream.Write(IsSender, sizeof(IsSender));

    DeskStream.Position := 0;
    Send(DeskStream.Memory, DeskStream.Size);
  finally
    DeskStream.Free;
  end;
end;

end.
