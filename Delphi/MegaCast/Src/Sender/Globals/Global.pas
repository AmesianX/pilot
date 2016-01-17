unit Global;

interface

uses
  ValueList, UserList, VirtualSocketUtils, 
  RoomClient, MegaCastMasterVoiceClient, MegaCastTextClient,
  MegaCastUtils, MegaCastClient, MegaCastRecorder,
  Classes, SysUtils;

type
  TGlobal = class(TComponent)
  strict private
    procedure on_Disconnected(Sender:TObject);
    procedure on_UserListChange(Sender:TObject);
    procedure on_TextReceived(Sender:TObject; APacket:TValueList);
    procedure on_TextSocketIsReady(Sender:TObject);
    procedure on_NeedFromServer(Sender:TObject);
    procedure on_FrameSizeChanged(Sender:TObject; AFrameSize:TFrameSize);
  strict private
    FInitialized: boolean;
  public
    RoomClient : TRoomClient;
    VoiceClient: TMegaCastMasterVoiceClient;
    TextClient : TMegaCastTextClient;
    MegaCastClient : TMegaCastClient;
    MegaCastRecorder : TMegaCastRecorder;
    UserList : TUserList;

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    class function Obj:TGlobal;

    procedure Initialize;
    procedure Finalize;

    procedure StartCast;
    procedure StopCast;
  published
    property Initialized : boolean read FInitialized;
  end;

implementation

uses
  View, Option, _fmMsg;

var
  MyObj : TGlobal = nil;

{ TGlobal }

constructor TGlobal.Create(AOwner:TComponent);
begin
  inherited;

  FInitialized := false;

  RoomClient := TRoomClient.Create(Self);
  RoomClient.OnDisconnected := on_Disconnected;

  VoiceClient := TMegaCastMasterVoiceClient.Create(Self, RoomClient.CreateSocket);

  TextClient := TMegaCastTextClient.Create(Self, RoomClient.CreateSocket);
  TextClient.OnReceived := on_TextReceived;
  TextClient.OnReadyToSend := on_TextSocketIsReady;

  MegaCastClient := TMegaCastClient.Create(Self, RoomClient.CreateSocket);
  MegaCastClient.OnNeedFromServer := on_NeedFromServer;
  
  MegaCastRecorder := TMegaCastRecorder.Create(Self);
  MegaCastRecorder.MonitorNo := 0;
  MegaCastRecorder.OnFrameSizeChanged := on_FrameSizeChanged;

  UserList := TUserList.Create(Self);
  UserList.OnChange := on_UserListChange;
end;

destructor TGlobal.Destroy;
begin
  Finalize;

  FreeAndNil(RoomClient);
  FreeAndNil(VoiceClient);
  FreeAndNil(TextClient);
  FreeAndNil(MegaCastClient);
  FreeAndNil(MegaCastRecorder);
  FreeAndNil(UserList);

  inherited;
end;

procedure TGlobal.Finalize;
begin
  if not FInitialized then Exit;
  FInitialized := false;

  StopCast;
  VoiceClient.Player.Stop;
  RoomClient.Disconnect; 
end;

procedure TGlobal.Initialize;
begin
  if FInitialized then Exit;

  RoomClient.Host := TOption.Obj.ServerHost;
  RoomClient.RoomNo := TOption.Obj.RoomNo;

  // Todo : ErrorCode 정리
  if not RoomClient.Connect then
    raise Exception.Create('ErrorCode: 서버 접속에 실패하였습니다.');

  FInitialized := true;
end;

class function TGlobal.Obj: TGlobal;
begin
  if MyObj = nil then MyObj := TGlobal.Create(nil);
  Result := MyObj;
end;

procedure TGlobal.on_Disconnected(Sender: TObject);
begin
  // Todo : ErrorCode 정리
  TView.Obj.sp_Terminate('ErrorCode: 서버와의 접속이 끊어졌습니다.');
end;

procedure TGlobal.on_FrameSizeChanged(Sender: TObject; AFrameSize: TFrameSize);
begin
  MegaCastClient.SendFrameSize(AFrameSize);
end;

procedure TGlobal.on_NeedFromServer(Sender: TObject);
var
  BlockUnit : pointer;
  UnitSize : integer;
  BytesSent : int64;
begin
  BytesSent := 0;
  while MegaCastRecorder.GetBlockUnit(BlockUnit, UnitSize) do begin
    try
      MegaCastClient.SendBlockUnit(BlockUnit, UnitSize);

      BytesSent := BytesSent + UnitSize;
      if BytesSent >= _PacketSizeLimit then Break;
    finally
      FreeMem(BlockUnit);
    end;
  end;

  MegaCastClient.SendEndOfData;
end;

procedure TGlobal.on_TextReceived(Sender: TObject; APacket: TValueList);
begin
  VoiceClient.ExecutePacket(APacket);
  UserList.ExecutePacket(APacket);

  TView.Obj.Broadcast(APacket);

  {$IFDEF DEBUG}
    fmMsg.Add(APacket.Text);
  {$ENDIF}
end;

procedure TGlobal.on_TextSocketIsReady(Sender: TObject);
begin
  TView.Obj.sp_TextSocketIsReady;
end;

procedure TGlobal.on_UserListChange(Sender: TObject);
begin
  TView.Obj.sp_UserListChange;
end;

procedure TGlobal.StartCast;
begin
  TextClient.sp_StartCast;

  MegaCastRecorder.Start;
  MegaCastClient.SendFrameSize(MegaCastRecorder.FrameSize);

  VoiceClient.Talk;
end;

procedure TGlobal.StopCast;
begin
  MegaCastRecorder.Stop;
  VoiceClient.Listen;

  TextClient.sp_StopCast;
end;

end.

