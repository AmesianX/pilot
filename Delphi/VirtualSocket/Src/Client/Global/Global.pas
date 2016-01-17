unit Global;

interface

uses
  TextClient, ValueList, UserList, VirtualClient, CamClient, VoiceClient,
  DataTypes, Option, HandleComponent,
  Windows, Messages, Classes, Graphics, SysUtils, ExtCtrls;

const
  WM_CAM_IMAGE = WM_USER + 1;

  PORT_OFFSET: Integer = 1000;  // 다른 메가채널 서버의 포트와 겹치지 않기 위한 임시 포트 오프셋

type
  TTextProtocol = class (TComponent)
  private
    procedure ProcessUserInPacket(const APacket: TValueList);
  public
  published
    procedure rp_OkLogin(APacket: TValueList);
    procedure rp_IDinUse(APacket: TValueList);
    procedure rp_UserList(APacket: TValueList);
    procedure rp_UserIn(APacket: TValueList);
    procedure rp_UserOut(APacket: TValueList);
    procedure rp_SetMic(APacket: TValueList);
  end;

  TGlobal = class(THandleComponent)
  private
    FInitialized: boolean;

    FTextProtocol: TTextProtocol;
    FVirtualClient: TVirtualClient;
    FUserList: TUserList;
    FTextClient: TTextClient;
    FVoiceClient: TVoiceClient;
    FCamClient: TCamClient;

    function Connect: Boolean;
    procedure Disconnect;

    // UserList
    procedure on_UserListChanged(Sender: TObject;
      const AAction: TUserListAction; const AUser: TUser);

    // TextClient
    procedure on_TextReceived(Sender: TObject; APacket: TValueList);
    procedure on_TextDisconnected(Sender: TObject);

    // VoiceClient
    procedure on_VoiceClientDisconnected(Sender: TObject);

    // CamClient
    procedure on_CamImage(Sender: TObject; AUserID: string; ACamImage: TBitmap);
    procedure do_WM_CAM_IMAGE(var Msg: TMessage); message WM_CAM_IMAGE;

    // VirtualClient
    procedure on_VirtualClientReceived(Sender: TObject; APacketType: Byte;
      AData: Pointer; ASize: Integer);
    procedure on_VirtualClientDisconnected(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Obj: TGlobal;

    procedure Initialize;
    procedure Finalize;
  published
    property Initialized : boolean read FInitialized;

    property UserList: TUserList read FUserList;
    property TextClient: TTextClient read FTextClient;
    property CamClient: TCamClient read FCamClient;
    property VoiceClient: TVoiceClient read FVoiceClient;
  end;

implementation

uses
  View;

var
  MyObj: TGlobal = nil;

{ TGlobal }

function TGlobal.Connect: Boolean;
begin
  Result := FTextClient.Connect and FVoiceClient.Connect and FVirtualClient.Connect;
end;

constructor TGlobal.Create(AOwner: TComponent);
begin
  inherited;

  FInitialized := False;

  FTextProtocol := TTextProtocol.Create(Self);

  // UserList
  FUserList := TUserList.Create;
  FUserList.OnChanged := on_UserListChanged;

  // TextClient
  FTextClient := TTextClient.Create(Self);
  FTextClient.Host := TOption.Obj.Host;
  FTextClient.Port := TOption.Obj.RoomNo + 10000 + PORT_OFFSET;
  FTextClient.OnPacket := on_TextReceived;
  FTextClient.OnDisconnected := on_TextDisconnected;

  // VoiceClient
  FVoiceClient := TVoiceClient.Create(Self, 2);
  FVoiceClient.Host := TOption.Obj.Host;
  FVoiceClient.Port := TOption.Obj.RoomNo + 20000 + PORT_OFFSET;
  FVoiceClient.OnDisconnected := on_VoiceClientDisconnected;

  // VirtualClient
  FVirtualClient := TVirtualClient.Create(Self);
  FVirtualClient.Host := TOption.Obj.Host;
  FVirtualClient.Port := TOption.Obj.RoomNo + 30000 + PORT_OFFSET;
  FVirtualClient.PacketLimit := 1024 * 64;
  FVirtualClient.OnDisconnected := on_VirtualClientDisconnected;
  FVirtualClient.OnReceived := on_VirtualClientReceived;

  // CamClient
  FCamClient := TCamClient.Create(Self);
  FCamClient.Socket := FVirtualClient;
  FCamClient.OnCamImage := on_CamImage;
end;

destructor TGlobal.Destroy;
begin
  FVoiceClient.Free;
  FCamClient.Free;
  FUserList.Free;
  FVirtualClient.Free;
  FTextProtocol.Free;
  TextClient.Free;

  Finalize;

  inherited;
end;

procedure TGlobal.Disconnect;
begin
  FTextClient.Disconnect;
  FVoiceClient.Disconnect;
  FVirtualClient.Disconnect;
end;

procedure TGlobal.do_WM_CAM_IMAGE(var Msg: TMessage);
var
  Packet: TValueList;
  CamImage: TBitmap;
begin
  Packet := TValueList(Msg.WParam);
  try
    Packet.Values['Code'] := 'CamImage';
    TView.Obj.BroadCast(Packet);
  finally
    CamImage := Packet.Pointers['CamImage'];
    if CamImage <> nil then
      CamImage.Free;
    Packet.Free;
  end;
end;

procedure TGlobal.Finalize;
begin
  if not FInitialized then Exit;
  FInitialized := False;

  Disconnect;
end;

procedure TGlobal.Initialize;
begin
  if FInitialized then Exit;

  if not Connect then
    raise Exception.Create('서버에 접속할 수가 없습니다.');

  FInitialized := True;
end;

class function TGlobal.Obj: TGlobal;
begin
  if MyObj = nil then MyObj := TGlobal.Create(nil);
  Result := MyObj;
end;

procedure TGlobal.on_VirtualClientDisconnected(Sender: TObject);
begin
  CamClient.ActiveSendData := False;
  CamClient.ActiveGetData := False;
end;

procedure TGlobal.on_VirtualClientReceived(Sender: TObject; APacketType: Byte;
  AData: Pointer; ASize: Integer);
begin
  case TPacketType(APacketType) of
    ptCam: FCamClient.DataIn(AData, ASize);
  end;
end;

procedure TGlobal.on_VoiceClientDisconnected(Sender: TObject);
begin
  VoiceClient.Stop;
end;

procedure TGlobal.on_CamImage(Sender: TObject; AUserID: string;
  ACamImage: TBitmap);
var
  Packet : TValueList;
  Bitmap: TBitmap;
begin
  Packet := TValueList.Create;
  Packet.Values['UserID'] := AUserID;
  if ACamImage <> nil then
  begin
    Bitmap := TBitmap.Create;
    Bitmap.Assign(ACamImage);
    Packet.Pointers['CamImage'] := Bitmap;
  end else
  begin
    Packet.Pointers['CamImage'] := nil;
  end;

  PostMessage(Handle, WM_CAM_IMAGE, Integer(Packet), 0);
end;

procedure TGlobal.on_TextDisconnected(Sender: TObject);
begin
  TView.Obj.sp_Terminate('서버와의 접속이 끊어졌습니다.');
end;

procedure TGlobal.on_TextReceived(Sender: TObject; APacket: TValueList);
var
  Proc : procedure (Packet:TValueList) of object;
begin
  if APacket = nil then Exit;

  TMethod(Proc).Data := FTextProtocol;
  TMethod(Proc).Code := FTextProtocol.MethodAddress('rp_' + APacket.Values['Code']);
  if Assigned(Proc) then Proc(APacket);

  TView.Obj.BroadCast(APacket);
end;

procedure TGlobal.on_UserListChanged(Sender: TObject;
  const AAction: TUserListAction; const AUser: TUser);
var
  Packet: TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'UserListChanged';
    Packet.Integers['Action'] := Integer(AAction);
    Packet.Pointers['User'] := AUser;

    TView.Obj.BroadCast(Packet);
  finally
    Packet.Free;
  end;
end;

{ TTextProtocol }

procedure TTextProtocol.ProcessUserInPacket(const APacket: TValueList);
var
  UserID: string;
  IsAdmin: Boolean;
  MicOnOff: Boolean;
begin
  UserID := APacket.Values['UserID'];
  if Trim(UserID) = '' then Exit;
  if TGlobal(Owner).UserList.FindByUserID(UserID) <> nil then Exit;

  IsAdmin := UpperCase(APacket.Values['IsAdmin']) = 'YES';
  MicOnOff := APacket.Boolean['MicOnOff'];

  TGlobal(Owner).UserList.Add(UserID, IsAdmin, MicOnOff);
end;

procedure TTextProtocol.rp_IDinUse(APacket: TValueList);
begin
  TView.Obj.sp_Terminate('동일한 아이디로 접속한 사용자가 있으므로, 프로그램을 종료합니다.');
end;

procedure TTextProtocol.rp_OkLogin(APacket: TValueList);
begin
  TGlobal(Owner).CamClient.UserID := APacket.Values['UserID'];
  TGlobal(Owner).CamClient.ActiveGetData := True;

  TGlobal(Owner).VoiceClient.Players.Mute := False;

  TGlobal(Owner).TextClient.sp_AskUserList;
end;

procedure TTextProtocol.rp_SetMic(APacket: TValueList);
var
  UserID: string;
  OnOff: Boolean;
  User: TUser;
begin
  UserID := APacket.Values['UserID'];
  OnOff := APacket.Boolean['OnOff'];

  User := TGlobal(Owner).UserList.FindByUserID(UserID);
  if User <> nil then User.MicOnOff := OnOff;

  if UpperCase(UserID) = UpperCase(TOption.Obj.UserID) then
  begin
    if OnOff then TGlobal(Owner).VoiceClient.Start(TOption.Obj.MicDeviceID)
    else TGlobal(Owner).VoiceClient.Stop;
  end;
end;

procedure TTextProtocol.rp_UserIn(APacket: TValueList);
begin
  ProcessUserInPacket(APacket);
end;

procedure TTextProtocol.rp_UserList(APacket: TValueList);
var
  i: Integer;
  vlTemp: TValueList;
begin
  vlTemp := TValueList.Create;
  try
    for i:=0 to APacket.Count-1 do
    begin
      vlTemp.Add(APacket.Strings[i]);
      if APacket.Strings[i] = 'end.' then
      begin
        ProcessUserInPacket(vlTemp);
        vlTemp.Clear;
      end;
    end;
  finally
    vlTemp.Free;
  end;
end;

procedure TTextProtocol.rp_UserOut(APacket: TValueList);
begin
  TGlobal(Owner).UserList.Delete(APacket.Values['UserID']);
end;

end.
