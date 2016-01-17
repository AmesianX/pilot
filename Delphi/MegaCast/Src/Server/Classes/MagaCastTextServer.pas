unit MagaCastTextServer;

interface

uses
  MegaCastUtils, VirtualSocketServer, TextServer, ValueList, RoomInfo,
  Classes, SysUtils, IdTCPServer;

type
  TMagaCastTextServer = class(TTextServer)
  private
    FRoomInfo : TRoomInfo;

    // FDBInterface: TDBInterface;

    procedure sp_OkLogin(const AConnection:TConnection);
    procedure sp_ErLogin(const AConnection:TConnection; const AErrorMsg:string);
    procedure sp_IDinUse(const AConnection:TConnection);
    procedure sp_UserIn(const AConnection:TConnection);
    procedure sp_UserOut(const AConnection:TConnection);
    procedure sp_UserList(const AConnection:TConnection; const AUserList:string);
    procedure sp_RoomInfo(const AConnection:TConnection);
  private
    FRoomNo: Integer;
    FUserLimit: Integer;
  protected
    procedure do_Connected(const AConnection:TConnection; var AChannelInfo:TChannelInfo); override;
    procedure do_BeforeDisconnected(const AConnection:TConnection; const AChannelInfo:TChannelInfo); override;
    procedure do_AfterDisconnected(const AConnection:TConnection; const AChannelInfo:TChannelInfo); override;
    procedure do_Received(const AConnection:TConnection; const AChannelInfo:TChannelInfo; const APacket:TValueList); override;
  public
    constructor Create(AOwner:TComponent; ASocket:TVirtualSocketServer); override;
    destructor Destroy; override;
  published
    procedure rp_Login(const AConnection:TConnection; const APacket:TValueList);
    procedure rp_InvisibleLogin(const AConnection:TConnection; const APacket:TValueList);
    procedure rp_AskUserList(const AConnection:TConnection; const APacket:TValueList);
    procedure rp_KickOut(const AConnection:TConnection; const APacket:TValueList);

    procedure rp_StartCast(const AConnection:TConnection; const APacket:TValueList);
    procedure rp_StopCast(const AConnection:TConnection; const APacket:TValueList);
  published
    property RoomNo: Integer read FRoomNo write FRoomNo;
    property UserLimit: Integer read FUserLimit write FUserLimit;
  end;

implementation

{ TMagaCastTextServer }

constructor TMagaCastTextServer.Create(AOwner: TComponent;
  ASocket: TVirtualSocketServer);
begin
  inherited;

  FRoomInfo := TRoomInfo.Create;
  FUserLimit := 0;
//  FDBInterface := TDBInterface.Create(nil);
end;

destructor TMagaCastTextServer.Destroy;
begin
  FreeAndNil(FRoomInfo);
//  FreeAndNil(FDBInterface);

  inherited;
end;

procedure TMagaCastTextServer.do_AfterDisconnected(
  const AConnection: TConnection; const AChannelInfo: TChannelInfo);
begin

end;

procedure TMagaCastTextServer.do_BeforeDisconnected(
  const AConnection: TConnection; const AChannelInfo: TChannelInfo);
begin
  if AConnection.Logined then begin
    sp_UserOut(AConnection);
    if AConnection.IsAdmin then FRoomInfo.SetAdminID('');
  end;
end;

procedure TMagaCastTextServer.do_Connected(const AConnection: TConnection;
  var AChannelInfo: TChannelInfo);
begin

end;

procedure TMagaCastTextServer.do_Received(const AConnection: TConnection;
  const AChannelInfo: TChannelInfo; const APacket: TValueList);
begin

end;

procedure TMagaCastTextServer.rp_InvisibleLogin(const AConnection: TConnection;
  const APacket: TValueList);
var
  ExUser : TConnection;
  List : TList;
  UserID, Password : string;
begin
  if AConnection.Logined then Exit;

  UserID := Trim(APacket.Values['UserID']);
  Password := Trim(APacket.Values['Password']);

  if Pos('@', UserID) = 0 then begin
    // Todo : ErrorCode 정리
    sp_ErLogin(AConnection, 'ErrorCode: 아이디가 규칙이 잘못되었습니다.');
    Exit;
  end;

  AConnection.UserID := UserID;
  List := LockList;
  try
    ExUser := FindConnectionByUserID(List, UserID);
    if ExUser <> nil then begin
      sp_IDinUse(ExUser);
      ExUser.Logined := False;
    end;
  finally
    UnlockList;
  end;

  AConnection.Logined := true;
  sp_OkLogin(AConnection);
end;

procedure TMagaCastTextServer.rp_AskUserList(const AConnection: TConnection;
  const APacket: TValueList);
var
  List : TList;
  Loop : integer;
  stUserList : string;
  Item : TConnection;
begin
  stUserList := '';

  List := LockList;
  try
    for Loop := 0 to List.Count - 1 do
    begin
      Item := TConnection(TIdPeerThread(List.Items[Loop]).Data);
      if not Item.Logined then Continue;

      // Invisible mode로 참가하고자 하는 아이디는 @로 시작하게 한다.
      // 일반 사용자가 사용하는 프로그램에서는 @로 시작하지 못하도록 방어한다.
      if Pos('@', Item.UserID) > 0 then Continue;

      stUserList := stUserList + Item.Lines.Text + '<rYu>end.<rYu>';
    end;
  finally
    UnlockList;
  end;

  sp_UserList(AConnection, stUserList);
end;

procedure TMagaCastTextServer.rp_KickOut(const AConnection: TConnection;
  const APacket: TValueList);
var
  List: TList;
  Connection: TConnection;
begin
  List := LockList;
  try
    Connection := FindConnectionByUserID(List, APacket.Values['UserID']);
    if Connection <> nil then Connection.Logined := false;
  finally
    UnlockList;
  end;
end;

procedure TMagaCastTextServer.rp_Login(const AConnection: TConnection;
  const APacket: TValueList);
var
  ExUser : TConnection;
  List : TList;
  UserID, Password : string;
begin
  if AConnection.Logined then Exit;

  if APacket.Values['Version'] <> '1.0' then begin
    // Todo : ErrorCode 정리
    sp_ErLogin(AConnection, 'ErrorCode: 프로그램 버전이 맞지 않습니다.');
    Exit;
  end;

  UserID := Trim(APacket.Values['UserID']);
  Password := Trim(APacket.Values['Password']);

  if AConnection.IsAdmin then begin
    FRoomInfo.Lock;
    try
      if FRoomInfo.AdminID <> '' then begin
        // Todo : ErrorCode 정리
        sp_ErLogin(AConnection, 'ErrorCode: 다른 강사가 이미 접속 중 입니다.');
        Exit;
      end else begin
        FRoomInfo.AdminID := UserID;
      end;
    finally
      FRoomInfo.Unlock;
    end;
  end;

  if Pos('@', UserID) > 0 then begin
    // Todo : ErrorCode 정리
    sp_ErLogin(AConnection, 'ErrorCode: 사용이 제한된 아이디입니다.');
    Exit;
  end;

//  if FDBInterface.Login(FRoomNo, UpperCase(UserID), Password, APacket) = false then begin
//    // Todo : ErrorCode 정리
//    sp_ErLogin(AConnection, 'ErrorCode: 아이디 또는 암호가 잘못 되었습니다.');
//    Exit;
//  end;

  AConnection.UserID := UserID;
  AConnection.IsAdmin := APacket.Values['IsAdmin'] <> '';

  List := LockList;
  try
    if (not AConnection.IsAdmin) and (FUserLimit > 0) and (List.Count > FUserLimit) then begin
      // Todo : ErrorCode 정리
      sp_ErLogin(AConnection, 'ErrorCode: 접속자 수가 무료서버의 제한 범위를 넘어섰습니다.');
      Exit;
    end;

    ExUser := FindConnectionByUserID(List, UserID);
    if ExUser <> nil then begin
      sp_IDinUse(ExUser);
      ExUser.Logined := false;
      sp_UserOut(AConnection);
    end;

    AConnection.Logined := True;
  finally
    UnlockList;
  end;

  sp_OkLogin(AConnection);
  sp_RoomInfo(AConnection);
  sp_UserIn(AConnection);
end;

procedure TMagaCastTextServer.rp_StartCast(const AConnection: TConnection;
  const APacket: TValueList);
begin
  FRoomInfo.SetCastStarted(true);
end;

procedure TMagaCastTextServer.rp_StopCast(const AConnection: TConnection;
  const APacket: TValueList);
begin
  FRoomInfo.SetCastStarted(false);
end;

procedure TMagaCastTextServer.sp_ErLogin(const AConnection: TConnection;
  const AErrorMsg: string);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'ErLogin';
    Packet.Values['ErrorMsg'] := AErrorMsg;
    SendTo(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMagaCastTextServer.sp_IDinUse(const AConnection: TConnection);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'IDinUse';
    SendTo(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMagaCastTextServer.sp_OkLogin(const AConnection: TConnection);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Text := AConnection.Lines.Text;
    Packet.Insert(0, 'Code=OkLogin');
    SendTo(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMagaCastTextServer.sp_RoomInfo(const AConnection: TConnection);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'RoomInfo';
    FRoomInfo.CopyTo(Packet);
    SendTo(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMagaCastTextServer.sp_UserIn(const AConnection: TConnection);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Text := AConnection.Lines.Text;
    Packet.Insert(0, 'Code=UserIn');
    SendToOther(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMagaCastTextServer.sp_UserList(const AConnection: TConnection;
  const AUserList: string);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Text := AUserList;
    Packet.Insert(0, 'Code=UserList');
    SendTo(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMagaCastTextServer.sp_UserOut(const AConnection: TConnection);
var
  Packet : TValueList;
begin
  Packet:= TValueList.Create;
  try
    Packet.Text := AConnection.Lines.Text;
    Packet.Insert(0, 'Code=UserOut');
    SendToOther(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

end.
