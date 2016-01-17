unit TextServer;

interface

uses
  VirtualSocketUtils, VirtualSocketServer, ValueList, Strg,
  Classes, SysUtils, IdTCPServer;

type
  TTextServer = class (TComponent)
  private
    FSocket : TVirtualSocketServer;

    procedure on_Connected(Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_BeforeDisconnected(Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_AfterDisconnected(Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_Received(Sender:TObject; AConnection:TConnection; AChannelInfo:TChannelInfo; AData:pointer; ASize:integer);
  private // 패킷 송신 프로토콜 모음
    procedure sp_OkLogin(const AConnection: TConnection);
    procedure sp_ErLogin(const AConnection: TConnection; const AErrorMsg: string);
    procedure sp_IDinUse(const AConnection: TConnection);
    procedure sp_UserIn(const AConnection: TConnection);
    procedure sp_UserOut(const AConnection: TConnection);
    procedure sp_UserList(const AConnection: TConnection; const AUserList: string);
  public
    constructor Create(AOwner:TComponent; ASocket:TVirtualSocketServer); reintroduce;
    destructor Destroy; override;
  published
    procedure rp_Login(const AConnection: TConnection; const APacket: TValueList);
    procedure rp_AskUserList(const AConnection: TConnection; const APacket: TValueList);
  end;

implementation

{ TTextServer }

constructor TTextServer.Create(AOwner: TComponent; ASocket: TVirtualSocketServer);
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnBeforeDisconnected := on_BeforeDisconnected;
  FSocket.OnAfterDisconnected := on_AfterDisconnected;
  FSocket.OnReceived := on_Received;
end;

destructor TTextServer.Destroy;
begin

  inherited;
end;

procedure TTextServer.on_Connected(Sender: TObject; AConnection: TConnection;
  var AChannelInfo: TChannelInfo);
begin
  AChannelInfo := TChannelInfo.Create(AConnection, FSocket.Channel);
end;

procedure TTextServer.on_AfterDisconnected(Sender: TObject;
  AConnection: TConnection; var AChannelInfo: TChannelInfo);
begin
  FreeAndNil(AChannelInfo);
end;

procedure TTextServer.on_BeforeDisconnected(Sender: TObject; AConnection: TConnection;
  var AChannelInfo: TChannelInfo);
begin
  if AConnection.Logined then sp_UserOut(AConnection);
end;

procedure TTextServer.on_Received(Sender: TObject; AConnection: TConnection;
  AChannelInfo: TChannelInfo; AData: pointer; ASize: integer);
var
  Code : string;
  Packet : TValueList;
  Proc : procedure (const AConnection: TConnection; const APacket: TValueList) of object;
begin
  Packet := TValueList.Create;
  try
    Packet.LoadFromBuffer(AData, ASize);

    Proc := nil;
    Code := Packet.Values['Code'];

    TMethod(Proc).Data := Self;
    TMethod(Proc).Code := Self.MethodAddress('rp_' + Code);
    if Assigned(Proc) then Proc(AConnection, Packet);

    case IndexOf(LowerCase(Packet.Values['SendTo']), ['all', 'other', 'userid']) of
         0 : FSocket.SendToAll(AData, ASize);
         1 : FSocket.SendToOther(AConnection, AData, ASize);
         2 : FSocket.SendToUserID(Packet.Values['SendTo.UserID'], AData, ASize);
    end;
  finally
    Packet.Free;       
  end;
end;

procedure TTextServer.rp_AskUserList(const AConnection: TConnection;
  const APacket: TValueList);
var
  List : TList;
  Loop : integer;
  stUserList : string;
  Item : TConnection;
begin
  stUserList := '';

  List := FSocket.LockList;
  try
    for Loop := 0 to List.Count - 1 do begin
      Item := TConnection(TIdPeerThread(List.Items[Loop]).Data);
      if not Item.Logined then Continue;
      if UpperCase(Item.UserID) = 'SYSTEM_ADMIN' then Continue;

      stUserList := stUserList + Item.Lines.Text + '<rYu>end.<rYu>';
    end;
  finally
    FSocket.UnlockList;
  end;

  sp_UserList(AConnection, stUserList);
end;

procedure TTextServer.rp_Login(const AConnection: TConnection;
  const APacket: TValueList);
var
  ExUser, ExAdmin : TConnection;
  List : TList;
  UserID, Password : string;
begin
  if AConnection.Logined then Exit;

  if APacket.Values['Version'] <> '1.0' then begin
    sp_ErLogin(AConnection, '프로그램 버전이 맞지 않습니다.');
    Exit;
  end;

  UserID := Trim(APacket.Values['UserID']);
  Password := Trim(APacket.Values['Password']);

  if UpperCase(UserID) = 'SYSTEM_ADMIN' then begin
    sp_ErLogin(AConnection, '사용이 제한된 아이디입니다.');
    Exit;
  end;

  // Todo : 
//  if FDBInterface.Login(FRoomNo, UpperCase(UserID), Password, APacket) = False then
//  begin
//    sp_ErLogin(AConnection, '아이디 또는 암호가 잘못 되었습니다.');
//    Exit;
//  end;

  AConnection.UserID := UserID;

  List := FSocket.LockList;
  try
    ExUser := FSocket.FindConnectionByUserID(List, UserID);
    if ExUser <> nil then begin
      sp_IDinUse(ExUser);
      ExUser.Logined := False;
      sp_UserOut(AConnection);
    end;

    AConnection.Logined := True;
  finally
    FSocket.UnlockList;
  end;

  sp_OkLogin(AConnection);
  sp_UserIn(AConnection);
end;

procedure TTextServer.sp_ErLogin(const AConnection: TConnection;
  const AErrorMsg: string);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'ErLogin';
    Packet.Values['ErrorMsg'] := AErrorMsg;
    FSocket.SendTo(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextServer.sp_IDinUse(const AConnection: TConnection);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'IDinUse';
    FSocket.SendTo(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextServer.sp_OkLogin(const AConnection: TConnection);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Text := AConnection.Lines.Text;
    Packet.Insert(0, 'Code=OkLogin');
    FSocket.SendTo(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextServer.sp_UserIn(const AConnection: TConnection);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Text := AConnection.Lines.Text;
    Packet.Insert(0, 'Code=UserIn');
    FSocket.SendToOther(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextServer.sp_UserList(const AConnection: TConnection;
  const AUserList: string);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Text := AUserList;
    Packet.Insert(0, 'Code=UserList');
    FSocket.SendTo(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextServer.sp_UserOut(const AConnection: TConnection);
var
  Packet : TValueList;
begin
  Packet:= TValueList.Create;
  try
    Packet.Text := AConnection.Lines.Text;
    Packet.Insert(0, 'Code=UserOut');
    FSocket.SendToOther(AConnection, Packet);
  finally
    Packet.Free;
  end;
end;

end.
