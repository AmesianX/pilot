unit TextClient;

interface

uses
  ValueList, EasyClient, Option,
  Windows, Classes, Messages, Graphics, SysUtils;

const
  WM_TextReceived = WM_USER + 1;

type
  TPacketEvent = procedure (Sender:TObject; APacket:TValueList) of object;

  TTextClient = class(TEasyClient)    
  private
    FOnPacket: TPacketEvent;
  private
    procedure do_WMTextReceived(var Msg:TMessage); message WM_TextReceived;
    procedure on_TextReceived(Sender:TObject; APacket:TValueList; AData:pointer; ASize:integer);
  public
    constructor Create(AOwner: TComponent); override;

    // Command
    procedure sp_Login(const AUserID: string; const APassword: string; const AIsAdmin: Boolean);
    procedure sp_AskUserList;
    procedure sp_KickOut(const AUserID: string; const AMsg: string);
    procedure sp_Alert(const AUserID: string);

    // Chat
    procedure sp_Talk(FromID, Msg: string; ATextColor: TColor);
    procedure sp_Whisper(FromID, UserID, Msg: string; ATextColor: TColor);

    // Voice
    procedure sp_AskMic(const AUserID: string);
    procedure sp_SetMic(const AUserID: string; const AOnOff: Boolean);
  published
    property OnPacket : TPacketEvent read FOnPacket write FOnPacket;
  end;

implementation

{ TTextClient }

constructor TTextClient.Create(AOwner: TComponent);
begin
  inherited;

  OnReceived := on_TextReceived;
end;

procedure TTextClient.do_WMTextReceived(var Msg: TMessage);
var
  Packet : TValueList;
begin
  if Msg.WParam = 0 then Exit;

  Packet := Pointer(Msg.WParam);
  try
    if Assigned(FOnPacket) then FOnPacket(Self, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextClient.on_TextReceived(Sender: TObject; APacket: TValueList;
  AData: pointer; ASize: integer);
var
  Packet : TValueList;
begin
  if APacket = nil then Exit;

  Packet := TValueList.Create;
  Packet.Text := APacket.Text;
  PostMessage(Handle, WM_TextReceived, Integer(Packet), 0);
end;

procedure TTextClient.sp_Alert(const AUserID: string);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'Alert';
    Packet.Values['UserID'] := AUserID;
    Packet.Values['SendTo'] := 'All';
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextClient.sp_AskMic(const AUserID: string);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'AskMic';
    Packet.Values['UserID'] := AUserID;
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextClient.sp_AskUserList;
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'AskUserList';
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextClient.sp_KickOut(const AUserID: string; const AMsg: string);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'KickOut';
    Packet.Values['SendTo'] := 'All';
    Packet.Values['UserID'] := AUserID;
    Packet.Values['Msg'] := AMsg;
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextClient.sp_Login(const AUserID, APassword: string;
  const AIsAdmin: Boolean);
var
  Packet: TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'Login';
    Packet.Values['Version'] := '1.0';
    Packet.Values['UserID'] := AUserID;
    Packet.Values['Password'] := APassword;
    if AIsAdmin then Packet.Values['IsAdmin'] := 'Yes';
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextClient.sp_SetMic(const AUserID: string; const AOnOff: Boolean);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'SetMic';
    Packet.Values['SendTo'] := 'All';
    Packet.Values['UserID'] := AUserID;
    Packet.Boolean['OnOff'] := AOnOff;
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextClient.sp_Talk(FromID, Msg: string; ATextColor: TColor);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code']   := 'Talk';
    Packet.Values['FromID'] := FromID;
    Packet.Values['Msg']    := Msg;
    Packet.Values['SendTo'] := 'All';
    Packet.Integers['TextColor'] := ATextColor;
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextClient.sp_Whisper(FromID, UserID, Msg: string;
  ATextColor: TColor);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code']  := 'Whisper';
    Packet.Values['FromID']:= FromID;
    Packet.Values['Msg']   := Msg;
    Packet.Values['SendTo']:= 'UserID';
    Packet.Values['SendTo.UserID'] := Format('<%s><%s>', [FromID, UserID]);
    Packet.Integers['TextColor'] := ATextColor;
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

end.
