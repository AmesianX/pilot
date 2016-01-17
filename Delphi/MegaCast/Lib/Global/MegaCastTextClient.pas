unit MegaCastTextClient;

interface

uses
  ValueList, VirtualSocketClient, TextClient,
  Classes, SysUtils, Graphics;

type
  TTextReceivedEvent = procedure (Sender:TObject; APacket:TValueList) of object;

  TMegaCastTextClient = class(TTextClient)
  private
    FOnReceived: TTextReceivedEvent;
    FOnReadyToSend: TNotifyEvent;
  protected
    procedure do_Received(Sender:TObject; APacket:TValueList); override;
    procedure do_ReadyToSend(Sender:TObject); override;
  public
    constructor Create(AOwner: TComponent; ASocket: TVirtualSocketClient); override;
    destructor Destroy; override;
  public
    procedure sp_Login(UserID,Password:string; IsAdmin:boolean);
    procedure sp_AskUserList;
    procedure sp_KickOut(UserID,Msg:string);
    procedure sp_Talk(FromID,Msg:string; TextColor:TColor);
    procedure sp_Whisper(FromID,UserID,Msg:string; TextColor:TColor);
    procedure sp_StartCast;
    procedure sp_StopCast;
  published
    procedure rp_OkLogin(APacket:TValueList);
  published
    property OnReceived : TTextReceivedEvent read FOnReceived write FOnReceived;
    property OnReadyToSend : TNotifyEvent read FOnReadyToSend write FOnReadyToSend;
  end;

implementation

{ TMegaCastTextClient }

constructor TMegaCastTextClient.Create(AOwner: TComponent;
  ASocket: TVirtualSocketClient);
begin
  inherited;

end;

destructor TMegaCastTextClient.Destroy;
begin

  inherited;
end;

procedure TMegaCastTextClient.do_ReadyToSend(Sender: TObject);
begin
  if Assigned(FOnReadyToSend) then FOnReadyToSend(Self);  
end;

procedure TMegaCastTextClient.do_Received(Sender: TObject; APacket: TValueList);
var
  Proc : procedure (APacket:TValueList) of object;
begin
  TMethod(Proc).Data := Self;
  TMethod(Proc).Code := Self.MethodAddress('rp_' + APacket.Values['Code']);
  if Assigned(Proc) then Proc(APacket);

  if Assigned(FOnReceived) then FOnReceived(Self, APacket);
end;

procedure TMegaCastTextClient.rp_OkLogin(APacket: TValueList);
begin
  sp_AskUserList;
end;

procedure TMegaCastTextClient.sp_AskUserList;
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

procedure TMegaCastTextClient.sp_KickOut(UserID, Msg: string);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'KickOut';
    Packet.Values['SendTo'] := 'All';
    Packet.Values['UserID'] := UserID;
    Packet.Values['Msg'] := Msg;
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMegaCastTextClient.sp_Login(UserID, Password: string;
  IsAdmin: boolean);
var
  Packet: TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'Login';
    Packet.Values['Version'] := '1.0';
    Packet.Values['UserID'] := UserID;
    Packet.Values['Password'] := Password;
    if IsAdmin then Packet.Values['IsAdmin'] := 'Yes';
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMegaCastTextClient.sp_StartCast;
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code']   := 'StartCast';
    Packet.Values['SendTo'] := 'Other';
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMegaCastTextClient.sp_StopCast;
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code']   := 'StopCast';
    Packet.Values['SendTo'] := 'Other';
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMegaCastTextClient.sp_Talk(FromID, Msg: string; TextColor: TColor);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Values['Code']   := 'Talk';
    Packet.Values['FromID'] := FromID;
    Packet.Values['Msg']    := Msg;
    Packet.Values['SendTo'] := 'All';
    Packet.Integers['TextColor'] := TextColor;
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TMegaCastTextClient.sp_Whisper(FromID, UserID, Msg: string;
  TextColor: TColor);
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
    Packet.Integers['TextColor'] := TextColor;
    SendPacket(Packet);
  finally
    Packet.Free;
  end;
end;

end.
