unit TextClient;

interface

uses
  VirtualSocketUtils, VirtualSocketClient, ValueList,
  Classes, SysUtils;

type
  TTextReceivedEvent = procedure (Sender:TObject; APacket:TValueList) of object;

  TTextClient = class (TComponent)
  private
    FSocket : TVirtualSocketClient;
    procedure on_Connected(Sender:TObject);
    procedure on_Disconnected(Sender:TObject);
    procedure on_Received(Sender:TObject; AData:pointer; ASize:integer);
  private
    FPakcet : TValueList;
    FOnReceived: TTextReceivedEvent;
  public
    constructor Create(AOwner:TComponent; ASocket:TVirtualSocketClient); reintroduce;
    destructor Destroy; override;

    procedure sp_Login(AUserID,APassword:string);
  published
    property Packet : TValueList read FPakcet;
    property OnReceived : TTextReceivedEvent read FOnReceived write FOnReceived;
  end;
  
implementation

{ TTextClient }

constructor TTextClient.Create(AOwner: TComponent;
  ASocket: TVirtualSocketClient);
begin
  inherited Create(AOwner);

  FPakcet := TValueList.Create;

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnDisconnected := on_Disconnected;
  FSocket.OnReceived := on_Received;
end;

destructor TTextClient.Destroy;
begin
  FreeAndNil(FPakcet);

  inherited;
end;

procedure TTextClient.on_Connected(Sender: TObject);
begin

end;

procedure TTextClient.on_Disconnected(Sender: TObject);
begin

end;

procedure TTextClient.on_Received(Sender: TObject; AData: pointer;
  ASize: integer);
var
  Packet : TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.LoadFromBuffer(AData,  ASize);
    if Assigned(FOnReceived) then FOnReceived(Self, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TTextClient.sp_Login(AUserID, APassword: string);
begin
  Packet.Clear;
  Packet.Values['Code'] := 'Login';
  Packet.Values['Version'] := '1.0';
  Packet.Values['UserID'] := AUserID;
  Packet.Values['Password'] := APassword;

  FSocket.Send(Packet.Text);
end;

end.

