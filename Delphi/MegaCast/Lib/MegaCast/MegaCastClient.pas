unit MegaCastClient;

interface

uses
  MegaCastUtils, VirtualSocketClient,
  Classes, SysUtils;

type
  TMegaCastClient = class(TComponent)
  private
    FSocket : TVirtualSocketClient;
    procedure on_Received(Sender:TObject; AData:pointer; ASize:integer);
  private
    FOnNeedFromServer: TNotifyEvent;
    FOnFrameSizeChanged: TFrameSizeChangedEvent;
    FOnNewBlockUnit: TNewBlockUnitEvent;
    function GetOnConnected: TNotifyEvent;
    function GetOnDisconnected: TNotifyEvent;
    procedure SetOnConnected(const Value: TNotifyEvent);
    procedure SetOnDisconnected(const Value: TNotifyEvent);
  protected
  public
    constructor Create(AOwner: TComponent; ASocket: TVirtualSocketClient); reintroduce; virtual;
    destructor Destroy; override;

    procedure SendBlockUnit(ABlockUnit:pointer; AUnitSize:integer);
    procedure SendEndOfData;
    procedure SendNeedScreenShot;
    procedure SendFrameSize(AFrameSize:TFrameSize);
  published
    property OnConnected : TNotifyEvent read GetOnConnected write SetOnConnected;
    property OnDisconnected : TNotifyEvent read GetOnDisconnected write SetOnDisconnected;
    property OnNeedFromServer : TNotifyEvent read FOnNeedFromServer write FOnNeedFromServer;
    property OnNewBlockUnit : TNewBlockUnitEvent read FOnNewBlockUnit write FOnNewBlockUnit;
    property OnFrameSizeChanged : TFrameSizeChangedEvent read FOnFrameSizeChanged write FOnFrameSizeChanged;
  end;

implementation

{ TMegaCastClient }

constructor TMegaCastClient.Create(AOwner: TComponent;
  ASocket: TVirtualSocketClient);
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnReceived := on_Received;
end;

destructor TMegaCastClient.Destroy;
begin

  inherited;
end;

procedure TMegaCastClient.on_Received(Sender: TObject; AData: pointer;
  ASize: integer);
var
  BlockUnit : pointer;
  UnitSize : integer;
  pHeader : ^TMegaCastPacketHeader;
  pFrameSize : ^TMegaCastFrameSize;
begin
  pHeader := AData;

  case pHeader^.Command of
    mccNeedFromServer: begin
      if Assigned(FOnNeedFromServer) then FOnNeedFromServer(Self);      
    end;

    mccBlockUnit: begin
      MegaCastPacketToBlockUnit(AData, ASize, BlockUnit, UnitSize);
      if Assigned(FOnNewBlockUnit) then FOnNewBlockUnit(Self, BlockUnit, UnitSize);
    end;

    mccFrameSize: begin
      pFrameSize := AData;
      if Assigned(FOnFrameSizeChanged) then FOnFrameSizeChanged(Self, pFrameSize^.FrameSize);
    end;
  end;
end;

function TMegaCastClient.GetOnConnected: TNotifyEvent;
begin
  Result := FSocket.OnConnected;
end;

function TMegaCastClient.GetOnDisconnected: TNotifyEvent;
begin
  Result := FSocket.OnDisconnected;
end;

procedure TMegaCastClient.SendBlockUnit(ABlockUnit: pointer; AUnitSize: integer);
var
  Data : pointer;
  Size : integer;
begin
  BlockUnitToMegaCastPacket(ABlockUnit, AUnitSize, Data, Size);
  try
    FSocket.Send(Data, Size);
  finally
    if Data <> nil then FreeMem(Data);    
  end;
end;

procedure TMegaCastClient.SendEndOfData;
var
  Command : TMegaCastCommand;
begin
  Command := mccEndFromClient;
  FSocket.Send(@Command, SizeOf(Command));
end;

procedure TMegaCastClient.SendFrameSize(AFrameSize: TFrameSize);
var
  Packet : TMegaCastFrameSize;
begin
  Packet.Command := mccFrameSize;
  Packet.FrameSize := AFrameSize;
  FSocket.Send(@Packet, SizeOf(Packet));
end;

procedure TMegaCastClient.SendNeedScreenShot;
var
  Command : TMegaCastCommand;
begin
  Command := mccNeedScreenShot;
  FSocket.Send(@Command, SizeOf(Command));
end;

procedure TMegaCastClient.SetOnConnected(const Value: TNotifyEvent);
begin
  FSocket.OnConnected := Value;
end;

procedure TMegaCastClient.SetOnDisconnected(const Value: TNotifyEvent);
begin
  FSocket.OnDisconnected := Value;
end;

end.
