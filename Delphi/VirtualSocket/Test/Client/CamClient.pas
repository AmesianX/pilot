unit CamClient;

interface

uses
  VirtualSocketUtils, VirtualSocketClient, MixedPacket,
  Windows, Classes, SysUtils;

type
  TCamReceivedEvent = procedure (Sender:TObject; ACamID,AUserID:string; AData:pointer; ASize:integer) of object;

  TCamClient = class (TComponent)
  private
    FSocket : TVirtualSocketClient;
    procedure on_Connected(Sender:TObject);
    procedure on_Disconnected(Sender:TObject);
    procedure on_Received(Sender:TObject; AData:pointer; ASize:integer);
  private
    FOnReceived: TCamReceivedEvent;
  public
    constructor Create(AOwner:TComponent; ASocket:TVirtualSocketClient); reintroduce;
    destructor Destroy; override;

    procedure SendCam(AUserID:string; AData:pointer; ASize:integer); overload;
    procedure SendCam(AUserID:string; AStream:TStream); overload;

    procedure NeedCam(AUserID : String);
  published
    property OnReceived : TCamReceivedEvent read FOnReceived write FOnReceived;
  end;
  
implementation

{ TCamClient }

constructor TCamClient.Create(AOwner: TComponent;
  ASocket: TVirtualSocketClient);
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnDisconnected := on_Disconnected;
  FSocket.OnReceived := on_Received;
end;

destructor TCamClient.Destroy;
begin

  inherited;
end;

procedure TCamClient.on_Connected(Sender: TObject);
begin

end;

procedure TCamClient.on_Disconnected(Sender: TObject);
begin

end;

procedure TCamClient.on_Received(Sender: TObject; AData: pointer;
  ASize: integer);
const
  _CamIDPart = 0;
  _UserIDPart = 1;
  _CamDataPart = 2;
var
  CamID : string;
  UserID : string;
  Data : pointer;
  Size : integer;
  MixedPacket : TMixedPacket;
begin
  MixedPacket := TMixedPacket.Create;
  try
    MixedPacket.SetData(AData, ASize);

    // 같은 이미지를 중복해서 보내는 일이 없도록 이미지마다 독립적인 ID를 갖도록 한다.
    if not MixedPacket.GetPacket(_CamIDPart, CamID) then Exit;
    
    if not MixedPacket.GetPacket(_UserIDPart, UserID) then Exit;
    
    if MixedPacket.GetPacket(_CamDataPart, Data, Size) then begin
      try
        if Assigned(FOnReceived) then FOnReceived(Self, CamID, UserID, Data, Size);         
      finally
        FreeMem(Data);
      end;
    end;
  finally
    MixedPacket.Free;
  end;
end;

procedure TCamClient.SendCam(AUserID: string; AData: pointer; ASize: integer);
var
  MixedPacket : TMixedPacket;
begin
  MixedPacket := TMixedPacket.Create;
  try
    MixedPacket.AddString(IntToStr(GetTickCount));
    MixedPacket.AddString(AUserID);
    MixedPacket.AddData(AData, ASize);
  finally
    MixedPacket.Free;
  end;
end;

procedure TCamClient.SendCam(AUserID: string; AStream: TStream);
var
  Data : pointer;
  Size : Integer;
  MixedPacket : TMixedPacket;
begin
  MixedPacket := TMixedPacket.Create;
  try
    MixedPacket.AddString(IntToStr(GetTickCount));
    MixedPacket.AddString(AUserID);
    MixedPacket.AddStream(AStream);
    MixedPacket.GetData(Data, Size);
    try
      FSocket.Send(Data, Size);
    finally
      FreeMem(Data);
    end;
  finally
    MixedPacket.Free;
  end;
end;

procedure TCamClient.NeedCam(AUserID: String);
var
  Data : pointer;
  Size : Integer;
  MixedPacket : TMixedPacket;
begin
  MixedPacket := TMixedPacket.Create;
  try
    MixedPacket.AddString('');
    MixedPacket.AddString(AUserID);
    MixedPacket.AddData(Nil, 0);

    MixedPacket.GetData(Data, Size);
    try
      FSocket.Send(Data, Size);
    finally
      FreeMem(Data);
    end;
  finally
    MixedPacket.Free;
  end;
end;

end.
