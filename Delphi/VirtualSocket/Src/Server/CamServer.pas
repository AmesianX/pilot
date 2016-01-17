unit CamServer;

interface

uses
  VirtualSocketUtils, VirtualSocketServer, SyncStream, MixedPacket,
  Classes, SysUtils;

type
  TChannelInfoForCam = class (TChannelInfo)
  strict private
    FCamData : TSyncStream;
  public
    CamID : ShortString;

    constructor Create(AConnection:TConnection; AChannel:byte); override;
    destructor Destroy; override;

    procedure AddCamData(AData:pointer; ASize:integer);
    procedure ReadCamData(var AData:pointer; var ASize:integer);
    procedure SendMixedPacket(AMixedPacket:TMixedPacket);
  end;

  TCamServer = class (TComponent)
  private
    FSocket : TVirtualSocketServer;

    procedure do_SendCam(ACamID:string; AChannelInfo:TChannelInfoForCam; AUserID:string);

    procedure on_Connected(Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_BeforeDisconnected(Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_AfterDisconnected(Sender:TObject; AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_Received(Sender:TObject; AConnection:TConnection; AChannelInfo:TChannelInfo; AData:pointer; ASize:integer);
  public
    constructor Create(AOwner:TComponent; ASocket:TVirtualSocketServer); reintroduce;
    destructor Destroy; override;
  end;  

implementation

{ TChannelInfoForCam }

procedure TChannelInfoForCam.AddCamData(AData: pointer; ASize: integer);
begin
  FCamData.Write(AData, ASize);
end;

constructor TChannelInfoForCam.Create(AConnection: TConnection; AChannel: byte);
begin
  inherited;
  
  FCamData := TSyncStream.Create;
end;

destructor TChannelInfoForCam.Destroy;
begin
  FCamData.Free;

  inherited;
end;

procedure TChannelInfoForCam.ReadCamData(var AData: pointer;
  var ASize: integer);
begin
  FCamData.Read(AData, ASize);
end;

procedure TChannelInfoForCam.SendMixedPacket(AMixedPacket: TMixedPacket);
var
  Data : pointer;
  Size : integer;
begin
  if AMixedPacket.GetData(Data, Size) then begin
    try
      Add(Data, Size);
    finally
      FreeMem(Data);
    end;
  end;
end;

{ TCamServer }

constructor TCamServer.Create(AOwner: TComponent; ASocket: TVirtualSocketServer);
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnBeforeDisconnected := on_BeforeDisconnected;
  FSocket.OnAfterDisconnected := on_AfterDisconnected;
  FSocket.OnReceived := on_Received;
end;

destructor TCamServer.Destroy;
begin

  inherited;
end;

procedure TCamServer.do_SendCam(ACamID:string; AChannelInfo: TChannelInfoForCam; AUserID: string);
var
  List : TList;
  Data : pointer;
  Size : integer;
  MixedPacket : TMixedPacket;
  Connection : TConnection;
  ChannelInfo : TChannelInfoForCam;
begin
  List := FSocket.LockList;
  try
    Connection := FSocket.FindConnectionByUserID(List, AUserID);
    if Connection = nil then Exit;

    ChannelInfo := TChannelInfoForCam(Connection.ChannelInfos[FSocket.Channel]);
    if ChannelInfo = nil then
      raise Exception.Create('TCamServer.do_SendCam: ChannelInfo 정보 저장에 문제 발생');

    // 이미 받았던 이미지
    if ACamID = ChannelInfo.CamID then Exit;    

    ChannelInfo.ReadCamData(Data, Size);
    if Size > 0 then begin
      MixedPacket := TMixedPacket.Create;
      try
        MixedPacket.AddString(ChannelInfo.CamID);
        MixedPacket.AddString(AUserID);
        MixedPacket.AddData(Data, Size);

        AChannelInfo.SendMixedPacket(MixedPacket);
      finally
        FreeMem(Data);
        MixedPacket.Free;
      end;
    end;
  finally
    FSocket.UnlockList;
  end;
end;

procedure TCamServer.on_AfterDisconnected(Sender: TObject;
  AConnection: TConnection; var AChannelInfo: TChannelInfo);
begin
  FreeAndNil(AChannelInfo);
end;

procedure TCamServer.on_BeforeDisconnected(Sender: TObject;
  AConnection: TConnection; var AChannelInfo: TChannelInfo);
begin

end;

procedure TCamServer.on_Connected(Sender: TObject; AConnection: TConnection;
  var AChannelInfo: TChannelInfo);
begin
  AChannelInfo := TChannelInfoForCam.Create(AConnection, FSocket.Channel);
end;

procedure TCamServer.on_Received(Sender: TObject; AConnection: TConnection;
  AChannelInfo: TChannelInfo; AData: pointer; ASize: integer);
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
    MixedPacket.GetPacket(_CamIDPart, CamID);

    if MixedPacket.GetPacket(_UserIDPart, UserID) then do_SendCam(CamID, TChannelInfoForCam(AChannelInfo), UserID);

    if MixedPacket.GetPacket(_CamDataPart, Data, Size) then begin
      TChannelInfoForCam(AChannelInfo).CamID := CamID;

      try
        TChannelInfoForCam(AChannelInfo).AddCamData(Data, Size);
      finally
        FreeMem(Data);
      end;
    end;
  finally
    MixedPacket.Free;
  end;
end;

end.

