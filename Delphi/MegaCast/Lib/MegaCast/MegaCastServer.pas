unit MegaCastServer;

interface

uses
  MegaCastUtils, MegaCastServerBase, VirtualSocketServer,
  BlockBuffer, FrameSizeSync,
  Classes, SysUtils, IdTCPServer;

type
  TChannelInfoForMegaCast = class(TChannelInfo)
  strict private
    FBlockUnits : TMemoryStream;
    FBlockBuffer : TBlockBuffer;
  public
    constructor Create(AConnection:TConnection; AChannel:byte); override;
    destructor Destroy; override;

    procedure SendNow; override;

    procedure ClearBlockUnits;
    procedure SetFrameSize(AFrameSize:TFrameSize);
    procedure BlockUnitIn(ABlockUnit:pointer; AUnitSize:integer);
    procedure CopyBlockUnitsTo(AChannelInfo:TChannelInfo);
    procedure LoadFromScreenShot(AScreenShot:TScreenShot);
  end;

  TMegaCastServer = class (TMegaCastServerBase)
  private
    FFrameSize : TFrameSizeSync;
    FScreenShot : TScreenShot;

    procedure send_FrameSizeTo(AChannelInfo:TChannelInfo; AFrameSize:TFrameSize);
    procedure send_FameSizeToOther(AChannelInfo:TChannelInfo; AFrameSize:TFrameSize);
    procedure send_ScreenShotTo(AChannelInfo:TChannelInfo; AScreenShot:TScreenShot);
    procedure send_BlockUnitsToOther(AChannelInfo:TChannelInfo);
  protected
    procedure do_Connected(const AConnection:TConnection; var AChannelInfo:TChannelInfo); override;

    procedure do_NewBlockUnit(AConnection:TConnection; AChannelInfo:TChannelInfo;
      AData:pointer; ASize:integer; ABlockUnit:pointer; AUnitSize:integer); override;
    procedure do_EndFromClient(AConnection:TConnection; AChannelInfo:TChannelInfo); override;
    procedure do_NeedScreenShot(AConnection:TConnection; AChannelInfo:TChannelInfo); override;
    procedure do_FrameSize(AConnection:TConnection; AChannelInfo:TChannelInfo;
      AData:pointer; ASize:integer; AFrameSize:TFrameSize); override;
  public
    constructor Create(AOwner: TComponent; ASocket: TVirtualSocketServer); override;
    destructor Destroy; override;
  end;

implementation

{ TChannelInfoForMegaCast }

procedure TChannelInfoForMegaCast.BlockUnitIn(ABlockUnit: pointer;
  AUnitSize: integer);
begin
  FBlockUnits.Write(AUnitSize, SizeOf(AUnitSize));
  FBlockUnits.Write(ABlockUnit^, AUnitSize);
end;

procedure TChannelInfoForMegaCast.ClearBlockUnits;
begin
  FBlockUnits.Clear;
end;

procedure TChannelInfoForMegaCast.CopyBlockUnitsTo(AChannelInfo: TChannelInfo);
var
  chDest : TChannelInfoForMegaCast absolute AChannelInfo;
begin
  chDest.FBlockBuffer.LoadFromStream(FBlockUnits);
end;

constructor TChannelInfoForMegaCast.Create(AConnection: TConnection;
  AChannel: byte);
begin
  inherited;

  FBlockBuffer := TBlockBuffer.Create;
  FBlockUnits := TMemoryStream.Create;
end;

destructor TChannelInfoForMegaCast.Destroy;
begin
  FreeAndNil(FBlockBuffer);
  FreeAndNil(FBlockUnits);

  inherited;
end;

procedure TChannelInfoForMegaCast.LoadFromScreenShot(AScreenShot: TScreenShot);
var
  msData : TMemoryStream;
begin
  msData := TMemoryStream.Create;
  try
    AScreenShot.SaveToStream(msData);
    FBlockBuffer.LoadFromStream(msData);
  finally
    msData.Free;
  end;
end;

procedure TChannelInfoForMegaCast.SendNow;
var
  Data : pointer;
  UnitSize : integer;
  HeaderSizeGap : integer;
  pPacketHeader : ^TMegaCastPacketHeader;
  pBlockUnit : ^byte;
  msData : TMemoryStream;
begin
  inherited;

  HeaderSizeGap := SizeOf(TMegaCastPacketHeader) - SizeOf(TMegaCastBlockUnitHeader);

  msData := TMemoryStream.Create;
  try
    // 보낼 수 있는 양만큼만 버퍼에서 가져온다.
    FBlockBuffer.SaveToStream(msData, Connection.PacketSizeSpace);

    // 버퍼에서 가져온 데이터는 [UnitSize][BlockUnit] 이 반복되는 형태가 된다.
    msData.Position := 0;
    while msData.Position < msData.Size do begin
      msData.Read(UnitSize, SizeOf(UnitSize));
      if UnitSize <= 0 then
        raise Exception.Create('TChannelInfoForMegaCast.SendNow: Size가 0 또는 그 이하일 수 없다.');

      //  MegaCastPacket 형태로 바꾸기 위해서 사이즈를 늘린다.
      GetMem(Data, UnitSize+HeaderSizeGap);
      try
        pPacketHeader := Data;
        pPacketHeader^.Command := mccBlockUnit;

        pBlockUnit := Data;
        Inc(pBlockUnit, HeaderSizeGap);
        msData.Read(pBlockUnit^, UnitSize);

        Connection.Send(Channel, Data, UnitSize+HeaderSizeGap);
      finally
        FreeMem(Data);
      end;
    end;
  finally
    msData.Free;
  end;
end;

procedure TChannelInfoForMegaCast.SetFrameSize(AFrameSize: TFrameSize);
begin
  FBlockBuffer.SetFrameSize(AFrameSize)
end;

{ TMegaCastServer }

constructor TMegaCastServer.Create(AOwner: TComponent;
  ASocket: TVirtualSocketServer);
begin
  inherited;

  FFrameSize := TFrameSizeSync.Create;
  FFrameSize.SetFrameSize(FrameSize(_CellSize, _CellSize));

  FScreenShot := TScreenShot.Create;
end;

destructor TMegaCastServer.Destroy;
begin
  FreeAndNil(FScreenShot);

  inherited;
end;

procedure TMegaCastServer.do_Connected(const AConnection: TConnection;
  var AChannelInfo: TChannelInfo);
begin
  AChannelInfo := TChannelInfoForMegaCast.Create(AConnection, FSocket.Channel);
end;

procedure TMegaCastServer.do_EndFromClient(AConnection: TConnection;
  AChannelInfo: TChannelInfo);
var
  ChannelInfo : TChannelInfoForMegaCast absolute AChannelInfo;
begin
  send_BlockUnitsToOther(AChannelInfo);
  ChannelInfo.ClearBlockUnits;
end;

procedure TMegaCastServer.do_FrameSize(AConnection: TConnection;
  AChannelInfo: TChannelInfo; AData: pointer; ASize: integer;
  AFrameSize: TFrameSize);
var
  ChannelInfo : TChannelInfoForMegaCast absolute AChannelInfo;
begin
  // Todo : 
  ChannelInfo.ClearBlockUnits;

  FScreenShot.SetFrameSize(AFrameSize);
  send_FameSizeToOther(AChannelInfo, AFrameSize);

  FFrameSize.SetFrameSize(AFrameSize);
end;

procedure TMegaCastServer.do_NeedScreenShot(AConnection: TConnection;
  AChannelInfo: TChannelInfo);
var
  FrameSize : TFrameSize;  
begin
  FFrameSize.GetFrameSize(FrameSize);
  send_FrameSizeTo(AChannelInfo, FrameSize);
  send_ScreenShotTo(AChannelInfo, FScreenShot);
end;

procedure TMegaCastServer.do_NewBlockUnit(AConnection: TConnection;
  AChannelInfo: TChannelInfo; AData: pointer; ASize: integer;
  ABlockUnit: pointer; AUnitSize: integer);
var
  ChannelInfo : TChannelInfoForMegaCast absolute AChannelInfo;
begin
  ChannelInfo.BlockUnitIn(ABlockUnit, AUnitSize);
  FScreenShot.AddBlockUnit(ABlockUnit, AUnitSize);
end;

procedure TMegaCastServer.send_BlockUnitsToOther(AChannelInfo: TChannelInfo);
var
  List : TList;
  Loop : integer;
  Connection : TConnection;
  ChannelInfo : TChannelInfo;
begin
  List := FSocket.LockList;
  try
    for Loop := 0 to List.Count - 1 do begin
      Connection := Pointer(TIdPeerThread(List[Loop]).Data);
      if Connection = nil then Continue;
      if Connection.Logined = false then Continue;

      ChannelInfo := Connection.ChannelInfos[FSocket.Channel];
      if ChannelInfo = AChannelInfo then Continue;

      if Connection.CheckIdle then Connection.Disconnect
      else TChannelInfoForMegaCast(AChannelInfo).CopyBlockUnitsTo(ChannelInfo);
    end;
  finally
    FSocket.UnlockList;
  end;
end;

procedure TMegaCastServer.send_FrameSizeTo(AChannelInfo: TChannelInfo;
  AFrameSize: TFrameSize);
var
  Packet : TMegaCastFrameSize;
  ChannelInfo : TChannelInfoForMegaCast absolute AChannelInfo;
begin
  ChannelInfo.SetFrameSize(AFrameSize);

  Packet.Command := mccFrameSize;
  Packet.FrameSize := AFrameSize;
  ChannelInfo.Add(@Packet, SizeOf(Packet));
end;

procedure TMegaCastServer.send_FameSizeToOther(AChannelInfo: TChannelInfo;
  AFrameSize: TFrameSize);
var
  List : TList;
  Loop : integer;
  Connection : TConnection;
  ChannelInfo : TChannelInfo;
begin
  List := FSocket.LockList;
  try
    for Loop := 0 to List.Count - 1 do begin
      Connection := Pointer(TIdPeerThread(List[Loop]).Data);
      if Connection = nil then Continue;
      if Connection.Logined = false then Continue;

      ChannelInfo := Connection.ChannelInfos[FSocket.Channel];
      if ChannelInfo = AChannelInfo then Continue;

      if Connection.CheckIdle then Connection.Disconnect
      else send_FrameSizeTo(ChannelInfo, AFrameSize);
    end;
  finally
    FSocket.UnlockList;
  end;
end;

procedure TMegaCastServer.send_ScreenShotTo(AChannelInfo: TChannelInfo;
  AScreenShot:TScreenShot);
var
  ChannelInfo : TChannelInfoForMegaCast absolute AChannelInfo;
begin
  ChannelInfo.LoadFromScreenShot(AScreenShot);
end;

end.
