unit MegaCastServerBase;

interface

uses
  MegaCastUtils, VirtualSocketServer, 
  Windows, Classes, SysUtils, TypInfo, IdTCPServer;

type
  TMegaCastServerBase = class (TComponent)
  private
    procedure on_Connected(Sender: TObject; const AConnection:TConnection; var AChannelInfo:TChannelInfo);
    procedure on_BeforeDisconnected(Sender:TObject; const AConnection:TConnection; const AChannelInfo:TChannelInfo);
    procedure on_AfterDisconnected(Sender:TObject; const AConnection:TConnection; const AChannelInfo:TChannelInfo);
    procedure on_Received(Sender:TObject; const AConnection:TConnection; const AChannelInfo:TChannelInfo; const AData:pointer; const ASize:integer);

    procedure send_NeedFromServer(AChannelInfo:TChannelInfo);
  protected
    FSocket: TVirtualSocketServer;

    procedure do_Connected(const AConnection:TConnection; var AChannelInfo:TChannelInfo); virtual; abstract;

    procedure do_NewBlockUnit(AConnection:TConnection; AChannelInfo:TChannelInfo;
      AData:pointer; ASize:integer; BlockUnit:pointer; UnitSize:integer); virtual; abstract;
    procedure do_EndFromClient(AConnection:TConnection; AChannelInfo:TChannelInfo); virtual; abstract;
    procedure do_NeedScreenShot(AConnection:TConnection; AChannelInfo:TChannelInfo); virtual; abstract;
    procedure do_FrameSize(AConnection:TConnection; AChannelInfo:TChannelInfo;
      AData:pointer; ASize:integer; AFrameSize:TFrameSize); virtual; abstract;
  public
    constructor Create(AOwner: TComponent; ASocket: TVirtualSocketServer); reintroduce; virtual;
    destructor Destroy; override;
  end;

implementation

{ TMegaCastServerBase }

constructor TMegaCastServerBase.Create(AOwner: TComponent;
  ASocket: TVirtualSocketServer);
begin
  inherited Create(AOwner);

  FSocket := ASocket;
  FSocket.OnConnected := on_Connected;
  FSocket.OnBeforeDisconnected := on_BeforeDisconnected;
  FSocket.OnAfterDisconnected := on_AfterDisconnected;
  FSocket.OnReceived := on_Received;
end;

destructor TMegaCastServerBase.Destroy;
begin

  inherited;
end;

procedure TMegaCastServerBase.on_AfterDisconnected(Sender: TObject;
  const AConnection: TConnection; const AChannelInfo: TChannelInfo);
begin

end;

procedure TMegaCastServerBase.on_BeforeDisconnected(Sender: TObject;
  const AConnection: TConnection; const AChannelInfo: TChannelInfo);
begin

end;

procedure TMegaCastServerBase.on_Connected(Sender: TObject; const AConnection: TConnection;
  var AChannelInfo: TChannelInfo);
begin
  do_Connected(AConnection, AChannelInfo);
  send_NeedFromServer(AChannelInfo);
end;

procedure TMegaCastServerBase.on_Received(Sender: TObject; const AConnection: TConnection;
  const AChannelInfo: TChannelInfo; const AData: Pointer; const ASize: Integer);
var
  BlockUnit : pointer;
  UnitSize : integer;
  pPacketHeader : ^TMegaCastPacketHeader;
  pFrameSize : ^TMegaCastFrameSize;
begin
  pPacketHeader := AData;

  {$IFDEF DEBUG}
//    OutputDebugString(PChar(Format(
//      'TMegaCastServerBase.on_Received: Command=%s',
//      [GetEnumName(TypeInfo(TMegaCastCommand), Integer(pPacketHeader^.Command))]
//    )));
  {$ENDIF}

  case pPacketHeader^.Command of
    mccBlockUnit: begin
      MegaCastPacketToBlockUnit(AData, ASize, BlockUnit, UnitSize);
      do_NewBlockUnit(AConnection, AChannelInfo, AData, ASize, BlockUnit, UnitSize);
    end;

    mccEndFromClient: begin
      do_EndFromClient(AConnection, AChannelInfo);
      send_NeedFromServer(AChannelInfo);
    end;

    mccNeedScreenShot: begin
      do_NeedScreenShot(AConnection, AChannelInfo);
    end;

    mccFrameSize: begin
      pFrameSize := AData;
      do_FrameSize(AConnection, AChannelInfo, AData, ASize, pFrameSize^.FrameSize);
    end;
  end;
end;

procedure TMegaCastServerBase.send_NeedFromServer(AChannelInfo: TChannelInfo);
var
  Command : TMegaCastCommand;
begin
  Command := mccNeedFromServer;
  AChannelInfo.Add(@Command, SizeOf(Command));
end;

end.
