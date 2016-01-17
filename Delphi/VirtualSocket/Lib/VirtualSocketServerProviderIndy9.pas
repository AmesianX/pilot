unit VirtualSocketServerProviderIndy9;

interface

uses
  VirtualSocketUtils, VirtualSocketServer, ValueList, PacketBuffer, SpinLock,
  Windows, Classes, SysUtils, ExtCtrls,
  IdTCPServer, IdComponent, IdTCPConnection, IdThreadMgr, IdThreadMgrDefault;

type
  TVirtualSocketServerProviderIndy9 = class(TVirtualSocketServerProvider)
  private
    FTimer : TTimer;
    FSocket : TIdTCPServer;

    procedure do_NeedDataFromClient(AConnection:TConnection);

    procedure on_Timer(Sender:TObject);
    procedure on_IdTCPServerConnect(AThread: TIdPeerThread);
    procedure on_IdTCPServerDisconnect(AThread: TIdPeerThread);
    procedure on_IdTCPServerExecute(AThread: TIdPeerThread);
  protected
    function do_LockList:TList; override;
    procedure do_UnlockList; override;
    function do_FindConnectionByUserID(AList:TList; AUserID:string):TConnection; override;

    function GetPort: integer; override;
    procedure SetPort(const Value: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override; 
    procedure Stop; override;
  end;
  
implementation

{ TVirtualSocketServerProviderIndy9 }

constructor TVirtualSocketServerProviderIndy9.Create(AOwner: TComponent);
begin
  inherited;

  FSocket := TIdTCPServer.Create(Self);
  FSocket.ThreadMgr :=    TIdThreadMgrDefault.Create(Self);
  FSocket.TerminateWaitTime := 5000;
  FSocket.OnConnect :=    on_IdTCPServerConnect;
  FSocket.OnDisconnect := on_IdTCPServerDisconnect;
  FSocket.OnExecute :=    on_IdTCPServerExecute;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1000;
  FTimer.OnTimer := on_Timer;
  FTimer.Enabled := true;
end;

destructor TVirtualSocketServerProviderIndy9.Destroy;
begin
  Stop;

  FTimer.Free;
  FSocket.Free;

  inherited;
end;

function TVirtualSocketServerProviderIndy9.do_FindConnectionByUserID(
  AList: TList; AUserID: string): TConnection;
var
  Loop : Integer;
  Connection : TConnection;
begin
  Result := nil;
  
  for Loop := AList.Count-1 downto 0 do begin
    Connection := Pointer(TIdPeerThread(AList[Loop]).Data);
    if Connection = nil then Continue;
    if Connection.Logined = false then Continue;

    if UpperCase(Connection.UserID) = UpperCase(AUserID) then begin
      Result := Connection;
      Exit;
    end;
  end;
end;

function TVirtualSocketServerProviderIndy9.do_LockList: TList;
begin
  Result := FSocket.Threads.LockList;
end;

procedure TVirtualSocketServerProviderIndy9.do_NeedDataFromClient(
  AConnection: TConnection);
var
  Loop : Integer;
begin
  AConnection.ClearIdleCount;
  for Loop := 0 to FSocketList.Count - 1 do AConnection.ChannelInfos[Loop].SendNow;
  AConnection.Send(_EndOfDataFromServer, nil, 0);
end;

procedure TVirtualSocketServerProviderIndy9.do_UnlockList;
begin
  FSocket.Threads.UnlockList;
end;

function TVirtualSocketServerProviderIndy9.GetPort: integer;
begin
  Result := FSocket.DefaultPort;
end;

procedure TVirtualSocketServerProviderIndy9.on_IdTCPServerConnect(
  AThread: TIdPeerThread);
var
  Connection : TConnection;
begin
  Connection := TConnection.Create;
  AThread.Data := Connection;
  Connection.PeerThread := AThread;

  on_Connected(Connection);
end;

procedure TVirtualSocketServerProviderIndy9.on_IdTCPServerDisconnect(
  AThread: TIdPeerThread);
var
  Connection : TConnection;
begin
  Connection := Pointer(AThread.Data);
  AThread.Data := nil;
  Connection.Logined := false;

  on_Disconnected(Connection);

  Connection.Free;
end;

procedure TVirtualSocketServerProviderIndy9.on_IdTCPServerExecute(
  AThread: TIdPeerThread);
var
  Channel : byte;
  Data : pointer;
  Size : integer;
  Pakcet : TValueList;
  Connection : TConnection;
begin
  Connection := Pointer(AThread.Data);
  if Connection = nil then Exit;

  try
    AThread.Connection.ReadBuffer(Channel, SizeOf(Channel));
    AThread.Connection.ReadBuffer(Size, SizeOf(Size));

    if (Size < 0) or (Size > _PacketSizeLimit) then
      raise Exception.Create('Packet의 크기가 범위를 벗어났습니다.');

    if Size > 0 then begin
      GetMem(Data, Size);
      Pakcet := TValueList.Create;
      try
        AThread.Connection.ReadBuffer(Data^, Size);
        Pakcet.LoadFromBuffer(Data, Size);

        on_Received(Channel, Connection, Data, Size);
      finally
        FreeMem(Data);
        Pakcet.Free;
      end;
    end;

    case Channel of
      _NeedDataFromClient: do_NeedDataFromClient(Connection);
    end;
  except
    on E : Exception do begin
      {$IFDEF DEBUG}
        OutputDebugString(PChar('TVirtualSocketServerProviderIndy9.on_IdTCPServerExecute: '+E.Message));
      {$ENDIF}
      Connection.Disconnect;
    end;
  end;
end;

procedure TVirtualSocketServerProviderIndy9.on_Timer(Sender: TObject);
var
  Loop : integer;
  Connection : TConnection;
  List : TList;
begin
  FTimer.Enabled := false;
  List := FSocket.Threads.LockList;
  try
    for Loop := List.Count-1 downto 0 do begin
      Connection := Pointer(TIdPeerThread(List[Loop]).Data);
      if Connection = nil then Continue;      

      if Connection.CheckIdle then Connection.Disconnect;
    end;
  finally
    FSocket.Threads.UnlockList;
    FTimer.Enabled := true;
  end;
end;

procedure TVirtualSocketServerProviderIndy9.SetPort(const Value: integer);
begin
  FSocket.DefaultPort := Value;
end;

procedure TVirtualSocketServerProviderIndy9.Start;
begin
  FSocket.Active := true;
end;

procedure TVirtualSocketServerProviderIndy9.Stop;
begin
  FSocket.Active := false;
end;

end.
