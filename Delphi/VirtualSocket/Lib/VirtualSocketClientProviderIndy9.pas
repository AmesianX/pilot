unit VirtualSocketClientProviderIndy9;

interface

uses
  VirtualSocketUtils, VirtualSocketClient, ThreadRepeater, ValueList, SpinLock,
  Windows, Messages, Classes, SysUtils, ExtCtrls,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

const
  WM_SocketDisconnected = WM_User + 1;

type
  TVirtualSocketClientProviderIndy9 = class(TVirtualSocketClientProvider)
  private
    FDisconnectedEventFired : boolean;
    FCSSend : TSpinLock;
    FSocket : TIdTCPClient;
    FThreadRcv : TThreadRepeater;
    FIdleCount : integer;
    FIdleCheckTimer : TTimer;
    procedure do_Disconnect;
    procedure on_IdleCheck(Sender:TObject);
    procedure do_RcvExecute(Sender:TObject);
    procedure on_SocketDisconnected(Sender:TObject);
    procedure do_SocketDisconnected(var msg:TMessage); message WM_SocketDisconnected;
  protected
    procedure do_Send(AChannel:byte; AData:pointer; ASize:integer); override;

    function GetConnected: boolean; override;
    function GetHost: string; override;
    function GetPort: integer; override;
    procedure SetHost(const Value: string); override;
    procedure SetPort(const Value: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Connect:boolean; override;
    procedure Disconnect; override; 
  end;

implementation

{ TVirtualSocketClientProviderIndy9 }

function TVirtualSocketClientProviderIndy9.Connect: boolean;
begin
  FIdleCount := 0;

  try
    FSocket.Connect;
  except
  end;

  Result := FSocket.Connected;
  if Result then begin
    FDisconnectedEventFired := false;
    on_Connected;

    FThreadRcv.Execute(do_RcvExecute);
    FIdleCheckTimer.OnTimer := on_IdleCheck;

    do_Send(_NeedDataFromClient, nil, 0);
  end;
end;

constructor TVirtualSocketClientProviderIndy9.Create(AOwner: TComponent);
begin
  inherited;

  FDisconnectedEventFired := true;

  FCSSend := TSpinLock.Create;

  FSocket := TIdTCPClient.Create(Self);
  FSocket.OnDisconnected := on_SocketDisconnected;

  FThreadRcv := TThreadRepeater.Create(Self);
  FThreadRcv.TimeOut := 250;

  FIdleCheckTimer := TTimer.Create(Self);
  FIdleCheckTimer.Interval := 1000;
  FIdleCheckTimer.Enabled := true;
  FIdleCheckTimer.OnTimer := nil;
end;

destructor TVirtualSocketClientProviderIndy9.Destroy;
begin
  Disconnect;

//  FreeAndNil(FCSSend);
//  FreeAndNil(FSocket);
//  FreeAndNil(FIdleCheckTimer);
//  FreeAndNil(FThreadRcv);

  inherited;
end;

procedure TVirtualSocketClientProviderIndy9.Disconnect;
begin
  FThreadRcv.Stop;

  FSocket.OnDisconnected := nil;
  try
    FSocket.Disconnect;
  finally
    FSocket.OnDisconnected := on_SocketDisconnected;
  end;
end;

procedure TVirtualSocketClientProviderIndy9.do_Disconnect;
begin
  FIdleCheckTimer.OnTimer := nil;

  Disconnect;

  // 이벤트가 중복 발생하지 않도록
  if not FDisconnectedEventFired then begin
    FDisconnectedEventFired := true;
    on_Disconnected;
  end;
end;

procedure TVirtualSocketClientProviderIndy9.do_RcvExecute(Sender: TObject);
var
  Data : pointer;
  Size : integer;
  Channel : byte;
begin
  try
    FSocket.ReadBuffer(Channel, SizeOf(Channel));
    FSocket.ReadBuffer(Size, SizeOf(Size));

    if Channel = _EndOfDataFromServer then begin
      FIdleCount := 0;
      do_Send(_NeedDataFromClient, nil, 0);
      Sleep(Interval);
      Exit;
    end;

    if (Size < 0) or (Size > _PacketSizeLimit) then
      raise Exception.Create('TEasyClient.Send: 패킷 크기가 제한을 넘었습니다.');

    GetMem(Data, Size);
    try
      FSocket.ReadBuffer(Data^, Size);
      on_Received(Channel, Data, Size);
    finally
      FreeMem(Data);
    end;
  except
    PostMessage(Handle, WM_SocketDisconnected, 0, 0);
    FThreadRcv.Terminate;
  end;
end;

procedure TVirtualSocketClientProviderIndy9.do_Send(AChannel: byte;
  AData: pointer; ASize: integer);
begin
  if (ASize < 0) or (ASize > _PacketSizeLimit) then
    raise Exception.Create('TEasyClient.Send: 패킷 크기가 제한을 넘었습니다.');

  FCSSend.Enter;
  try
    if not FSocket.Connected then Exit;

    try
      FSocket.WriteBuffer(AChannel, SizeOf(AChannel));
      FSocket.WriteBuffer(ASize, SizeOf(ASize));
      if ASize > 0 then FSocket.WriteBuffer(AData^, ASize);
    except
      PostMessage(Handle, WM_SocketDisconnected, 0, 0);
    end;
  finally
    FCSSend.Leave;
  end;
end;

procedure TVirtualSocketClientProviderIndy9.do_SocketDisconnected(
  var msg: TMessage);
begin
  do_Disconnect;
end;

function TVirtualSocketClientProviderIndy9.GetConnected: boolean;
begin
  Result := FSocket.Connected;
end;

function TVirtualSocketClientProviderIndy9.GetHost: string;
begin
  Result := FSocket.Host;
end;

function TVirtualSocketClientProviderIndy9.GetPort: integer;
begin
  Result := FSocket.Port;
end;

procedure TVirtualSocketClientProviderIndy9.on_IdleCheck(Sender: TObject);
begin
  FIdleCheckTimer.Enabled := false;
  try
    FIdleCount := FIdleCount + 1;
    if (FIdleCount * 1000) > _IdleCountLimit then do_Disconnect;
  finally
    FIdleCheckTimer.Enabled := true;
  end;
end;

procedure TVirtualSocketClientProviderIndy9.on_SocketDisconnected(
  Sender: TObject);
begin
  PostMessage(Handle, WM_SocketDisconnected, 0, 0);
end;

procedure TVirtualSocketClientProviderIndy9.SetHost(const Value: string);
begin
  FSocket.Host := Value;
end;

procedure TVirtualSocketClientProviderIndy9.SetPort(const Value: integer);
begin
  FSocket.Port := Value;
end;

end.
