unit ServerUnit;

interface

uses
  Base,
  Classes, SysUtils;

type
  TVirtualSocketServerIndy9 = class(TVirtualSocketServerProvider)
  strict private
  strict protected
    procedure on_Connected(AChannel:byte; AConnection:TConnection); override;
    procedure on_Disconnected(AChannel:byte; AConnection:TConnection); override;
    procedure on_Received(AChannel:byte; AConnection:TConnection; AData:pointer; ASize:integer); override;
  public
    procedure Start; override;
    procedure Stop; override;

    function LockList:TList; override;
    procedure UnlockList; override;

    function FindConnectionByUserID(AList:TList; AUserID:string):TConnection; override;

    procedure SendTo(AConnection:TConnection; AData:pointer; ASize:integer); override;
    procedure SendToUserID(AUserID:string; AData:pointer; ASize:integer); override;
    procedure SendToAll(AData:Pointer; ASize:integer); override;
    procedure SendToOther(AConnection:TConnection; AData:Pointer; ASize:integer); override;
  end;

  TTextChannelInfo = class(TChannelInfo)
  private
  public
    procedure Clear; override;
    procedure Add(AData:pointer; ASize:integer); override;
    procedure SendNow; override;
  end;

  TCamChannelInfo = class(TChannelInfo)
  private
  public
    procedure Clear; override;
    procedure Add(AData:pointer; ASize:integer); override;
    procedure SendNow; override;
  end;

  TDeskCamChannelInfo = class(TChannelInfo)
  private
  public
    procedure Clear; override;
    procedure Add(AData:pointer; ASize:integer); override;
    procedure SendNow; override;
  end;

implementation

{ TVirtualSocketServerIndy9 }

function TVirtualSocketServerIndy9.FindConnectionByUserID(AList: TList;
  AUserID: string): TConnection;
begin

end;

function TVirtualSocketServerIndy9.LockList: TList;
begin

end;

procedure TVirtualSocketServerIndy9.on_Connected(AChannel: byte;
  AConnection: TConnection);
begin

end;

procedure TVirtualSocketServerIndy9.on_Disconnected(AChannel: byte;
  AConnection: TConnection);
begin

end;

procedure TVirtualSocketServerIndy9.on_Received(AChannel: byte;
  AConnection: TConnection; AData: pointer; ASize: integer);
begin

end;

procedure TVirtualSocketServerIndy9.SendTo(AConnection: TConnection;
  AData: pointer; ASize: integer);
begin

end;

procedure TVirtualSocketServerIndy9.SendToAll(AData: Pointer; ASize: integer);
begin

end;

procedure TVirtualSocketServerIndy9.SendToOther(AConnection: TConnection;
  AData: Pointer; ASize: integer);
begin

end;

procedure TVirtualSocketServerIndy9.SendToUserID(AUserID: string;
  AData: pointer; ASize: integer);
begin

end;

procedure TVirtualSocketServerIndy9.Start;
begin

end;

procedure TVirtualSocketServerIndy9.Stop;
begin

end;

procedure TVirtualSocketServerIndy9.UnlockList;
begin

end;

{ TTextChannelInfo }

procedure TTextChannelInfo.Add(AData: pointer; ASize: integer);
begin

end;

procedure TTextChannelInfo.Clear;
begin

end;

procedure TTextChannelInfo.SendNow;
begin

end;

{ TCamChannelInfo }

procedure TCamChannelInfo.Add(AData: pointer; ASize: integer);
begin

end;

procedure TCamChannelInfo.Clear;
begin

end;

procedure TCamChannelInfo.SendNow;
begin

end;

{ TDeskCamChannelInfo }

procedure TDeskCamChannelInfo.Add(AData: pointer; ASize: integer);
begin

end;

procedure TDeskCamChannelInfo.Clear;
begin

end;

procedure TDeskCamChannelInfo.SendNow;
begin

end;

end.
