unit RoomUnit;

interface

uses
  MegaCastUtils, VirtualSocketServer, VirtualSocketServerProviderIndy9,
  MasterVoiceServer, MagaCastTextServer, MegaCastServer,
  Classes, SysUtils;

type
  TRoomUnit = class (TComponent)
  private
    FServerProvider: TVirtualSocketServerProvider;
    FVoiceServer: TMasterVoiceServer;
    FTextServer: TMagaCastTextServer;
    FMegaCastServer : TMegaCastServer;
  private
    FRoomNo: Integer;
    procedure SetRoomNo(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property RoomNo : integer read FRoomNo write SetRoomNo;
  end;

implementation

{ TRoomUnit }

constructor TRoomUnit.Create(AOwner: TComponent);
begin
  inherited;

  FServerProvider := TVirtualSocketServerProviderIndy9.Create(Self);
  FVoiceServer := TMasterVoiceServer.Create(Self, FServerProvider.CreateSocket);
  FTextServer := TMagaCastTextServer.Create(Self, FServerProvider.CreateSocket);
  FMegaCastServer := TMegaCastServer.Create(Self, FServerProvider.CreateSocket);
end;

destructor TRoomUnit.Destroy;
begin
  Stop;

  FreeAndNil(FMegaCastServer);
  FreeAndNil(FTextServer);
  FreeAndNil(FVoiceServer);
  FreeAndNil(FServerProvider);

  inherited;
end;

procedure TRoomUnit.SetRoomNo(const Value: integer);
begin
  FRoomNo := Value;
  FServerProvider.Port := FRoomNo + _MagaCastPortOffset;
end;

procedure TRoomUnit.Start;
begin
  FServerProvider.Start;
end;

procedure TRoomUnit.Stop;
begin
  FServerProvider.Stop;
end;

end.
