unit MegaCastMasterVoiceClient;

interface

uses
  ValueList, VirtualSocketClient, TextClient, MasterVoiceClient,
  Classes, SysUtils;

type
  TMegaCastMasterVoiceClient = class(TMasterVoiceClient)
  private
  public
    constructor Create(AOwner: TComponent; ASocket: TVirtualSocketClient); override;
    destructor Destroy; override;

    procedure ExecutePacket(APacket:TValueList);
  published
    procedure rp_OkLogin(APacket:TValueList);
  end;

implementation

{ TMegaCastMasterVoiceClient }

constructor TMegaCastMasterVoiceClient.Create(AOwner: TComponent;
  ASocket: TVirtualSocketClient);
begin
  inherited;

  Mute := true;
  Player.Capacity := 200;
end;

destructor TMegaCastMasterVoiceClient.Destroy;
begin

  inherited;
end;

procedure TMegaCastMasterVoiceClient.ExecutePacket(APacket: TValueList);
var
  Proc : procedure (APacket:TValueList) of object;
begin
  TMethod(Proc).Data := Self;
  TMethod(Proc).Code := Self.MethodAddress('rp_' + APacket.Values['Code']);
  if Assigned(Proc) then Proc(APacket);
end;

procedure TMegaCastMasterVoiceClient.rp_OkLogin(APacket: TValueList);
begin
  Mute := false;
end;

end.
