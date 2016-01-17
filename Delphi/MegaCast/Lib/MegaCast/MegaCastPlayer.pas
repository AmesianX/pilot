unit MegaCastPlayer;

interface

uses
  MegaCastUtils, BlockSync, MegaCastDecoder,
  Windows, Classes, SysUtils, Graphics;

type
  TMegaCastPlayer = class(TComponent)
  private
    FBlockSync : TBlockSync;
    FDecoder : TMegaCastDecoder;
    function on_NeedBlockUnit(Sender:TObject; var ABlockUnit:pointer; var AUnitSize:integer):boolean;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    procedure Stop;

    procedure BlockUnitIn(ABlockUnit:pointer; AUnitSize:integer);
    procedure SetFrameSize(AFrameSize:TFrameSize);
    procedure AudioSync(ATick:int64);

    function GetBitmap(const ABitmap:TBitmap):boolean;
  end;

implementation

{ TMegaCastPlayer }

procedure TMegaCastPlayer.AudioSync(ATick: int64);
begin
  FBlockSync.AudioSync(ATick);
end;

constructor TMegaCastPlayer.Create(AOwner: TComponent);
begin
  inherited;

  FBlockSync := TBlockSync.Create;

  FDecoder := TMegaCastDecoder.Create(Self);
  FDecoder.OnNeedBlockUnit := on_NeedBlockUnit;

  SetFrameSize(FrameSize(1024, 768));
end;

procedure TMegaCastPlayer.BlockUnitIn(ABlockUnit: pointer; AUnitSize: integer);
begin
  FBlockSync.AddBlockUnit(ABlockUnit, AUnitSize);
end;

destructor TMegaCastPlayer.Destroy;
begin
  FreeAndNil(FDecoder);
  FreeAndNil(FBlockSync);

  inherited;
end;

function TMegaCastPlayer.GetBitmap(const ABitmap: TBitmap): boolean;
begin
  Result := FDecoder.GetBitmap(ABitmap); 
end;

function TMegaCastPlayer.on_NeedBlockUnit(Sender: TObject;
  var ABlockUnit: pointer; var AUnitSize: integer): boolean;
begin
  Result := FBlockSync.GetBlockUnit(ABlockUnit, AUnitSize);
end;

procedure TMegaCastPlayer.SetFrameSize(AFrameSize: TFrameSize);
begin
  FBlockSync.SetFrameSize(AFrameSize);
  FDecoder.SetFrameSize(AFrameSize);
end;

procedure TMegaCastPlayer.Stop;
begin
  FDecoder.Stop;
end;

end.
