unit Option;

interface

uses
  Classes, SysUtils;

type
  TOption = class(TComponent)
  strict private
  private
    FUserID : string;
    FPassword : string;
    function GetServerHost: string;
    function GetRoomNo: integer;
    function GetPassword: string;
    function GetUserID: string;
  published
  public
    constructor Create(AOwner: TComponent); override;
    
    class function Obj:TOption;
  published
    property ServerHost : string read GetServerHost;
    property RoomNo : integer read GetRoomNo;
    property UserID : string read GetUserID;
    property Password : string read GetPassword;
  end;

implementation

var
  MyObj : TOption = nil;

{ TOption }

function TOption.GetServerHost: string;
begin
  Result := ParamStr(1);
end;

constructor TOption.Create(AOwner: TComponent);
begin
  inherited;

  FUserID := '';
  FPassword := '';
end;

function TOption.GetPassword: string;
begin
  if FPassword = '' then Result := ParamStr(4)
  else Result := FPassword;
end;

function TOption.GetUserID: string;
begin
  if FUserID = '' then Result := ParamStr(3)
  else Result := FUserID;
end;

function TOption.GetRoomNo: integer;
begin
  Result := StrToIntDef(ParamStr(2), 0);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then MyObj := TOption.Create(nil);
  Result := MyObj;
end;

end.

