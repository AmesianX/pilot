unit Option;

interface

uses
  Classes, SysUtils;

type
  TOption = class(TComponent)
  private
    FUserID: string;
    FPassword: string;
    FLogined: boolean;
    FRoomNo: integer;
    FHost: string;
    FIsAdmin: boolean;
    FMicDeviceID: Integer;
    function GetVoiceBufferCapacity: integer;
    procedure SetVoiceBufferCapacity(const Value: integer);
    function GetApplicationTitle: string;
  public
    constructor Create(AOwner: TComponent); override;
    class function Obj:TOption;
  published
    property Host : string read FHost;
    property RoomNo : integer read FRoomNo;
    property IsAdmin : boolean read FIsAdmin;
    property UserID : string read FUserID write FUserID;
    property Password : string read FPassword write FPassword;
    property Logined : boolean read FLogined write FLogined;
    property VoiceBufferCapacity : integer read GetVoiceBufferCapacity write SetVoiceBufferCapacity;
    property ApplicationTitle: string read GetApplicationTitle;
    property MicDeviceID: Integer read FMicDeviceID write FMicDeviceID;
  end;

implementation

uses
  Disk;

var
  MyObj : TOption = nil;

{ TOption }

class function TOption.Obj: TOption;
begin
  if MyObj = nil then MyObj := TOption.Create(nil);
  Result := MyObj;
end;

constructor TOption.Create(AOwner: TComponent);
begin
  inherited;

  FLogined := False;
  FMicDeviceID := -1;

  FHost := ParamStr(1);
  FRoomNo := StrToIntDef(ParamStr(2), 0);

  if (UpperCase(ParamStr(3)) = 'Y') then
    FIsAdmin := True
  else
    FIsAdmin := False;

  FUserID := ParamStr(4);
  FPassword := ParamStr(5);
end;

function TOption.GetApplicationTitle: string;
begin
  Result := 'MegaChannel Topia Client';
end;

function TOption.GetVoiceBufferCapacity: integer;
begin
  Result := IniInteger(GetExecPath+'Options.ini', 'Voice', 'BufferCapacity', 50);
end;

procedure TOption.SetVoiceBufferCapacity(const Value: integer);
begin
  WriteIniInt(GetExecPath+'Options.ini', 'Voice', 'BufferCapacity', Value);
end;

end.
