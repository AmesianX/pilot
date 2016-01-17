unit RoomInfo;

interface

uses
  CriticalValues, ValueList,
  Classes, SysUtils;

type
  TRoomInfo = class(TCriticalValues)
  private
    function get_AdminID: string;
    procedure set_AdminID(const Value: string);
    function get_CastStarted: boolean;
    procedure set_CastStarted(const Value: boolean);
  public
    procedure SetAdminID(const AValue:string);
    procedure SetCastStarted(const AValue:boolean);

    procedure CopyTo(Dst:TValueList);

    property AdminID : string read get_AdminID write set_AdminID;
    property CastStarted : boolean read get_CastStarted write set_CastStarted;
  end;

implementation

{ TRoomInfo }

procedure TRoomInfo.CopyTo(Dst: TValueList);
begin
  Lock;
  try
    Dst.Add(Lines.Text);
  finally
    Unlock;
  end;
end;

function TRoomInfo.get_AdminID: string;
begin
  Result := Lines.Values['AdminID'];
end;

function TRoomInfo.get_CastStarted: boolean;
begin
  Result := Lines.Booleans['CastStarted'];
end;

procedure TRoomInfo.SetAdminID(const AValue: string);
begin
  Lock;
  try
    AdminID := AValue;
  finally
    Unlock;
  end;
end;

procedure TRoomInfo.SetCastStarted(const AValue: boolean);
begin
  Lock;
  try
    CastStarted := AValue;
  finally
    Unlock;
  end;
end;

procedure TRoomInfo.set_AdminID(const Value: string);
begin
  Lines.Values['AdminID'] := Value;
end;

procedure TRoomInfo.set_CastStarted(const Value: boolean);
begin
  Lines.Booleans['CastStarted'] := Value;
end;

end.
