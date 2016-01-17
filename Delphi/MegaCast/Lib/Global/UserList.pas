unit UserList;

interface

uses
  ValueList,
  Classes, SysUtils;

type
  TUserList = class(TComponent)
  private
    FList : TStringList;
    function find_UserID(AUserID:string):integer;
    procedure do_Add(APacket:TValueList);
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    function GetUserInfos(Index: integer): TValueList;
    function GetCount: integer;
    function GetUserInfoByUserID(UserID: string): TValueList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecutePacket(APacket:TValueList);

    procedure Clear;
    procedure Add(APacket:TValueList);
    procedure Remove(AUserID:string);

    property Count : integer read GetCount;
    property UserInfos[Index:integer] : TValueList read GetUserInfos;
    property UserInfoByUserID[UserID:string] : TValueList read GetUserInfoByUserID;
    property OnChange : TNotifyEvent read GetOnChange write SetOnChange;
  published
    procedure rp_OkLogin(APacket:TValueList);
    procedure rp_UserList(APacket:TValueList);
    procedure rp_UserIn(APacket:TValueList);
    procedure rp_UserOut(APacket:TValueList);
  end;

implementation

{ TUserList }

procedure TUserList.Add(APacket: TValueList);
var
  Loop: Integer;
  UserInfo: TValueList;
begin
  FList.BeginUpdate;
  UserInfo := TValueList.Create;
  try
    for Loop := 0 to APacket.Count-1 do begin
      UserInfo.Add(APacket.Strings[Loop]);
      if APacket.Strings[Loop] = 'end.' then begin
        do_Add(UserInfo);
        UserInfo.Clear;
      end;
    end;

    if UserInfo.Count > 0 then do_Add(UserInfo);
  finally
    FList.EndUpdate;
    UserInfo.Free;
  end;
end;

procedure TUserList.Clear;
var
  Loop: Integer;
  UserInfo: TValueList;
begin
  for Loop := 0 to FList.Count - 1 do begin
    UserInfo := Pointer(FList.Objects[Loop]);
    UserInfo.Free;
  end;
  FList.Clear;
end;

constructor TUserList.Create(AOwner: TComponent);
begin
  inherited;

  FList := TStringList.Create;
end;

destructor TUserList.Destroy;
begin
  Clear;

  FreeAndNil(FList);

  inherited;
end;

procedure TUserList.do_Add(APacket: TValueList);
var
  UserInfo: TValueList;
begin
  Remove(APacket.Values['UserID']);

  UserInfo := TValueList.Create;
  UserInfo.Text := APacket.Text;

  FList.AddObject(APacket.Values['UserID'], UserInfo);
end;

procedure TUserList.ExecutePacket(APacket: TValueList);
var
  Proc : procedure (APacket:TValueList) of object;
begin
  TMethod(Proc).Data := Self;
  TMethod(Proc).Code := Self.MethodAddress('rp_' + APacket.Values['Code']);
  if Assigned(Proc) then Proc(APacket);
end;

function TUserList.find_UserID(AUserID: string): integer;
begin
  Result := FList.IndexOf(AUserID);
end;

function TUserList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TUserList.GetOnChange: TNotifyEvent;
begin
  Result := FList.OnChange;
end;

function TUserList.GetUserInfoByUserID(UserID: string): TValueList;
var
  iIndex: Integer;
begin
  iIndex := find_UserID(UserID);
  if iIndex = -1 then begin
    Result := nil;
    Exit;
  end;

  Result := Pointer(FList.Objects[iIndex]);
end;

function TUserList.GetUserInfos(Index: integer): TValueList;
begin
  Result := Pointer(FList.Objects[Index]);
end;

procedure TUserList.Remove(AUserID: string);
var
  iIndex: Integer;
  UserInfo: TValueList;
begin
  iIndex := find_UserID(AUserID);
  if iIndex = -1 then Exit;

  UserInfo := Pointer(FList.Objects[iIndex]);
  UserInfo.Free;

  FList.Delete(iIndex);
end;

procedure TUserList.rp_OkLogin(APacket: TValueList);
begin
  Clear;
  Add(APacket);
end;

procedure TUserList.rp_UserIn(APacket: TValueList);
begin
  Add(APacket);
end;

procedure TUserList.rp_UserList(APacket: TValueList);
begin
  Add(APacket);
end;

procedure TUserList.rp_UserOut(APacket: TValueList);
begin
  Remove(APacket.Values['UserID']);
end;

procedure TUserList.SetOnChange(const Value: TNotifyEvent);
begin
  FList.OnChange := Value;
end;

end.
