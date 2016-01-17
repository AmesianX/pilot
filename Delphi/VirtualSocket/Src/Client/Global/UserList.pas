unit UserList;

interface

uses
  Contnrs, SysUtils, Classes;

type
  TUserListAction = (
    uaAdd,
    uaDelete,
    uaUpdate
  );

  TUser = class;
  TUserList = class;

  TUserListChangedEvent = procedure(Sender: TObject;
    const AAction: TUserListAction; const AUser: TUser) of object;

  TUser = class
  strict private
    FOwner: TUserList;
    FUserID: string;
    FCamVisible: Boolean;
    FIsAdmin: Boolean;
    FMicOnOff: Boolean;
  private
    procedure SetCamVisible(const Value: Boolean);
    procedure SetIsAdmin(const Value: Boolean);
    procedure SetMicOnOff(const Value: Boolean);
    procedure SetUserID(const Value: string);
  public
    constructor Create(const AUserList: TUserList);

    property UserID: string read FUserID write SetUserID;
    property IsAdmin: Boolean read FIsAdmin write SetIsAdmin;
    property MicOnOff: Boolean read FMicOnOff write SetMicOnOff;
    property CamVisible: Boolean read FCamVisible write SetCamVisible;
  end;

  TUserList = class
  private
    FList: TObjectList;
    procedure do_Changed(const AAction: TUserListAction; const AUser: TUser);
  private
    FOnChanged: TUserListChangedEvent;
    function GetCount: Integer;
    function GetItem(Index: Integer): TUser;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const AUserID: string; const AIsAdmin: Boolean; const AMicOnOff: Boolean): TUser;
    procedure Delete(const AUserID: string);
    procedure Clear;

    function FindByUserID(const AUserID: string): TUser;

    property Items[Index: Integer]: TUser read GetItem; default;
    property Count: Integer read GetCount;
    property OnChanged: TUserListChangedEvent read FOnChanged write FOnChanged;
  end;

implementation

{ TUserList }

function TUserList.Add(const AUserID: string; const AIsAdmin: Boolean;
  const AMicOnOff: Boolean): TUser;
begin
  Result := TUser.Create(Self);
  Result.UserID := AUserID;
  Result.IsAdmin := AIsAdmin;
  Result.MicOnOff := AMicOnOff;
  FList.Add(Result);

  do_Changed(uaAdd, Result);
end;

procedure TUserList.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
  begin
    do_Changed(uaDelete, TUser(FList[i]));
    TUser(FList[i]).Free;
  end;

  FList.Clear;
end;

constructor TUserList.Create;
begin
  FList := TObjectList.Create;
end;

procedure TUserList.Delete(const AUserID: string);
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
  begin
    if UpperCase(TUser(FList[i]).UserID) = UpperCase(AUserID) then
    begin
      do_Changed(uaDelete, TUser(FList[i]));

      FList.Delete(i);
      Exit;
    end;
  end;
end;

destructor TUserList.Destroy;
begin
  FList.Free;

  inherited;
end;

function TUserList.FindByUserID(const AUserID: string): TUser;
var
  i: Integer;
begin
  Result := nil;

  for i:=Count-1 downto 0 do
  begin
    if UpperCase(AUserID) = UpperCase(Items[i].UserID) then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function TUserList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TUserList.GetItem(Index: Integer): TUser;
begin
  Result := TUser(FList.Items[Index]);
end;

procedure TUserList.do_Changed(const AAction: TUserListAction; const AUser: TUser);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self, AAction, AUser);
end;

{ TUser }

constructor TUser.Create(const AUserList: TUserList);
begin
  FOwner := AUserList;
end;

procedure TUser.SetCamVisible(const Value: Boolean);
begin
  FCamVisible := Value;
  FOwner.do_Changed(uaUpdate, Self);
end;

procedure TUser.SetIsAdmin(const Value: Boolean);
begin
  FIsAdmin := Value;
  FOwner.do_Changed(uaUpdate, Self);
end;

procedure TUser.SetMicOnOff(const Value: Boolean);
begin
  FMicOnOff := Value;
  FOwner.do_Changed(uaUpdate, Self);
end;

procedure TUser.SetUserID(const Value: string);
begin
  FUserID := Value;
  FOwner.do_Changed(uaUpdate, Self);
end;

end.
