unit _frMultiCamProxy;

interface

uses
  _frMultiUserCam, View, Global, ValueList, UserList, Option,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfrMultiCamProxy = class(TFrame)
    frMultiCam: TfrMultiUserCamView;
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure on_GetNextUserID(Sender: TObject; var AUserID: string);
  published
    procedure rp_CamImage(APacket: TValueList);
    procedure rp_UserListChanged(APacket: TValueList);
  end;

implementation

{$R *.dfm}

{ TfrMultiCamProxy }

constructor TfrMultiCamProxy.Create(AOwner: TComponent);
begin
  inherited;

  TView.Obj.Add(Self);

  frMultiCam.DefaultCamOn := True;
  TGlobal.Obj.CamClient.OnGetNextUserID := on_GetNextUserID;
end;

destructor TfrMultiCamProxy.Destroy;
begin
  TView.Obj.Remove(Self);

  inherited;
end;

procedure TfrMultiCamProxy.on_GetNextUserID(Sender: TObject;
  var AUserID: string);
begin
  AUserID := frMultiCam.GetNextUserID;
end;

procedure TfrMultiCamProxy.rp_CamImage(APacket: TValueList);
var
  UserID: string;
  CamImage: TBitmap;
begin
  UserID := APacket.Values['UserID'];
  CamImage := APacket.Pointers['CamImage'];

  frMultiCam.DataIn(UserID, CamImage);
end;

procedure TfrMultiCamProxy.rp_UserListChanged(APacket: TValueList);
var
  Action: TUserListAction;
  User: TUser;
begin
  Action := TUserListAction(APacket.Integers['Action']);
  User := APacket.Pointers['User'];

  case Action of
    uaAdd:
    begin
      if UpperCase(User.UserID) <> UpperCase(TOption.Obj.UserID) then
        frMultiCam.UserIn(User.UserID);
    end;

    uaDelete: frMultiCam.UserOut(User.UserID);
    uaUpdate: ;
  end;
end;

end.
