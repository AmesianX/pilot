unit _fmLogin;

interface

uses
  ValueList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, SwitchButton;

type
  TfmLogin = class(TForm)
    edUserID: TEdit;
    edPassword: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ImageBG: TImage;
    btLogin: TSwitchButton;
    btCancel: TSwitchButton;
    procedure btCancelClick(Sender: TObject);
    procedure btLoginClick(Sender: TObject);
    procedure edPasswordKeyPress(Sender: TObject; var Key: Char);
    procedure edPasswordKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edUserIDKeyPress(Sender: TObject; var Key: Char);
    procedure edUserIDKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FLogined : boolean;
    function get_UserIDandPasswordValidate:boolean;
  public
  published
    property Logined : boolean read FLogined;

    procedure rp_OkLogin(Packet:TValueList);
    procedure rp_ErLogin(Packet:TValueList);
  end;

function LoginDlg:boolean;

implementation

uses
  Global, View, Option;

function LoginDlg:boolean;
var
  fmLogin : TfmLogin;
begin
  fmLogin := TfmLogin.Create(Application);
  try
    fmLogin.ShowModal;
    Result := fmLogin.Logined;
  finally
    fmLogin.Free;
  end;
end;

{$R *.dfm}

procedure TfmLogin.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmLogin.btLoginClick(Sender: TObject);
begin
  if get_UserIDandPasswordValidate = false then begin
    if Trim(edUserID.Text) = '' then begin
      MessageBox(0, '사용자 이름을 확인해 주세요.', pChar(Caption), MB_ICONERROR or MB_OK);
      edUserID.SetFocus;
    end else if Trim(edPassword.Text) = '' then begin
      MessageBox(0, '비밀번호를을 확인해 주세요.', pChar(Caption), MB_ICONERROR or MB_OK);
      edPassword.SetFocus;
    end;
    Exit;
  end;

  TOption.Obj.UserID := edUserID.Text;
  TOption.Obj.Password := edPassword.Text;

  btLogin.Enabled := false;
  TGlobal.Obj.TextClient.sp_Login(edUserID.Text, edPassword.Text, TOption.Obj.IsAdmin);
end;

procedure TfmLogin.edPasswordKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    Key := #0;
    if btLogin.Enabled then btLoginClick(btLogin);
  end;
end;

procedure TfmLogin.edPasswordKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  btLogin.Enabled:= get_UserIDandPasswordValidate;
end;

procedure TfmLogin.edUserIDKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    Key := #0;
    edPassword.SetFocus;
  end;
end;

procedure TfmLogin.edUserIDKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  btLogin.Enabled := get_UserIDandPasswordValidate;
end;

procedure TfmLogin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TView.Obj.Remove(Self);
end;

procedure TfmLogin.FormCreate(Sender: TObject);
begin
  TView.Obj.Add(Self);

  FLogined := false;

  edUserID.Text := TOption.Obj.UserID;
  edPassword.Text := TOption.Obj.Password;

  if get_UserIDandPasswordValidate then btLoginClick(nil);
end;

function TfmLogin.get_UserIDandPasswordValidate: boolean;
begin
  Result := (Trim(edUserID.Text) <> '') and (Trim(edPassword.Text) <> '');
end;

procedure TfmLogin.rp_ErLogin(Packet: TValueList);
begin
  MessageDlg(Packet.Values['ErrorMsg'], mtError, [mbOk], 0);
  edUserID.SetFocus;

  btLogin.Enabled := get_UserIDandPasswordValidate;
end;

procedure TfmLogin.rp_OkLogin(Packet: TValueList);
begin
  FLogined := True;
  Close;
end;

end.
