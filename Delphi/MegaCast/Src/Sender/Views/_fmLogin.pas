unit _fmLogin;

interface

uses
  ValueList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfmLogin = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
  published
    procedure rp_TextSocketIsReady(APacket:TValueList);
  end;

var
  fmLogin: TfmLogin;

implementation

uses
  Global, View, Option;

{$R *.dfm}

procedure TfmLogin.FormCreate(Sender: TObject);
begin
  TView.Obj.Add(Self);
end;

procedure TfmLogin.FormDestroy(Sender: TObject);
begin
  TView.Obj.Remove(Self);
end;

procedure TfmLogin.rp_TextSocketIsReady(APacket: TValueList);
begin
  TGlobal.Obj.TextClient.sp_Login(TOption.Obj.UserID, TOption.Obj.Password, false);
end;

end.
