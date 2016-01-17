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
var
  ID : string;
  Strs : string;
  Loop : Integer;
begin
  ID := '';
  Strs := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';

  Randomize;
  for Loop := 1 to 32 do ID := ID + Copy(Strs, Random(Length(Strs))+1, 1);

  // Todo : 
  TGlobal.Obj.TextClient.sp_Login(ID, TOption.Obj.Password, false);
end;

end.
