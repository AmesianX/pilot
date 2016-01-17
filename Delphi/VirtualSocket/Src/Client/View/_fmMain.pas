unit _fmMain;

interface

uses
  View, ValueList, Global, Option,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _frChat, ExtCtrls, _frCam, _frToolBox, _frMultiCamProxy,
  _frMicSetting, WinSkinData, ComCtrls, Menus;

type
  TfmMain = class(TForm)
    Panel: TPanel;
    frChat: TfrChat;
    frCam: TfrCam;
    frMultiCamProxy: TfrMultiCamProxy;
    frToolBox: TfrToolBox;
    SkinData: TSkinData;
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miHelp: TMenuItem;
    miAbout: TMenuItem;
    miClose: TMenuItem;
    frMicSetting1: TfrMicSetting;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
  private
    procedure on_AppException(Sender: TObject; E: Exception);
    procedure DisplayUserTypeStatus;
    procedure DisplayMicStatus(const AMicOnOff: Boolean);
  public
  published
    procedure rp_Terminate(APacket: TValueList);
    procedure rp_OkLogin(APacket: TValueList);
    procedure rp_SetMic(APacket: TValueList);
  end;

var
  fmMain: TfmMain;

implementation

uses
  _fmAbout;

{$R *.dfm}

procedure TfmMain.DisplayMicStatus(const AMicOnOff: Boolean);
begin
  if AMicOnOff then StatusBar.Panels[1].Text := ' ¸¶ÀÌÅ© ÄÑÁü'
  else StatusBar.Panels[1].Text := ' ¸¶ÀÌÅ© ²¨Áü';
end;

procedure TfmMain.DisplayUserTypeStatus;
begin
  if TOption.Obj.IsAdmin then StatusBar.Panels[0].Text := ' °­»ç'
  else StatusBar.Panels[0].Text := ' ¼ö°­ÀÚ';
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  TView.Obj.Add(Self);

  Application.OnException := on_AppException;

  DisplayUserTypeStatus;
  DisplayMicStatus(False);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  TView.Obj.Remove(Self);
end;

procedure TfmMain.miAboutClick(Sender: TObject);
begin
  ShowAboutDlg;
end;

procedure TfmMain.miCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfmMain.on_AppException(Sender: TObject; E: Exception);
begin
//
end;

procedure TfmMain.rp_OkLogin(APacket: TValueList);
begin
  Caption := TOption.Obj.ApplicationTitle + ' - ' + TOption.Obj.UserID;
end;

procedure TfmMain.rp_SetMic(APacket: TValueList);
var
  UserID: string;
  OnOff: Boolean;
begin
  UserID := APacket.Values['UserID'];
  OnOff := APacket.Boolean['OnOff'];

  if UpperCase(UserID) = UpperCase(TOption.Obj.UserID) then
    DisplayMicStatus(OnOff);
end;

procedure TfmMain.rp_Terminate(APacket: TValueList);
begin
  if APacket.Values['Msg'] <> '' then
    MessageDlg(APacket.Values['Msg'], mtError, [mbOk], 0);
  Application.Terminate;
end;

end.
