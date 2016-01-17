unit _frToolBox;

interface

uses
  ValueList, UserList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Spin, ExtCtrls, Gauges, SwitchButton;

type
  TfrToolBox = class(TFrame)
    ToolBoxPanel: TPanel;
    btnMicOn: TSwitchButton;
    btnAlert: TSwitchButton;
    procedure btnMicOnChanged(Sender: TObject);
    procedure btnAlertChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure rp_ErAskMic(APacket: TValueList);
    procedure rp_SetMic(APacket: TValueList);
  end;

implementation

uses
  Global, Option, View;

{$R *.dfm}

procedure TfrToolBox.btnAlertChanged(Sender: TObject);
begin
  TGlobal.Obj.TextClient.sp_Alert(TOption.Obj.UserID);
  btnAlert.SwitchOn := True;
end;

procedure TfrToolBox.btnMicOnChanged(Sender: TObject);
begin
  if btnMicOn.SwitchOn then
  begin
    if TOption.Obj.MicDeviceID = -1 then
    begin
      MessageDlg('마이크가 없으므로 음성대화를 할 수 없습니다.', mtError, [mbOk], 0);
      btnMicOn.SwitchOn := False;
      Exit;
    end;

    TGlobal.Obj.TextClient.sp_AskMic(TOption.Obj.UserID)
  end else
  begin
    TGlobal.Obj.TextClient.sp_SetMic(TOption.Obj.UserID, False);
  end;
end;

constructor TfrToolBox.Create(AOwner: TComponent);
begin
  inherited;

  TView.Obj.Add(Self);

  btnAlert.Visible := not TOption.Obj.IsAdmin;
end;

destructor TfrToolBox.Destroy;
begin
  TView.Obj.Remove(Self);

  inherited;
end;

procedure TfrToolBox.rp_ErAskMic(APacket: TValueList);
var
  ErrorMsg: string;
begin
  ErrorMsg := APacket.Values['ErrorMsg'];
  MessageDlg(ErrorMsg, mtInformation, [mbOk], 0);

  btnMicOn.SwitchOn := False;
end;

procedure TfrToolBox.rp_SetMic(APacket: TValueList);
var
  UserID: string;
  OnOff: Boolean;
begin
  UserID := APacket.Values['UserID'];
  OnOff := APacket.Boolean['OnOff'];

  if UpperCase(UserID) = UpperCase(TOption.Obj.UserID) then
    btnMicOn.SwitchOn := OnOff;
end;

end.
