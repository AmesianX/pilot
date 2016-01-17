unit _frChat;

interface

uses
  ValueList, UserList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, StdCtrls, ExtCtrls,
  jpeg, Menus, SwitchButton;

const
  _MaxChatLines = 500;
  _MicOnString = '말하는중';

type
  TfrChat = class(TFrame)
    ilUserList: TImageList;
    UserList: TListView;
    Splitter1: TSplitter;
    ilChat: TImageList;
    plChat: TPanel;
    moMsg: TMemo;
    PopupMenu: TPopupMenu;
    MenuItemKickOut: TMenuItem;
    moReceive: TRichEdit;
    PanelToolBox: TPanel;
    ColorBox: TColorBox;
    chkWhisper: TCheckBox;
    cbUserList: TComboBox;
    btSend: TSwitchButton;
    MenuItemMicOn: TMenuItem;
    MenuItemMicOff: TMenuItem;
    procedure btSendClick(Sender: TObject);
    procedure moMsgKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItemKickOutClick(Sender: TObject);
    procedure chkWhisperClick(Sender: TObject);
    procedure cbUserListChange(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure MenuItemMicOnClick(Sender: TObject);
    procedure MenuItemMicOffClick(Sender: TObject);
  private
    procedure UserAdded(const AUser: TUser);
    procedure UserDeleted(const AUser: TUser);
    procedure UserUpdated(const AUser: TUser);
    function FindByUserIDFromUserListView(const AUserID: string): TListItem;
    function FindByUserIDFromUserComboBox(const AUserID: string): Integer;
    procedure AddUserListView(const AUser: TUser);
    procedure DeleteUserListView(const AUser: TUser);
    procedure UpdateUserListView(const AUser: TUser);
    procedure AddUserComboBox(const AUser: TUser);
    procedure DeleteUserComboBox(const AUser: TUser);
    procedure AlertSoundPlay;
    procedure WriteChatString(const AStr: string; const AColor: TColor);
    procedure WriteNotice(const AStr: string);
 public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure rp_UserListChanged(APacket: TValueList);
    procedure rp_UserIn(APacket: TValueList);
    procedure rp_Talk(APacket: TValueList);
    procedure rp_Whisper(APacket: TValueList);
    procedure rp_KickOut(APacket:TValueList);
    procedure rp_Alert(APacket: TValueList);
    procedure rp_AskMic(APacket: TValueList);
  end;

implementation

uses
  Option, Global, View,
  MMSystem;

{$R *.dfm}

{ TfrChat }

procedure TfrChat.AddUserComboBox(const AUser: TUser);
begin
  if Trim(AUser.UserID) = '' then Exit;
  if cbUserList.Items.IndexOf(AUser.UserID) > 0 then Exit;
  if UpperCase(AUser.UserID) = UpperCase(TOption.Obj.UserID) then Exit;

  cbUserList.Items.Add(AUser.UserID);
end;

procedure TfrChat.AddUserListView(const AUser: TUser);
var
  ListItem : TListItem;
begin
  ListItem := UserList.Items.Add;
  ListItem.Caption := AUser.UserID;  // ID
  if AUser.MicOnOff then             // 음성
    ListItem.SubItems.Add(_MicOnString)
  else
    ListItem.SubItems.Add('');
  if AUser.ISAdmin then              // 아이콘
    ListItem.ImageIndex := 0
  else
    ListItem.ImageIndex := 1;
end;

procedure TfrChat.btSendClick(Sender: TObject);
var
  sMsg : string;
  iResult : integer;
begin
  sMsg := Trim(moMsg.Text);
  if sMsg = '' then Exit;
  if chkWhisper.Checked and (cbUserList.ItemIndex < 0) then
  begin
    iResult := MessageBox(0, '귓속말이 선택되었으나 대화상대가 지정되지 않았습니다.'+#13+#10+'모두에게 보내겠습니까 ?', '귓속말 확인', MB_ICONQUESTION or MB_YESNO);
    if iResult = idNo then Exit;
  end;

  if chkWhisper.Checked and (cbUserList.ItemIndex < 0) then chkWhisper.Checked := False;

  moMsg.Clear;

  if chkWhisper.Checked then begin
    TGlobal.Obj.TextClient.sp_Whisper(TOption.Obj.UserID, cbUserList.Items[cbUserList.ItemIndex], sMsg, ColorBox.Selected);
  end
  else begin
    TGlobal.Obj.TextClient.sp_Talk(TOption.Obj.UserID, sMsg, ColorBox.Selected);
  end;
end;

procedure TfrChat.chkWhisperClick(Sender: TObject);
begin
  if not TCheckBox(Sender).Checked then cbUserList.ItemIndex := -1;
end;

procedure TfrChat.cbUserListChange(Sender: TObject);
begin
  chkWhisper.Checked := TComboBox(Sender).ItemIndex >= 0;
end;

constructor TfrChat.Create(AOwner: TComponent);
begin
  inherited;

  TView.Obj.Add(Self);

  UserList.Checkboxes := TOption.Obj.IsAdmin;

  WriteNotice('강의실에 입장하였습니다.');
end;

procedure TfrChat.DeleteUserComboBox(const AUser: TUser);
var
  FindedIndex: Integer;
begin
  FindedIndex := FindByUserIDFromUserComboBox(AUser.UserID);
  if FindedIndex <> -1 then
    cbUserList.Items.Delete(FindedIndex);
end;

procedure TfrChat.DeleteUserListView(const AUser: TUser);
var
  ListItem : TListItem;
begin
  ListItem := FindByUserIDFromUserListView(AUser.UserID);
  if ListItem <> nil then
    ListItem.Free;
end;

destructor TfrChat.Destroy;
begin
  TView.Obj.Remove(Self);

  inherited;
end;

procedure TfrChat.WriteChatString(const AStr: string; const AColor: TColor);
begin
  if moReceive.Lines.Count >= _MaxChatLines then
    moReceive.Lines.Clear;

  moReceive.SelAttributes.Color := AColor;
  moReceive.Lines.Add(AStr);
end;

procedure TfrChat.WriteNotice(const AStr: string);
begin
  WriteChatString(AStr, $00484848);
end;

function TfrChat.FindByUserIDFromUserComboBox(const AUserID: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := cbUserList.Items.Count-1 downto 0 do
  begin
    if UpperCase(AUserID) = UpperCase(cbUserList.Items[i]) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TfrChat.FindByUserIDFromUserListView(const AUserID: string): TListItem;
var
  i: Integer;
begin
  Result := nil;

  for i := UserList.Items.Count-1 downto 0 do
  begin
    if UpperCase(AUserID) = UpperCase(UserList.Items[i].Caption) then
    begin
      Result := UserList.Items[i];
      Break;
    end;
  end;
end;

procedure TfrChat.moMsgKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift <> [ssShift]) and (Key = VK_RETURN) then btSendClick(nil);

  if moMsg.Lines.Count > 3 then moMsg.ScrollBars := ssVertical
  else moMsg.ScrollBars := ssNone;
end;

procedure TfrChat.PopupMenuPopup(Sender: TObject);
var
  Item: TListItem;
begin
  Item := UserList.Selected;
  if Item = nil then Exit;

  MenuItemKickOut.Visible := TOption.Obj.IsAdmin and (Item.Caption <> TOption.Obj.UserID);
  MenuItemMicOn.Visible   := TOption.Obj.IsAdmin and (Item.SubItems[0] <> _MicOnString) and (Item.Caption <> TOption.Obj.UserID);
  MenuItemMicOff.Visible  := TOption.Obj.IsAdmin and (Item.SubItems[0] = _MicOnString) and (Item.Caption <> TOption.Obj.UserID);
end;

procedure TfrChat.rp_Alert(APacket: TValueList);
var
  UserID: string;
begin
  UserID := APacket.Values['UserID'];

  if TOption.Obj.IsAdmin then
  begin
    AlertSoundPlay;
    WriteNotice(APacket.Values['UserID'] + '님이 호출합니다.');
  end else
  begin
    if UpperCase(UserID) = UpperCase(TOption.Obj.UserID) then
      WriteNotice('강사를 호출하였습니다.');
  end;
end;

procedure TfrChat.rp_AskMic(APacket: TValueList);
begin
  AlertSoundPlay;
  WriteNotice(APacket.Values['UserID'] + '님이 마이크를 요청합니다.');
end;

procedure TfrChat.AlertSoundPlay;
const
  Alert_File = '.\Alert.wav';
begin
  if not FileExists(Alert_File) then Exit;

  if TOption.Obj.IsAdmin then sndPlaySound(Alert_File, SND_ASYNC);
end;

procedure TfrChat.rp_KickOut(APacket: TValueList);
var
  UserID, Msg: string;
begin
  UserID := APacket.Values['UserID'];
  Msg := APacket.Values['Msg'];

  if UpperCase(TOption.Obj.UserID) = UpperCase(UserID) then
    TView.Obj.sp_Terminate(Msg)
  else
    WriteNotice(UserID + ' 님이 강제 퇴장되었습니다.');
end;

procedure TfrChat.rp_Talk(APacket: TValueList);
var
  Msg: string;
begin
  Msg := APacket.Values['Msg'];

  WriteChatString(Format('%s 님의 말:', [APacket.Values['FromID']]), $00484848);
  WriteChatString(Format('%s', [Msg]), APacket.Integers['TextColor']);
end;

procedure TfrChat.rp_UserIn(APacket: TValueList);
begin
  WriteNotice(APacket.Values['UserID'] + ' 님이 입장하였습니다.');
end;

procedure TfrChat.rp_UserListChanged(APacket: TValueList);
var
  Action: TUserListAction;
  User: TUser;
begin
  Action := TUserListAction(APacket.Integers['Action']);
  User := APacket.Pointers['User'];

  case Action of
    uaAdd: UserAdded(User);
    uaDelete: UserDeleted(User);
    uaUpdate: UserUpdated(User);
  end;
end;

procedure TfrChat.rp_Whisper(APacket: TValueList);
var
  Msg: string;
begin
  Msg := APacket.Values['Msg'];

  WriteChatString(Format('%s 님의 귓속말:', [APacket.Values['FromID']]), $00484848);
  WriteChatString(Format('%s', [Msg]), APacket.Integers['TextColor']);
end;

procedure TfrChat.UpdateUserListView(const AUser: TUser);
var
  ListItem : TListItem;
begin
  ListItem := FindByUserIDFromUserListView(AUser.UserID);
  if ListItem = nil then Exit;

  if AUser.MicOnOff then
  begin
    AlertSoundPlay;
    WriteNotice(AUser.UserID + '님이 발언합니다.');

    ListItem.SubItems[0] := _MicOnString;
  end else
  begin
    ListItem.SubItems[0] := '';
  end;
end;

procedure TfrChat.UserAdded(const AUser: TUser);
begin
  AddUserListView(AUser);
  AddUserComboBox(AUser);
end;

procedure TfrChat.UserDeleted(const AUser: TUser);
begin
  DeleteUserListView(AUser);
  DeleteUserComboBox(AUser);
  WriteNotice(AUser.UserID + ' 님이 퇴장하였습니다.');
end;

procedure TfrChat.UserUpdated(const AUser: TUser);
begin
  UpdateUserListView(AUser);
end;

procedure TfrChat.MenuItemKickOutClick(Sender: TObject);
var
  UserID: String;
begin
  if UserList.Selected = nil then Exit;
  UserID := UserList.Selected.Caption;

  if UpperCase(UserID) = UpperCase(TOption.Obj.UserID) then
    MessageDlg('자기 자신은 강제 퇴장 할수 없습니다.', mtInformation, [mbOK], 0)
  else
    TGlobal.Obj.TextClient.sp_KickOut(UserID, '방장으로부터 강제 퇴장되었습니다.');
end;

procedure TfrChat.MenuItemMicOffClick(Sender: TObject);
var
  Item: TListItem;
  UserID: string;
begin
  Item := UserList.Selected;
  if Item = nil then Exit;

  UserID := Item.Caption;
  TGlobal.Obj.TextClient.sp_SetMic(UserID, False);
end;

procedure TfrChat.MenuItemMicOnClick(Sender: TObject);
var
  Item: TListItem;
  UserID: string;
begin
  Item := UserList.Selected;
  if Item = nil then Exit;

  UserID := Item.Caption;
  TGlobal.Obj.TextClient.sp_SetMic(UserID, True);
end;

end.
