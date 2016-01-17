unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TfmMain = class(TForm)
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure on_WMHotKey(var Message:TWMHotKey); message WM_HotKey;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

function SetHotKey(Handle:integer; Key:UINT):boolean;
begin
  Result:= RegisterHotKey(Handle, GlobalAddAtom('MacroKey'), 0, Key);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  if SetHotKey(handle, VK_F5) = false then ShowMessage('핫키등록 실패');
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  UnRegisterHotKey(Self.Handle, GlobalAddAtom('MacroKey'));
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Caption:= IntToStr(Key);
end;

procedure SetKeyDown(Key:word);
begin
  Keybd_Event(Key, MapVirtualkey(Key, 0), 0, 0);
end;

procedure SetKeyUp(Key:word);
begin
  Keybd_Event(Key, MapVirtualkey(Key, 0), KEYEVENTF_KEYUP, 0);
end;

procedure SetKeyPress(Key:word);
begin
  SetKeyDown(Key);
  SetKeyUp(Key);
end;

procedure TfmMain.on_WMHotKey(var Message: TWMHotKey);
begin
  SetKeyPress(VK_END);
  SetKeyPress(VK_DELETE);

  SetKeyDown(VK_SHIFT);
  SetKeyDown(VK_RETURN);
  SetKeyUp(VK_RETURN);
  SetKeyUp(VK_SHIFT);
end;

end.

