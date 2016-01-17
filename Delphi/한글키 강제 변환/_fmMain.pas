unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure on_WMHotKey(var Msg:TWMHotKey); message WM_HotKey;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

{ TfmMain }

function SetHotKey(Handle:integer; Modifiers,Key:UINT):boolean;
begin
  {* Note:
    case Modifiers of
         MOD_ALT
         MOD_CONTROL
         MOD_KEYUP
         MOD_SHIFT
         MOD_WIN
  }

  Result:= RegisterHotKey(Handle, GlobalAddAtom('HanKey'), Modifiers, Key);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  if SetHotKey(handle, MOD_SHIFT, VK_SPACE) = false then begin
    MessageDlg('핫키등록 실패', mtError, [mbOk], 0);
    Application.Terminate;
  end;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  UnRegisterHotKey(Self.Handle, GlobalAddAtom('HanKey'));
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

procedure TfmMain.on_WMHotKey(var Msg: TWMHotKey);
begin
  SetKeyPress(VK_HANGUL);
end;

end.
