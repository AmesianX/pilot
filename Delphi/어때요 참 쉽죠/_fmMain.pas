unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls;

type
  TfmMain = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure on_WMHotKey(var Message:TWMHotKey); message WM_HotKey;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

{ TForm1 }

function SetHotKey(Handle:integer; Key:UINT):boolean;
begin
  Result:= RegisterHotKey(Handle, GlobalAddAtom('This_is_simple'), 0, Key);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  if SetHotKey(handle, VK_F9) = false then ShowMessage('핫키등록 실패');
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  UnRegisterHotKey(Self.Handle, GlobalAddAtom('This_is_simple'));
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Hide;  
end;

procedure TfmMain.on_WMHotKey(var Message: TWMHotKey);
begin
  Show;
end;

end.
