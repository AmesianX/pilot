unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  pText : pchar;
  iHandle : integer;
  bCondition : boolean;
  WindowInfo : tagWindowInfo;
begin
  GetMem(pText, 512);

  iHandle:= FindWindowEx(0, 0, nil, nil);
  while iHandle > 0 do begin
    GetWindowInfo(iHandle, WindowInfo);
    GetWindowText(iHandle, pText, 512);

    bCondition:=
      IsWindowVisible(iHandle) and
      ((WindowInfo.dwStyle and WS_EX_APPWINDOW) = WS_EX_APPWINDOW) and
      (StrPas(pText) <> '') and
      (GetWindowDC(iHandle) > 0) and
      (GetParent(iHandle) = 0);
    if bCondition then
      ListBox1.Items.Add(Format('%s : (%d, %d)', [StrPas(pText), WindowInfo.rcWindow.Left, WindowInfo.rcWindow.Top]));

    iHandle:= FindWindowEx(0, iHandle, nil, nil);
  end;
end;

end.
