unit _fmMain;

interface

uses
  ScreenCapture,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus;

const
  _HotKey01 = 'DeskPen-HotKey-01';

type
  TfmMain = class(TForm)
    PopupMenu: TPopupMenu;
    miClose: TMenuItem;
    TrayIcon: TTrayIcon;
    Image: TImage;
    N1: TMenuItem;
    miSelectMonitor: TMenuItem;
    procedure miCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FSelectMonitorMenuItems : array of TMenuItem;
    FScreenCapture : TScreenCapture;
    procedure do_Show;
    procedure do_GetMonitorList;
    procedure on_SelectMonitor(Sender:TObject);
    procedure do_WM_Hotkey(var Msg:TWMHotkey); message WM_HOTKEY;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.do_GetMonitorList;
var
  Loop : Integer;
begin
  SetLength(FSelectMonitorMenuItems, Screen.MonitorCount);

  for Loop := Low(FSelectMonitorMenuItems) to High(FSelectMonitorMenuItems) do begin
    FSelectMonitorMenuItems[Loop] := TMenuItem.Create(PopupMenu);

    FSelectMonitorMenuItems[Loop].Caption := Format('모니터 %d번 선택', [Loop]);
    FSelectMonitorMenuItems[Loop].Checked := Loop = 0;
    FSelectMonitorMenuItems[Loop].Tag := Loop;
    FSelectMonitorMenuItems[Loop].OnClick := on_SelectMonitor;

    miSelectMonitor.Add(FSelectMonitorMenuItems[Loop]);
  end;
end;

procedure TfmMain.do_Show;
begin
  if not Visible then begin
    Left := FScreenCapture.X;
    Top  := FScreenCapture.Y;
    Width := FScreenCapture.Width;
    Height := FScreenCapture.Height;

    FScreenCapture.Capture;

    Show;
  end;

  Image.Picture.Bitmap.Assign(FScreenCapture.Bitmap);
end;

procedure TfmMain.do_WM_Hotkey(var Msg: TWMHotkey);
begin
  if Msg.HotKey = GlobalAddAtom(_HotKey01) then do_Show;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  Hide;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.Canvas.Pen.Color := clRed;
  Image.Picture.Bitmap.Canvas.Pen.Width := 3;

  FScreenCapture := TScreenCapture.Create(Self);
  FScreenCapture.WithCursor := false;
  FScreenCapture.MonitorNo := 0;

  do_GetMonitorList;

  if (RegisterHotKey(Handle, GlobalAddAtom(_HotKey01), MOD_CONTROL, Byte('1')) = false) then begin
    MessageDlg('핫키 등록 실패', mtError, [mbOK], 0);
  end;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  UnRegisterHotKey(Handle, GlobalAddAtom(_HotKey01));

  FreeAndNil(FScreenCapture);
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;  
end;

procedure TfmMain.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Image.Picture.Bitmap.Canvas.MoveTo(X, Y);
end;

procedure TfmMain.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Shift = [ssLeft] then Image.Picture.Bitmap.Canvas.LineTo(X, Y);
end;

procedure TfmMain.miCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfmMain.on_SelectMonitor(Sender: TObject);
var
  Loop : Integer;
begin
  for Loop := Low(FSelectMonitorMenuItems) to High(FSelectMonitorMenuItems) do begin
    FSelectMonitorMenuItems[Loop].Checked := false;
  end;

  TMenuItem(Sender).Checked := true;
  FScreenCapture.MonitorNo := TMenuItem(Sender).Tag;
end;

end.
