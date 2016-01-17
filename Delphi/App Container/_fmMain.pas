unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvCombobox, ExtCtrls, TLHelp32, Buttons;

type
  TSizeChangeEvent = procedure(Sender : TObject; Rect:TRect) of object;

  TfmMain = class(TForm)
    pnToolBar: TPanel;
    cbAppList: TJvComboBox;
    BtnReflash: TSpeedButton;
    pnScreen: TPanel;
    BtnOut: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure cbAppListChange(Sender: TObject);
    procedure BtnReflashClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnOutClick(Sender: TObject);
  private
    FLastContainedWindowHandle : HWND;
    FLastContainedWindowRect : TRect;
    FOnSizeChange: TSizeChangeEvent;
    procedure GetProcessList;
    procedure OutContainedForm;
    procedure SetOnSizeChange(const Value: TSizeChangeEvent);
  public
    property OnSizeChange : TSizeChangeEvent read FOnSizeChange write SetOnSizeChange;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

function EnumWindowCallBack(WindowHandle : HWND; LongParam  : LPARAM):boolean; stdcall;
var
  Cap : Array [0..255] of Char;
  Self : TfmMain absolute LongParam;
begin
  if (GetParent(WindowHandle) = 0) and (WindowHandle <> Self.Handle) then begin
    GetWindowText(WindowHandle, Cap, sizeof(Cap));
    if IsWindowVisible(WindowHandle) and (StrLen(Cap) > 0)then begin
      Self.cbAppList.Items.AddObject(Cap, Pointer(WindowHandle));
    end;
  end;
  Result := true;
end;

procedure TfmMain.BtnOutClick(Sender: TObject);
begin
  OutContainedForm;
end;

procedure TfmMain.BtnReflashClick(Sender: TObject);
begin
  GetProcessList;
end;

procedure TfmMain.cbAppListChange(Sender: TObject);
var
  Cb : TJvComboBox absolute Sender;
  NowHandle : HWND;
begin
  if Cb.ItemIndex = 0 then exit;

  NowHandle := HWND(Cb.Items.Objects[Cb.ItemIndex]);
  OutContainedForm;
  FLastContainedWindowHandle := NowHandle;
  SendMessage(FLastContainedWindowHandle, WM_SHOWWINDOW, 1, SW_PARENTOPENING);
  GetWindowRect(FLastContainedWindowHandle, FLastContainedWindowRect);

  Windows.SetParent(FLastContainedWindowHandle, pnScreen.Handle);
  MoveWindow(FLastContainedWindowHandle,0,0,
    FLastContainedWindowRect.Right - FLastContainedWindowRect.Left,
    FLastContainedWindowRect.Bottom - FLastContainedWindowRect.Top,
    True);

end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OutContainedForm;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FLastContainedWindowHandle := 0;
  GetProcessList;
end;

procedure TfmMain.FormResize(Sender: TObject);
var
  R : TRect;
begin
  if Assigned(FOnSizeChange) then begin
    GetWindowRect(pnScreen.Handle, R);
    FOnSizeChange(Self, R);
  end;
end;

procedure TfmMain.GetProcessList;
begin
  cbAppList.Clear;
  cbAppList.Items.Add('프로그램을 선택 해 주세요');
  EnumWindows(@EnumWindowCallBack, Integer(Self));

  cbAppList.ItemIndex := 0;
end;

procedure TfmMain.OutContainedForm;
begin
  if FLastContainedWindowHandle <> 0 then begin
    Windows.SetParent(FLastContainedWindowHandle, GetDesktopWindow);
    MoveWindow(FLastContainedWindowHandle, FLastContainedWindowRect.Left,
      FLastContainedWindowRect.Top,
      FLastContainedWindowRect.Right - FLastContainedWindowRect.Left,
      FLastContainedWindowRect.Bottom - FLastContainedWindowRect.Top,
      true);
  end;
  FLastContainedWindowHandle := 0;

  GetProcessList;
end;

procedure TfmMain.SetOnSizeChange(const Value: TSizeChangeEvent);
begin
  FOnSizeChange := Value;
end;

end.
