unit _fmmsSelector;

interface

uses
  _fmmsScreen, 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, lImageButton, StdCtrls;

type
  TfmmsSelector = class(TForm)
    Label1: TLabel;
    BtnSelect: TlImageButton;
    BtnCancel: TlImageButton;
    LblMonitorNum: TLabel;
    lblMonitorRect: TLabel;
    lblWidth: TLabel;
    lblHeight: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSelectClick(Sender: TObject);
  private
    FMonitorNum: Integer;
    FSelectedMonitorNum: Integer;
    procedure SetMonitorNum(const Value: Integer);
    { Private declarations }
  public
    property MonitorNum : Integer read FMonitorNum write SetMonitorNum; //외부 사용금지
    property SelectedMonitorNum : Integer read FSelectedMonitorNum;
  end;

var
  fmmsSelector: TfmmsSelector;

implementation

{$R *.dfm}

procedure TfmmsSelector.BtnCancelClick(Sender: TObject);
begin
  FSelectedMonitorNum := -1;
  Close;
end;

procedure TfmmsSelector.BtnSelectClick(Sender: TObject);
begin
  FSelectedMonitorNum := MonitorNum;
  Close;
end;

procedure TfmmsSelector.FormCreate(Sender: TObject);
var
  MoniterCount : Integer;
  I: Integer;
  fm : TfmmsScreen;
begin
  FMonitorNum := -1;
  for I :=  Screen.MonitorCount - 1 downto 0 do begin
    fm := TfmmsScreen.Create(Self);
    fm.Width := Screen.Monitors[i].Width;
    fm.Height := Screen.Monitors[i].Height;
    fm.Left := Screen.Monitors[i].Left;
    fm.Top := Screen.Monitors[i].Top;
    fm.MonitorNum := i;
    fm.Show;
  end;
end;

procedure TfmmsSelector.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then BtnCancelClick(BtnCancel);
  if Key = VK_RETURN then BtnSelectClick(BtnSelect);
  
end;

procedure TfmmsSelector.FormShow(Sender: TObject);
var
  R : TRect;
begin
  Windows.GetWindowRect(Handle, R);
  SetWindowPos(Handle, HWND_TOPMOST , R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, 0);

  if (MonitorNum = -1) then MonitorNum := 0;
end;

procedure TfmmsSelector.SetMonitorNum(const Value: Integer);
begin
  if FMonitorNum = Value then exit;
  
  FMonitorNum := Value;

  LblMonitorNum.Caption := Format('%d 번 모니터', [Value + 1]);
  lblMonitorRect.Caption := Format('X, Y, W, H : %d %d %d %d',
    [Screen.Monitors[Value].Left, Screen.Monitors[Value].Top,
    Screen.Monitors[Value].BoundsRect.Right, Screen.Monitors[Value].BoundsRect.Bottom]);

  lblWidth.Caption := Format('영역의 넓이 : %d', [Screen.Monitors[Value].Width]);
  lblHeight.Caption := Format('영역의 높이 : %d', [Screen.Monitors[Value].Height]);

  Top := Screen.Monitors[Value].Top + (Screen.Monitors[Value].Height - Height) div 2;
  Left := Screen.Monitors[Value].Left + (Screen.Monitors[Value].Width - Width) div 2;
end;

end.
