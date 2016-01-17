unit _fmmsScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfmmsScreen = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FMonitorNum: Integer;
    procedure SetMonitorNum(const Value: Integer);
  public
    property MonitorNum : Integer read FMonitorNum write SetMonitorNum;
  end;

var
  fmmsScreen: TfmmsScreen;

implementation

uses
  _fmmsSelector;

{$R *.dfm}

{ TfmmsScreen }

procedure TfmmsScreen.FormActivate(Sender: TObject);
begin
  SetForegroundWindow(TForm(Owner).Handle);
end;

procedure TfmmsScreen.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfmmsScreen.FormCreate(Sender: TObject);
begin
  FMonitorNum := -1;
  SetWindowLong(Handle, GWL_EXSTYLE,GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
  SetLayeredWindowAttributes(Handle, clPurple,200,  LWA_COLORKEY or LWA_ALPHA);
end;

procedure TfmmsScreen.FormMouseEnter(Sender: TObject);
begin
  Invalidate;
end;

procedure TfmmsScreen.FormMouseLeave(Sender: TObject);
begin
  Invalidate;
end;

procedure TfmmsScreen.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TfmmsSelector(Owner).MonitorNum := MonitorNum;
end;

procedure TfmmsScreen.FormPaint(Sender: TObject);
begin
  if TfmmsSelector(Owner).MonitorNum <> MonitorNum then exit;

  Canvas.Pen.Width := 20;
  Canvas.Pen.Color := clRed;
  Canvas.Brush.Style := bsClear;

  Canvas.Rectangle(ClientRect);
end;

procedure TfmmsScreen.FormShow(Sender: TObject);
var
  R : TRect;
begin
  Windows.GetWindowRect(Handle, R);
  SetWindowPos(Handle, HWND_TOPMOST , R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, 0);
end;

procedure TfmmsScreen.SetMonitorNum(const Value: Integer);
begin
  FMonitorNum := Value;
end;

end.
