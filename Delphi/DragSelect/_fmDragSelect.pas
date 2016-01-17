unit _fmDragSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

const
  SC_DRAG_RESIZEL = $f001; // left resize
  SC_DRAG_RESIZER = $f002; // right resize
  SC_DRAG_RESIZEU = $f003; // upper resize
  SC_DRAG_RESIZEUL = $f004; // upper-left resize
  SC_DRAG_RESIZEUR = $f005; // upper-right resize
  SC_DRAG_RESIZED = $f006; // down resize
  SC_DRAG_RESIZEDL = $f007; // down-left resize
  SC_DRAG_RESIZEDR = $f008; // down-right resize
  SC_DRAG_MOVE = $f012; // move

const
  HandleImageSize = 10;

type
  TSizeChangeEvent = procedure (Rect : TRect) of Object;
  
  TfmDragSelect = class(TForm)
    pnArea: TPanel;
    imLeftTop: TImage;
    imTop: TImage;
    imLeft: TImage;
    imRightTop: TImage;
    imBottom: TImage;
    imLeftBottom: TImage;
    imRightBottom: TImage;
    imRight: TImage;
    imCenter: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure pnAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnAreaMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imHandlesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imBottomMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FIsFirstDown : Boolean;
    FIsMouseDown : Boolean;
    FDownPoint : TPoint;
    FUpPoint : TPoint;

    FptArea: TPoint;
    FptLeft: TPoint;
    FptRight: TPoint;
    FptLeftBottom: TPoint;
    FptBottom: TPoint;
    FptRightBottom: TPoint;
    FptRightTop: TPoint;
    FptTop: TPoint;
    FptLeftTop: TPoint;

    FOldPanelWinProc : TWndMethod;
    FOnSizeChange: TSizeChangeEvent;
    
    procedure ReAlignRect(var Rect : TRect);
    procedure RePositionHandle;
    procedure WndProcForPanel(var Message: TMessage);
  public
  published
    property OnSizeChange : TSizeChangeEvent read FOnSizeChange write FOnSizeChange;
    { Public declarations }
  end;

var
  fmDragSelect: TfmDragSelect;

implementation

uses
   _fmDragExit;
   
{$R *.dfm}

procedure TfmDragSelect.FormCreate(Sender: TObject);
begin
  FIsFirstDown := true;
  FIsMouseDown := false;

  SetWindowLong(Handle, GWL_EXSTYLE,GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
  SetLayeredWindowAttributes(Handle, clPurple, 125,  LWA_COLORKEY or LWA_ALPHA);

  FOldPanelWinProc := pnArea.WindowProc;
  pnArea.WindowProc := WndProcForPanel;
end;

procedure TfmDragSelect.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FIsFirstDown <> true then exit;
  if Button <> mbLeft then exit;

  FIsFirstDown := false;
  FIsMouseDown := true;

  FDownPoint.X := X;
  FDownPoint.Y := Y;

  pnArea.Top := Y;
  pnArea.Left := X;
  pnArea.Width := 1;
  pnArea.Height := 1;
  pnArea.Visible := true;
end;

procedure TfmDragSelect.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Rect : TRect;
begin
  if FIsMouseDown <> true then exit;  

  Rect.Left := FDownPoint.X;
  Rect.Top := FDownPoint.Y;
  Rect.Right := X;
  Rect.Bottom := Y;

  ReAlignRect(Rect);
  MoveWindow(pnArea.Handle, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, true);
end;

procedure TfmDragSelect.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FIsMouseDown <> true then exit;
  if Button <> mbLeft then exit;

  FIsMouseDown := false;
  FUpPoint.X := X;
  FUpPoint.Y := Y;

  RePositionHandle;
end;

procedure TfmDragSelect.FormShow(Sender: TObject);
var
  R : TRect;
  ExitForm : TfmDragExit;
begin
  R.Top := GetSystemMetrics(SM_YVIRTUALSCREEN);
  R.Left := GetSystemMetrics(SM_XVIRTUALSCREEN);
  R.Bottom := R.Top + GetSystemMetrics(SM_CYVIRTUALSCREEN);
  R.Right := R.Left + GetSystemMetrics(SM_CXVIRTUALSCREEN);

  SetWindowPos(Handle, HWND_TOPMOST , R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, 0);

  ExitForm := TfmDragExit.Create(Self);
  ExitForm.DragSelect := Self;
  ExitForm.Show;
  ExitForm.BringToFront;
end;

procedure TfmDragSelect.imBottomMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  pnAreaMouseMove(pnArea, Shift, TImage(Sender).Left + X, TImage(Sender).Top + Y);
end;

procedure TfmDragSelect.imHandlesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pnAreaMouseDown(pnArea, Button, Shift, TImage(Sender).Left + X, TImage(Sender).Top + Y);
end;

procedure TfmDragSelect.pnAreaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  Limit = 10;
var
  SenderControl: TWinControl absolute Sender;
  SysCommWparam: integer;
begin
  if (X < HandleImageSize )and ( Y < HandleImageSize) then
  SysCommWparam := SC_DRAG_RESIZEUL
  else if(X > SenderControl.Width-HandleImageSize) and (Y > SenderControl.Height-HandleImageSize) then
  SysCommWparam := SC_DRAG_RESIZEDR
  else if(X < HandleImageSize) and (Y > SenderControl.Height-HandleImageSize) then
  SysCommWparam := SC_DRAG_RESIZEDL
  else if(X > SenderControl.Width-HandleImageSize ) and ( Y < HandleImageSize) then
  SysCommWparam := SC_DRAG_RESIZEUR
  else if(X < HandleImageSize) then
  SysCommWparam := SC_DRAG_RESIZEL
  else if(X > SenderControl.Width-HandleImageSize) then
  SysCommWparam := SC_DRAG_RESIZER
  else if(Y < HandleImageSize) then
  SysCommWparam := SC_DRAG_RESIZEU
  else if(Y > SenderControl.Height-HandleImageSize) then
  SysCommWparam := SC_DRAG_RESIZED
  else
  SysCommWparam := SC_DRAG_MOVE;
  
  ReleaseCapture;
  SendMessage(SenderControl.Handle, WM_SYSCOMMAND, SysCommWparam, 0);
end;

procedure TfmDragSelect.pnAreaMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  SenderControl: TWinControl absolute Sender;
begin
  if ((X < HandleImageSize) AND (Y < HandleImageSize))or ((X > SenderControl.Width-HandleImageSize) and (Y > SenderControl.Height-HandleImageSize)) then
    SenderControl.Cursor := crSizeNWSE
  else if ((X < HandleImageSize) and (Y > SenderControl.Height-HandleImageSize)) or ((X > SenderControl.Width-HandleImageSize) and (Y < HandleImageSize))then
    SenderControl.Cursor := crSizeNESW
  else if ((X < HandleImageSize )or (X > SenderControl.Width-HandleImageSize))then
    SenderControl.Cursor := crSizeWE
  else if ((Y < HandleImageSize) or (Y > SenderControl.Height-HandleImageSize))then
    SenderControl.Cursor := crSizeNS
  else
    SenderControl.Cursor := crDefault;
end;

procedure TfmDragSelect.ReAlignRect(var Rect: TRect);
var
  Left, Top, Right, Bottom : Integer;
  Temp : Integer;
begin
  Left := Rect.Left;
  Top := Rect.Top;
  Right := Rect.Right;
  Bottom := Rect.Bottom;

  if Left > Right then begin
    Temp := Left;
    Left := Right;
    Right := Temp;
  end;

  if Top > Bottom then begin
    Temp := Top;
    Top := Bottom;
    Bottom := Temp;
  end;
  
  SetRect(Rect, Left, Top, Right, Bottom);
end;

procedure TfmDragSelect.RePositionHandle;
const
  SmallGap = 5;
  LargeGap = 10;
var
  HighTop, MiddleTop, LowTop : Integer;
  HighLeft, MiddleLeft, LowLeft : Integer;
begin
//Calc Position
  HighTop := 0;
  MiddleTop := (pnArea.Height div 2) - SmallGap;
  LowTop := pnArea.Height - LargeGap;

  HighLeft := 0;
  MiddleLeft := (pnArea.Width div 2) - SmallGap;
  LowLeft := pnArea.Width - LargeGap;

//Set Position
  imLeftTop.Top := HighTop;
  imLeftTop.Left := HighLeft;
  imTop.Top := HighTop;
  imTop.Left := MiddleLeft;
  imRightTop.Top := HighTop;
  imRightTop.Left := LowLeft;

  imLeft.Top := MiddleTop;
  imLeft.Left := HighLeft;
  imRight.Top := MiddleTop;
  imRight.Left := LowLeft;

  imLeftBottom.Top := LowTop;
  imLeftBottom.Left := HighLeft;
  imBottom.Top := LowTop;
  imBottom.Left := MiddleLeft;
  imRightBottom.Top := LowTop;
  imRightBottom.Left := LowLeft;

  imCenter.Top := (pnArea.Height div 2) - 13;
  imCenter.Left := (pnArea.Width div 2) - 13;
//Show
  imLeftTop.Visible := true;
  imTop.Visible := true;
  imRightTop.Visible := true;
  imLeft.Visible := true;
  imRight.Visible := true;
  imLeftBottom.Visible := true;
  imBottom.Visible := true;
  imRightBottom.Visible := true;
  imCenter.Visible := true;

  if Assigned(FOnSizeChange) then
    FOnSizeChange(Rect(pnArea.Left, pnArea.Top, pnArea.Left + pnArea.Width, pnArea.Top + pnArea.Height));
end;

procedure TfmDragSelect.WndProcForPanel(var Message: TMessage);
begin
  if Message.Msg = WM_SIZING then begin
    RePositionHandle;
  end;

  FOldPanelWinProc(Message);
end;

end.
