unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Timer: TTimer;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FEvent, FShift : string;
    FX, FY : integer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function ShiftStateToStr(AShift:TShiftState):string;
begin
  Result := '';

  if ssShift   in AShift then Result := Result + 'ssShift, ';
  if ssAlt     in AShift then Result := Result + 'ssAlt, ';
  if ssCtrl    in AShift then Result := Result + 'ssCtrl, ';
  if ssLeft    in AShift then Result := Result + 'ssLeft, ';
  if ssRight   in AShift then Result := Result + 'ssRight, ';
  if ssMiddle  in AShift then Result := Result + 'ssMiddle, ';
  if ssDouble  in AShift then Result := Result + 'ssDouble, ';
  if ssTouch   in AShift then Result := Result + 'ssTouch, ';
  if ssPen     in AShift then Result := Result + 'ssPen, ';
  if ssCommand in AShift then Result := Result + 'ssCommand, ';

  if Length(Result) > 0 then SetLength(Result, Length(Result)-2);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FEvent := '';
  FShift := '';
  FX := 0;
  FY := 0;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FEvent := 'MouseDown';
  FShift := ShiftStateToStr(Shift);
  FX := X;
  FY := Y;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FEvent := 'MouseMove';
  FShift := ShiftStateToStr(Shift);
  FX := X;
  FY := Y;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FEvent := 'MouseUp';
  FShift := ShiftStateToStr(Shift);
  FX := X;
  FY := Y;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := false;
  try
    Caption := Format('%s - %s, X: %d, Y: %d', [FEvent, FShift, FX, FY]);
  finally
    Timer.Enabled := true;
  end;
end;

end.
