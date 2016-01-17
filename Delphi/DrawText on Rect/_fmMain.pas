unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormPaint(Sender: TObject);
const
  S = 'This is a sample text, I think, is it not?';
var
  r: TRect;
begin
  r := Rect(0, 0, 100, 0);
  DrawText(Canvas.Handle,
    PChar(S),
    Length(S),
    r,
    DT_LEFT or DT_WORDBREAK or DT_CALCRECT);

  DrawText(Canvas.Handle,
    PChar(S),
    Length(S),
    r,
    DT_LEFT or DT_WORDBREAK);
end;

end.


procedure TForm1.FormPaint(Sender: TObject);
const
  S = 'This is a sample text, I think, is it not?';
var
  r: TRect;
begin
  r := Rect(10, 10, 60, 60);
  DrawText(Canvas.Handle,
    PChar(S),
    Length(S),
    r,
    DT_LEFT or DT_WORDBREAK or DT_CALCRECT);

  DrawText(Canvas.Handle,
    PChar(S),
    Length(S),
    r,
    DT_LEFT or DT_WORDBREAK);
end;

procedure TForm1.FormPaint(Sender: TObject);
const
  S: array [0 .. 3] of string = ('Hi! How are you?',
    'I am fine, thanks. How are you? How are your kids?', 'Fine!',
    'Glad to hear that!');
  Colors: array [boolean] of TColor = (clMoneyGreen, clSkyBlue);
  Aligns: array [boolean] of integer = (DT_RIGHT, DT_LEFT);
var
  i, y, MaxWidth, RectWidth: integer;
  r, r2: TRect;
begin
  y := 10;
  MaxWidth := ClientWidth div 2;

  for i := low(S) to high(S) do begin
    Canvas.Brush.Color := Colors[Odd(i)];

    r := Rect(10, y, MaxWidth, 16);
    DrawText(Canvas.Handle, PChar(S[i]), Length(S[i]), r, Aligns[Odd(i)] or
      DT_WORDBREAK or DT_CALCRECT);

    if not Odd(i) then begin
      RectWidth := r.Right - r.Left;
      r.Right := ClientWidth - 10;
      r.Left := r.Right - RectWidth;
    end;

    r2 := Rect(r.Left - 4, r.Top - 4, r.Right + 4, r.Bottom + 4);
    Canvas.RoundRect(r2, 5, 5);

    DrawText(Canvas.Handle, PChar(S[i]), Length(S[i]), r, Aligns[Odd(i)] or
      DT_WORDBREAK);

    y := r.Bottom + 10;
  end;
end;

