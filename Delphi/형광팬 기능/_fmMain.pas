unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function BrightenColor(Color:TColor):TColor;
const
  Weight  = $0F * 2;
var
  R, G, B : byte;
  bCondition : boolean;
begin
  R:= (Color and $0000FF);
  G:= (Color and $00FF00) shr  8;
  B:= (Color and $FF0000) shr 16;

  if R <= ($FF - Weight) then R:= R + Weight;
  if G <= ($FF - Weight) then G:= G + Weight;

  bCondition:= (R >= $FF) and (G >= $FF) and (B >= $FF);
  if bCondition then Result:= $FFFF
  else Result:= (B shl 16) + (G shl 8) + R;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Loop: integer;
begin
  for Loop := 1 to 300 do
    Image1.Picture.Bitmap.Canvas.Pixels[Loop, Loop]:=
      BrightenColor(Image1.Picture.Bitmap.Canvas.Pixels[Loop, Loop]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  Image1.Picture.Bitmap.Canvas.Pen.Width:= 4;
//  Image1.Picture.Bitmap.Canvas.Pen.Color:= $003030;
//  Image1.Picture.Bitmap.Canvas.Pen.Mode:= pmMerge;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//  if Shift = [ssLeft] then Image1.Picture.Bitmap.Canvas.MoveTo(X, Y);
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
//  if Shift = [ssLeft] then Image1.Picture.Bitmap.Canvas.LineTo(X, Y);
  if Shift = [ssLeft] then Image1.Picture.Bitmap.Canvas.LineTo(X, Y);
end;

end.
