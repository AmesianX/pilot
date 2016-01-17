unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    edRGB: TEdit;
    edHSB: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

type
  TRGBColor = record
    Red,
    Green,
    Blue : Byte;
  end;

  THSBColor = record
    Hue,
    Saturnation,
    Brightness : Double;
  end;


function RGBToHSB(RGBColor:TRGBColor):THSBColor;
var
  h , s , b : Double;
  minRGB, maxRGB, Delta : Double;
begin
  h := 0.0;

  minRGB := Min(Min(RGBColor.Red, RGBColor.Green), RGBColor.Blue);
  maxRGB := Max(Max(RGBColor.Red, RGBColor.Green), RGBColor.Blue);
  b := maxRGB;

  Delta := ( maxRGB - minRGB ) ;
  if (maxRGB <> 0.0) then s := 255.0 * Delta / maxRGB
  else s := 0.0;

  if (s <> 0.0) then begin
    if RGBColor.Red = maxRGB then h := (RGBColor.Green - RGBColor.Blue) / Delta
    else if RGBColor.Green = maxRGB then h := 2.0 + (RGBColor.Blue - RGBColor.Red) / Delta
    else if RGBColor.Blue = maxRGB then h := 4.0 + (RGBColor.Red - RGBColor.Green) / Delta
  end else h := -1.0;

  h := h * 60;
  if h < 0.0 then h := h + 360.0;

  with result do begin
    Hue := h;
    Saturnation := s * 100 / 255;
    Brightness := b * 100 / 255;
  end;
end;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  RGB : TStringList;
  RGBColor : TRGBColor;
  HSBColor : THSBColor;
begin
  RGB := TStringList.Create;
  try
    RGB.Delimiter := ',';
    RGB.StrictDelimiter := true;
    RGB.DelimitedText := Trim(edRGB.Text);

    RGBColor.Red := StrToIntDef(RGB[0], 0);
    RGBColor.Green := StrToIntDef(RGB[1], 0);
    RGBColor.Blue := StrToIntDef(RGB[2], 0);

    HSBColor := RGBToHSB(RGBColor);

    with HSBColor do edHSB.Text := Format('%f, %f, %f', [Hue, Saturnation, Brightness]);
  finally
    RGB.Free;
  end;
end;

end.

