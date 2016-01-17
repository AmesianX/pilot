unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Shape1: TShape;
    procedure Button1Click(Sender: TObject);
    procedure Memo1Exit(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
begin
  Memo1.Text:= '';
  Memo1.Left:= 10;
  Memo1.Top:=  10;
  Memo1.Height:= Canvas.TextHeight('Hy') * 2;
  Memo1.Visible:= true;
  Memo1.SetFocus;
end;

procedure TForm1.Memo1Exit(Sender: TObject);
begin
  Memo1.Visible:= false;
end;

procedure TForm1.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Loop, iWidth, iHeight : integer;
begin
  case Key of
    VK_ESCAPE : Memo1.Visible:= false;
  end;

  iWidth:= 0;
  for Loop := 0 to Memo1.Lines.Count - 1 do
    if Canvas.TextWidth(Memo1.Lines.Strings[Loop]) > iWidth then
      iWidth:= Canvas.TextWidth(Memo1.Lines.Strings[Loop]);
  iWidth:= iWidth + Canvas.TextWidth('XXX');
  if iWidth < 100 then iWidth:= 100;
  if iWidth <> Memo1.Width then Memo1.Width:= iWidth;

  iHeight:= Memo1.Lines.Count * Canvas.TextHeight('Hy');
  if iHeight = 0 then iHeight:= Canvas.TextHeight('Hy');
  iHeight:= iHeight + Canvas.TextHeight('Hy');
  if iHeight <> Memo1.Height then Memo1.Height:= iHeight;
end;

end.
