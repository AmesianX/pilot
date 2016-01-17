unit _fmMain;

interface

uses
  RyuGraphics,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  Loop,
  iLeft, iTop, iWidth, iHeight : integer;
begin
  iLeft   := 30;
  iTop    := 30;
  iWidth  := 100;
  iHeight := 100;

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clLime;

  for Loop := 1 to 10 do begin
    Canvas.Rectangle(iLeft, iTop, iWidth, iHeight);

    Dec(iLeft);
    Dec(iTop);
    Inc(iWidth);
    Inc(iHeight);

    Canvas.Pen.Color := SetBright(clLime, 100 - Loop * 5);
  end;
end;

end.
