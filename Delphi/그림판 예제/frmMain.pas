unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DrawShape, StdCtrls;

type
  TForm1 = class(TForm)
    imgMain: TImage;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure imgMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    DrawShape : TADrawShape;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  DrawShape.Brush.Style:= bsClear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  imgMain.Picture.Bitmap.Width:= imgMain.Width;
  imgMain.Picture.Bitmap.Height:= imgMain.Height;

  DrawShape:= TDrawCircle.Create(imgMain.Picture.Bitmap.Canvas);
end;

procedure TForm1.imgMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DrawShape.BeginDraw(X, Y);
end;

procedure TForm1.imgMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if DrawShape.IsDrawing = true then
     DrawShape.Drawing(X, Y);
end;

procedure TForm1.imgMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DrawShape.EndDraw(X, Y);
end;

end.
