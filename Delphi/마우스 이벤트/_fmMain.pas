unit _fmMain;

interface

uses
  DebugTools,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, BitmapWindow,
  Vcl.StdCtrls, SwitchButton;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    Label1: TLabel;
    SwitchButton: TSwitchButton;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDirection : integer;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FDirection := 1;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  Point : TPoint;
begin
  if not SwitchButton.SwitchOn then Exit;

  FDirection := FDirection * (-1);

  Point.X := FDirection * 5;
  Point.Y := 0;

  mouse_event( MOUSEEVENTF_MOVE, Point.X, Point.Y, 0, 0 );
end;

end.
