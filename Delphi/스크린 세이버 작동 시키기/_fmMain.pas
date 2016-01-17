unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ScreenSaverUtils;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  SetScreenSaverOn(GetSystemPath+'ssbezier.scr', 60, true);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SetScreenSaverOff;
end;

end.
