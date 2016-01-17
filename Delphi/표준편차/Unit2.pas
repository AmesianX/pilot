unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  Math;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  Result : double;
  Data: array of double;
begin
  SetLength(Data, 3);
  Data[0] := 33;
  Data[1] := 22;
  Data[2] := 11;
  Result:= StdDev(Data);
  ShowMessage('STD DEV='+FloatToStr(Result));
end;

end.
