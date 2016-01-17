unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  Tick : Cardinal;
  Loop1: Integer;
  Loop2: Integer;
  Count : integer;
begin
  // 5.7 배 정도의 차이가 남
  Tick := GetTickCount;
  Count := 0;
  for Loop1 := 1 to 10000 do
  for Loop2 := 1 to 100000 do Count := Count + 1;
  Memo1.Lines.Add(Format('Count := Count + 1 - %d, %d', [GetTickCount-Tick, Count]));

  Tick := GetTickCount;
  Count := 0;
  for Loop1 := 1 to 10000 do
  for Loop2 := 1 to 100000 do InterlockedIncrement(Count);
  Memo1.Lines.Add(Format('InterlockedIncrement(Count) - %d, %d', [GetTickCount-Tick, Count]));
end;

end.
