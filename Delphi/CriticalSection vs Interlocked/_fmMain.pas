unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, syncObjs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FCS : TCriticalSection;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  i, Loop : integer;
  Tick : Cardinal;
begin
  i := 0;
  Tick := GetTickCount;
  for Loop := 1 to $FFFFFF do begin
    FCS.Enter;
    try
//      i := i + 1;
      Inc(i);
    finally
      FCS.Leave;
    end;
  end;
  Caption := Format('%d, %d', [GetTickCount-Tick, i]);
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  i, Loop : integer;
  Tick : Cardinal;
begin
  i := 0;
  Tick := GetTickCount;
  for Loop := 1 to $FFFFFF do begin
    InterlockedIncrement(i);
  end;
  Caption := Format('%d, %d', [GetTickCount-Tick, i]);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FCS := TCriticalSection.Create;
end;

end.
