unit _fmMain;

interface

uses
  RyuThread,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FList : TList;
  public
  end;

var
  Form1: TForm1;

implementation

type
  TNewThread = class (TRyuThread)
  private
  protected
    procedure Execute; override;
  public
  end;

{$R *.dfm}

{ TNewThread }

procedure TNewThread.Execute;
begin
  repeat
    Sleep(10);

//    Form1.Caption := IntToStr(Form1.Tag);
//    Form1.Tag := Form1.Tag + 1;
  until Terminated;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Loop: Integer;
begin
//  TNewThread.Create(false);
  for Loop := 1 to 1024 * 32 do FList.Add(TNewThread.Create(false));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TList.Create;
end;

end.
