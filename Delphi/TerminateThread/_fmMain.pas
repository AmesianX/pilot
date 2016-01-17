unit _fmMain;

interface

uses
  SimpleThread,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FSimpleThread : TSimpleThread;
    procedure on_Repeat(Sender:TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FSimpleThread.TerminateNow;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSimpleThread := TSimpleThread.Create(on_Repeat);
end;

procedure TForm1.on_Repeat(Sender: TObject);
begin
  while not FSimpleThread.Terminated do begin
    Caption := IntToStr(Tag);
    Tag := Tag + 1;

    Sleep(100);

//    FSimpleThread.SleepTight;
  end;
end;

end.
