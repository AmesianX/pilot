unit _fmMain;

interface

uses
  ValueList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FValueList : TValueList;
    procedure on_Change(Sender:TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FValueList.Values['x'] := '1';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FValueList.Integers['y'] := 2;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FValueList := TValueList.Create;
  FValueList.OnChange := on_Change;
end;

procedure TForm1.on_Change(Sender: TObject);
begin
  Caption := IntToStr(GetTickCount);
end;

end.
