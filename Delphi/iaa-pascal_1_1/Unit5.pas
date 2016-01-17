unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TForm5 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    BitBtn1: TBitBtn;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.DFM}

procedure TForm5.Button2Click(Sender: TObject);
begin
    Memo1.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+'gnu_gpl_english.txt');
end;

procedure TForm5.Button1Click(Sender: TObject);
begin
    Memo1.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+'gnu_gpl_polski.txt');
end;

procedure TForm5.FormShow(Sender: TObject);
begin
    Memo1.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+'licencja.txt');
end;

procedure TForm5.Button3Click(Sender: TObject);
begin
    Memo1.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+'licencja.txt');
end;

end.
