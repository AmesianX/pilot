unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, HTMLUn2,
  HtmlView;

type
  TForm1 = class(TForm)
    htmlvwr1: THtmlViewer;
    pnl1: TPanel;
    bt1: TButton;
    procedure bt1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.bt1Click(Sender: TObject);
begin
  htmlvwr1.DocumentSource := 'Hi';
end;

end.
