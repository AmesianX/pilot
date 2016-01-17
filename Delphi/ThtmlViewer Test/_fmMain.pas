unit _fmMain;

interface

uses
  CodeSiteLogging,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, HTMLUn2, HtmlView;

type
  TfmMain = class(TForm)
    HtmlViewer: THtmlViewer;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  HtmlViewer.LoadFromFile('D:\Projects\Lib\HtmlViewer\Demos\Compiled Framedemo\Demo.htm');
end;

end.
