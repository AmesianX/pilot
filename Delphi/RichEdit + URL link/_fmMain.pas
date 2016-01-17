unit _fmMain;

interface

uses
  RichEditPlus, ShellAPI,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TRichEdit = class (TRichEditPlus);

  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    procedure FormCreate(Sender: TObject);
  private
    procedure on_TURLClick(Sender:TObject; const AURL:string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  RichEdit1.OnURLClick := on_TURLClick;

  RichEdit1.Lines.Add( 'http://www.himytv.com/' );
end;

procedure TForm1.on_TURLClick(Sender: TObject; const AURL: string);
begin
  ShellExecute(Handle, 'open', PChar(AURL), 0, 0, SW_SHOWNORMAL);
end;

end.
