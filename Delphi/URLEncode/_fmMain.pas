unit _fmMain;

interface

uses
  IdGlobal, IdURI,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
//  Caption := TIdURI.URLEncode( 'Name: 류종택', IndyTextEncoding_UTF8 );
  Label1.Caption := TIdURI.ParamsEncode( 'Name: 류종택', IndyTextEncoding_UTF8 );
  Label2.Caption := TIdURI.URLDecode( Label1.Caption );
end;

end.
