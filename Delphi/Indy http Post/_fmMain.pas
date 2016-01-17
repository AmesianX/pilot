unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, ExtCtrls;

type
  TfmMain = class(TForm)
    IdHTTP: TIdHTTP;
    moData: TMemo;
    Panel1: TPanel;
    btPost: TButton;
    procedure btPostClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  httpUtils;

{$R *.dfm}

procedure TfmMain.btPostClick(Sender: TObject);
begin
  ShowMessage(httpPost('http://demo.codeway.co.kr/test.php', moData.Text));
end;

end.

