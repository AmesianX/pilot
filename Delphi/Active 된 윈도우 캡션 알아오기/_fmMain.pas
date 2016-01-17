unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Timer1Timer(Sender: TObject);
var
  hActive : THandle;
  Title : string;
  Len : integer;
begin
  hActive := GetForegroundWindow;

  Len := GetWindowTextLength(hActive) + 1;
  SetLength(Title, Len);
  GetWindowText(hActive, PChar(Title), Len);

  Caption := Title;
end;

end.
