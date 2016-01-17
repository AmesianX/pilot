unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TfmMain = class(TForm)
    btSub: TSpeedButton;
    Edit1: TEdit;
    procedure btSubClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  _fmSub;

{$R *.dfm}

procedure TfmMain.btSubClick(Sender: TObject);
begin
  fmSub.ShowWithoutFocus;
  fmSub.Parent := Self;
  fmSub.Left := 0;
  fmSub.Top  := 0;
end;

end.
