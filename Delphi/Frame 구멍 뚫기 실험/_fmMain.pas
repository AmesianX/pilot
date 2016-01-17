unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, _frMain, Vcl.StdCtrls, Vcl.Buttons;

type
  TfmMain = class(TForm)
    frMain: TfrMain;
    Edit1: TEdit;
    btMakeHole: TSpeedButton;
    procedure btMakeHoleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btMakeHoleClick(Sender: TObject);
begin
  frMain.MakeHole;
  frMain.Visible := true;
end;

end.
