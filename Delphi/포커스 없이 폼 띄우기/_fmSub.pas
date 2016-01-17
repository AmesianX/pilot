unit _fmSub;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TfmSub = class(TForm)
  private
  public
    procedure ShowWithoutFocus;
  end;

var
  fmSub: TfmSub;

implementation

{$R *.dfm}

{ TfmSub }

procedure TfmSub.ShowWithoutFocus;
begin
  ShowWindow( Handle, SW_SHOWNOACTIVATE );
  Visible := True;
end;

end.
