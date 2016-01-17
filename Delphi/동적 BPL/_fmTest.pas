unit _fmTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmTest = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
  public
  end;

var
  fmTest: TfmTest;

implementation

{$R *.dfm}

procedure TfmTest.Timer1Timer(Sender: TObject);
begin
  Caption := FormatDateTime( 'hh:nn:ss', Now );
end;

initialization
  RegisterClass(TfmTest);
finalization
  UnRegisterClass(TfmTest);
end.
