program Test;

uses
  Forms,
  frmMain in 'frmMain.pas' {Form8},
  freBar in 'freBar.pas' {Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
