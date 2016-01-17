program RndChoice;

uses
  Forms,
  frmMain in 'frmMain.pas' {fmMain},
  RandomChoice in 'RandomChoice.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
