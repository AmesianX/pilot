program TomsBaby;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  _frShellControl in '_frShellControl.pas' {frShellControl: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
