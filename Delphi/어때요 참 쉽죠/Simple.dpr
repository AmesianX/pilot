program Simple;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
