program Test;

uses
  Forms,
  _fmMain in '_fmMain.pas' {Form1},
  MultiIJL in 'MultiIJL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
