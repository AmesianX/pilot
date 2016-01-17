program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {Form1},
  _frPlayer in '_frPlayer.pas' {frPlayer: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
