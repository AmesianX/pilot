program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {Form1},
  ScreenSaverUtils in 'ScreenSaverUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
