program Project1;

uses
  Forms,
  _fmMain in '_fmMain.pas' {Form1},
  WebCam in 'WebCam.pas' {frCam: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
