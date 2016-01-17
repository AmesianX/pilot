program HanKey;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := false;
  Application.ShowMainForm:= false;
  Application.Title := '한글키 강제 전환 Shift+Space';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
