program Sample;

uses
  Forms,
  _fmmsScreen in '_fmmsScreen.pas' {fmmsScreen},
  _fmmsSelector in '_fmmsSelector.pas' {fmmsSelector};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmmsSelector, fmmsSelector);
  Application.Run;
end.
