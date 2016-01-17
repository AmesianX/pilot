program RecordSizeUI;

uses
  Forms,
  _frRecordSize in '_frRecordSize.pas' {frRecordSize: TFrame},
  _fmMain in '_fmMain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
