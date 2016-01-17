program Lotto;

uses
  Forms,
  _dmMain in '_dmMain.pas' {dmMain: TDataModule},
  _fmMain in '_fmMain.pas' {fmMain},
  RandNo in 'RandNo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.Run;
end.
