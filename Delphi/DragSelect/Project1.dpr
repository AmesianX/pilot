program Project1;

uses
  Forms,
  _fmDragExit in '_fmDragExit.pas' {fmDragExit},
  _fmDragSelect in '_fmDragSelect.pas' {fmDragSelect};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmDragSelect, fmDragSelect);
  Application.Run;
end.
