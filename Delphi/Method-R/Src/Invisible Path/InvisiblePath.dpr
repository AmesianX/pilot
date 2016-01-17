program InvisiblePath;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  ClassList in '..\..\Lib\ClassList.pas',
  ClassDefines in '..\..\Lib\ClassDefines.pas',
  PascalParser in '..\..\Lib\PascalParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
