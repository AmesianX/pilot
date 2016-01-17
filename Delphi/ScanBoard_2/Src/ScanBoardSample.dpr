program ScanBoardSample;

uses
  Forms,
  _fmMain in 'View\_fmMain.pas' {fmMain},
  View in 'View\View.pas',
  Option in 'Option\Option.pas',
  Global in 'Global\Global.pas',
  Capture in 'Lib\Capture.pas',
  Slice in 'Lib\Slice.pas',
  EncoderList in 'Lib\EncoderList.pas',
  ScanBoard in 'Lib\ScanBoard.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
