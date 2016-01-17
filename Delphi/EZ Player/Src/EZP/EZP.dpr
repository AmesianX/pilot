program EZP;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  RyuMPEG in '..\..\Lib\RyuMPEG.pas',
  VideoDecoder in '..\..\Lib\VideoDecoder.pas',
  AudioDecoder in '..\..\Lib\AudioDecoder.pas',
  msacm2 in '..\..\Lib\msacm2.pas',
  VoiceZipUtils in '..\..\Lib\VoiceZipUtils.pas',
  WaveOut in '..\..\Lib\WaveOut.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
