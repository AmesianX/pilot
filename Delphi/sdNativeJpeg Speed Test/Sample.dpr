program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {Form1},
  NativeJpeg in 'Lib\NativeJpeg.pas',
  sdJpegBaseline in 'Lib\sdJpegBaseline.pas',
  sdJpegBitstream in 'Lib\sdJpegBitstream.pas',
  sdJpegBlockCoder in 'Lib\sdJpegBlockCoder.pas',
  sdJpegColors in 'Lib\sdJpegColors.pas',
  sdJpegConsts in 'Lib\sdJpegConsts.pas',
  sdJpegDCT in 'Lib\sdJpegDCT.pas',
  sdJpegDCTAccurate in 'Lib\sdJpegDCTAccurate.pas',
  sdJpegDCTFast in 'Lib\sdJpegDCTFast.pas',
  sdJpegFormat in 'Lib\sdJpegFormat.pas',
  sdJpegHuffman in 'Lib\sdJpegHuffman.pas',
  sdJpegLossless in 'Lib\sdJpegLossless.pas',
  sdJpegMarkers in 'Lib\sdJpegMarkers.pas',
  sdJpegProgressive in 'Lib\sdJpegProgressive.pas',
  sdJpegTypes in 'Lib\sdJpegTypes.pas',
  sdBitmapConversion in 'Lib\sdBitmapConversion.pas',
  sdBitmapResize in 'Lib\sdBitmapResize.pas',
  sdMapIterator in 'Lib\sdMapIterator.pas',
  SortedLists in 'Lib\SortedLists.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
