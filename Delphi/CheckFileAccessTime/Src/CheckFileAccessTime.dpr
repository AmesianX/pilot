program CheckFileAccessTime;

uses
  Forms,
  _fmMain in 'View\_fmMain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := '파일 랜덤 접근 평균 속도 구하기';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
