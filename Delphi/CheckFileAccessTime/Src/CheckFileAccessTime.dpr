program CheckFileAccessTime;

uses
  Forms,
  _fmMain in 'View\_fmMain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := '���� ���� ���� ��� �ӵ� ���ϱ�';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
