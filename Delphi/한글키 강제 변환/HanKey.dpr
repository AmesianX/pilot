program HanKey;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := false;
  Application.ShowMainForm:= false;
  Application.Title := '�ѱ�Ű ���� ��ȯ Shift+Space';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
