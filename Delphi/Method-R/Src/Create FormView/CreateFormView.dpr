program CreateFormView;

uses
  Forms,
  SysUtils,
  Dialogs,
  _fmMain in '_fmMain.pas' {fmMain};

{$R *.res}

var
  FormName : string;

begin
  if ParamCount <> 1 then Exit;

  if LowerCase(ParamStr(1)) = '/install' then begin
    TfmMain.Obj.Install;
    Exit;
  end;

  FormName := InputBox('Method-R Create FormView', 'Form Name', '');
  if FormName = '' then Exit;

  TfmMain.Obj.SaveFilesTo(ParamStr(1), FormName);
end.
