program CreateMVC01;

uses
  Forms,
  SysUtils,
  Dialogs,
  _fmMain in '_fmMain.pas' {fmMain};

{$R *.res}

var
  ProjectName : string;

begin
  if ParamCount <> 1 then Exit;

  if LowerCase(ParamStr(1)) = '/install' then begin
    TfmMain.Obj.Install;
    Exit;
  end;

  ProjectName := InputBox('Method-R Create MVC Framework Step-01', 'Project Name', '');
  if ProjectName = '' then Exit;

  TfmMain.Obj.SaveFilesTo(ParamStr(1), ProjectName);
end.

