program CreateFrameView;

uses
  Forms,
  SysUtils,
  Dialogs,
  _fmMain in '_fmMain.pas' {fmMain};

{$R *.res}

var
  FrameName : string;

begin
  if ParamCount <> 1 then Exit;

  if LowerCase(ParamStr(1)) = '/install' then begin
    TfmMain.Obj.Install;
    Exit;
  end;

  FrameName := InputBox('Method-R Create FrameView', 'Frame Name', '');
  if FrameName = '' then Exit;

  TfmMain.Obj.SaveFilesTo(ParamStr(1), FrameName);
end.
