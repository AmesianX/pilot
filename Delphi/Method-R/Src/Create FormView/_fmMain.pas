unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TfmMain = class(TForm)
    PageControl: TPageControl;
    tsUnit: TTabSheet;
    tsDFM: TTabSheet;
    moUnit: TMemo;
    moDFM: TMemo;
  private
  public
    class function Obj:TfmMain;

    procedure Install;
    procedure SaveFilesTo(APath,AFormName:string);
  end;

implementation

uses
  Disk, Sys;

var
  MyObject : TfmMain = nil;

{$R *.dfm}

procedure TfmMain.Install;
var
  sPath : string;
begin
//[HKEY_LOCAL_MACHINE\SOFTWARE\Classes\Folder\shell\Method_R_CreateFormView]
//@="Method-R CreateFormView"
//[HKEY_LOCAL_MACHINE\SOFTWARE\Classes\Folder\shell\Method_R_CreateFormView\command]
//@="Path\\CreateFormView.exe %1"

  sPath := StringReplace(GetExecPath, '\', '\\', [rfReplaceAll]);
  AddContextMenu('Method_R_CreateFormView', 'Method-R Create FormView', sPath+'CreateFormView.exe "%1"');
end;

class function TfmMain.Obj: TfmMain;
begin
  if MyObject = nil then MyObject := TfmMain.Create(nil);
  Result := MyObject;
end;

procedure TfmMain.SaveFilesTo(APath, AFormName: string);
var
  sUnit, sDFM : string;
begin
  sUnit := StringReplace(moUnit.Text, '@FormName', AFormName, [rfReplaceAll]);
  sDFM  := StringReplace(moDFM.Text , '@FormName', AFormName, [rfReplaceAll]);

  if Copy(APath, Length(APath), 1) <> '\' then APath := APath + '\';
  SaveTextToFile(APath + Format('_%s.pas', [AFormName]), sUnit);
  SaveTextToFile(APath + Format('_%s.dfm', [AFormName]), sDFM);
end;

end.
