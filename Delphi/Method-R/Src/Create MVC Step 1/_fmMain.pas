unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TfmMain = class(TForm)
    PageControl: TPageControl;
    tsGlobal: TTabSheet;
    moGlobal: TMemo;
    tsView: TTabSheet;
    moView: TMemo;
    tsOption: TTabSheet;
    moOption: TMemo;
    tsMainUnit: TTabSheet;
    tsMainForm: TTabSheet;
    moMainUnit: TMemo;
    moMainForm: TMemo;
    tsApplication: TTabSheet;
    moApplication: TMemo;
  private
  public
    class function Obj:TfmMain;

    procedure Install;
    procedure SaveFilesTo(APath,AProjectName:string);
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
  sPath := StringReplace(GetExecPath, '\', '\\', [rfReplaceAll]);
  AddContextMenu('Method_R_CreateMVC01', 'Method-R Create MVC Step-01', sPath+'CreateMVC01.exe "%1"');
end;

class function TfmMain.Obj: TfmMain;
begin
  if MyObject = nil then MyObject := TfmMain.Create(nil);
  Result := MyObject;
end;

procedure TfmMain.SaveFilesTo(APath, AProjectName: string);
begin
  if Copy(APath, Length(APath), 1) <> '\' then APath := APath + '\';

  ForceDirectories(APath + 'Dialogs');
  ForceDirectories(APath + 'Frames');
  ForceDirectories(APath + 'Globals');
  ForceDirectories(APath + 'Options');
  ForceDirectories(APath + 'Views');

  moApplication.Text := StringReplace(moApplication.Text, '@PrjectName', AProjectName, []);
  moApplication.Lines.SaveToFile(APath + AProjectName + '.dpr');

  moGlobal.Lines.SaveToFile(APath + 'Globals\Global.pas');
  moOption.Lines.SaveToFile(APath + 'Options\Option.pas');
  moView.Lines.SaveToFile(APath + 'Views\View.pas');
  moMainUnit.Lines.SaveToFile(APath + 'Views\_fmMain.pas');
  moMainForm.Lines.SaveToFile(APath + 'Views\_fmMain.dfm');
end;

end.

