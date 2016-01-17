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
    procedure SaveFilesTo(APath,AFrameName:string);
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
  AddContextMenu('Method_R_CreateFrameView', 'Method-R Create FrameView', sPath+'CreateFrameView.exe "%1"');
end;

class function TfmMain.Obj: TfmMain;
begin
  if MyObject = nil then MyObject := TfmMain.Create(nil);
  Result := MyObject;
end;

procedure TfmMain.SaveFilesTo(APath, AFrameName: string);
var
  sUnit, sDFM : string;
begin
  sUnit := StringReplace(moUnit.Text, '@FrameName', AFrameName, [rfReplaceAll]);
  sDFM  := StringReplace(moDFM.Text , '@FrameName', AFrameName, [rfReplaceAll]);

  if Copy(APath, Length(APath), 1) <> '\' then APath := APath + '\';
  SaveTextToFile(APath + Format('_%s.pas', [AFrameName]), sUnit);
  SaveTextToFile(APath + Format('_%s.dfm', [AFrameName]), sDFM);
end;

end.
