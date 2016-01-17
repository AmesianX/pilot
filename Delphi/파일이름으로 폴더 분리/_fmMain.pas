unit _fmMain;

interface

uses
  Disk, SearchDir,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    moMsg: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSearchDir : TSearchDir;
    procedure on_FindFile(Sender:TObject; APath:string; AFileInfo:TSearchRec);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  FSearchDir.Search('D:\Temp\823WGTMA', '*.*');
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FSearchDir := TSearchDir.Create(Self);
  FSearchDir.OnFindFile := on_FindFile;

  ForceDirectories(GetExecPath + 'Data')
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearchDir);
end;

procedure TfmMain.on_FindFile(Sender: TObject; APath: string;
  AFileInfo: TSearchRec);
var
  strTime, strFolder : string;
begin
  strTime := FormatDateTime('yyyy.mm.dd', AFileInfo.TimeStamp);
  moMsg.Lines.Add(Format('%s%s: %s', [APath, AFileInfo.Name, strTime]));

  strFolder := GetExecPath + 'Data\' + strTime + '\';
  ForceDirectories(strFolder);

  CopyFile(PChar(APath + AFileInfo.Name), PChar(strFolder + AFileInfo.Name), true);
end;

end.
