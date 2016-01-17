unit _fmMain;

interface

uses
  SearchDir, Disk,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    moMsg: TMemo;
    btStart: TButton;
    procedure btStartClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
var
  sTargetPath: string;
begin
  SearchFiles(
    GetExecPath, false,
    procedure (Path:string; SearchRec:TSearchRec; var NeedStop:boolean) begin
      moMsg.Lines.Add(SearchRec.Name);

      sTargetPath := FormatDateTime('yyyy.mm.dd', SearchRec.TimeStamp);
      sTargetPath := StringReplace(sTargetPath, ' ', '0', [rfReplaceAll]);
      sTargetPath := Path + sTargetPath + '\';

      ForceDirectories(sTargetPath);

      MoveFileEx(
        PChar(Path + SearchRec.Name),
        PChar(sTargetPath + SearchRec.Name),
        MOVEFILE_COPY_ALLOWED or MOVEFILE_REPLACE_EXISTING
      );
    end
  )
end;

end.
