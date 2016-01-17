unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    OpenDialog: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  OfficeTools, Disk;

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  TmpPPT: variant;
  sTmp: string;
  sTmpFileExt: string;
  iTmpPos: Integer;
begin
  if not OpenDialog.Execute then Exit;

  PPT2JPG(OpenDialog.FileName, GetExecPath + 'Test');
end;

end.
