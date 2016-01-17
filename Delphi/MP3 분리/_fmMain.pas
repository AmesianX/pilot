unit _fmMain;

interface

uses
  Disk, Strg,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  Loop: Integer;
  FileName : string;
begin
  for Loop := 0 to moMsg.Lines.Count-1 do begin
    FileName := Strg.DeleteLeftPlus(moMsg.Lines[Loop], '\');
//    ShowMessage('D:\MP3\POP\BillBoard Top USA Singles\' +  moMsg.Lines[Loop]);
//    ShowMessage(FileName);
    CopyFile(PChar('D:\MP3\POP\BillBoard Top USA Singles\' +  moMsg.Lines[Loop]), PChar('D:\MP3\POP\BillBoard Top USA Singles\' + FileName), false);
  end;

end;

end.
