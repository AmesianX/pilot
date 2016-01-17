unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    moMsg: TMemo;
    btStart: TButton;
    procedure btStartClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

function GetEMail(AText:string):string;
begin
  Result := '';
  if Pos('@', AText) = 0 then Exit;

  Result := AText;
end;

procedure TfmMain.btStartClick(Sender: TObject);
var
  sLine : string;
  List : TStringList;
  Loop: Integer;
begin
  List := TStringList.Create;
  try
    List.Delimiter := ' ';
    List.DelimitedText := moMsg.Text;

    moMsg.Clear;

    for Loop := 0 to List.Count-1 do begin
      sLine := GetEMail(List[Loop]);

      sLine := StringReplace( sLine, ',', '', [rfReplaceAll] );

      if sLine <> '' then moMsg.Lines.Add(sLine + ',');
    end;
  finally
    List.Free;
  end;
end;

end.
