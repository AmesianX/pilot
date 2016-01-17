unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

function PosFrom(Sub,Str:string; From:integer):integer;
begin
  Result:= Pos(Sub, Copy(Str, From, Length(Str)));
end;

function SelectString(Memo:TMemo; Text:string; From:integer):integer;
begin
  Result:= PosFrom(Text, Memo.Text, From);
  if Result = 0 then Exit;

  Memo.SetFocus;
  Memo.SelStart:= Result - 1;
  Memo.SelLength:= Length(Text);
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  sFindText : string;
  iIndex : integer;
begin
  sFindText:= 'Ryu';

  iIndex:= SelectString(Memo1, sFindText, 1);
  while iIndex > 0 do begin
    iIndex:= SelectString(Memo1, sFindText, iIndex+Length(sFindText));
    Sleep(2000);
  end;

  MessageDlg('문자열을 모두 검색하였습니다.', mtInformation, [mbOk], 0);
end;

end.
