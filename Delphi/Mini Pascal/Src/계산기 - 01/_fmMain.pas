unit _fmMain;

interface

uses
  ParserUtils, Strg, Scanner, Infix2Postfix, PolishParser,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    procedure moMsgKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FScanner : TScanner;
    FInfix2Postfix : TInfix2Postfix;
    FPolishParser : TPolishParser;
    procedure on_NewToken(Sender:TObject; AIndex:integer; ATokenType:TScannerTokenType; AText:string);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FScanner := TScanner.Create(Self);
  FScanner.OnToken := on_NewToken;

  FInfix2Postfix := TInfix2Postfix.Create;
  FPolishParser := TPolishParser.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScanner);
  FreeAndNil(FInfix2Postfix);
  FreeAndNil(FPolishParser);
end;

procedure TfmMain.moMsgKeyPress(Sender: TObject; var Key: Char);
var
  sLine : string;
  iIndex : integer;
begin
  if Key = '=' then begin
    Key := #0;

    iIndex := moMsg.Perform(EM_LINEFROMCHAR, moMsg.SelStart, 0);
    sLine := moMsg.Lines[iIndex];

    if Pos('=', sLine) > 0 then sLine := DeleteRightPlus(sLine, '=');

    sLine := Trim(sLine);

    // 후위법으로 변환
    FInfix2Postfix.Clear;
    FScanner.Execute(sLine);
    FInfix2Postfix.EndOfLine;

    // 후위법을 파싱하여 계산
    try
//      moMsg.Text := FInfix2Postfix.Polish.Text;

      FPolishParser.Assign(FInfix2Postfix.Polish);
      moMsg.Lines[iIndex] := sLine + ' = ' + FloatToStr(FPolishParser.Result);
    except
      moMsg.Lines[iIndex] := sLine + ' = Error!';
    end;
  end;
end;

procedure TfmMain.on_NewToken(Sender: TObject; AIndex: integer;
  ATokenType: TScannerTokenType; AText: string);
begin
  FInfix2Postfix.Push(AText);
end;

end.
