unit _fmMain;

interface

uses
  ParserUtils, Scanner, 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, TypInfo;

type
  TfmMain = class(TForm)
    ListBox: TListBox;
    Panel1: TPanel;
    btStart: TButton;
    moSrc: TMemo;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure btStartClick(Sender: TObject);
  private
    FScanner : TScanner;
    procedure on_Token(Sender:TObject; AIndex:integer; ATokenType:TScannerTokenType; AText:string);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  ListBox.Clear;
  FScanner.Execute(moSrc.Text);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FScanner := TScanner.Create(Self);
  FScanner.OnToken := on_Token;
end;

procedure TfmMain.on_Token(Sender: TObject; AIndex: integer;
  ATokenType: TScannerTokenType; AText: string);
begin
  if ATokenType = ttWhiteSpace then Exit;
  
  ListBox.Items.Add(Format('%4d: %s (%s)', [AIndex, LowerCase(AText), GetEnumName(TypeInfo(TScannerTokenType), Integer(ATokenType))]));
end;

end.

