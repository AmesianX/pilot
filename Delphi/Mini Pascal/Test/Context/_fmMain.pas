unit _fmMain;

interface

uses
  ParsingContext,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    Splitter1: TSplitter;
    ListBox: TListBox;
    Panel1: TPanel;
    btStart: TButton;
    moSrc: TMemo;
    procedure btStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FContext : TParsingContext;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  FContext.Scan(moSrc.Text);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FContext := TParsingContext.Create(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FContext.Free;
end;

end.
