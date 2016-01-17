unit _fmMain;

interface

uses
  ParsingContext, NodeProgram,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    btStart: TButton;
    moSrc: TMemo;
    moOut: TMemo;
    procedure btStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCommands : TList;
    FContext : TParsingContext;
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  NodeCommandList, NodeRepeat, NodeGo, NodeTurn;

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
var
  Lines : TStringList;
  NodeProgram : TNodeProgram;
begin
  FContext.Scan(moSrc.Text);

  NodeProgram := TNodeProgram.Create(FCommands, FContext);
  try
    NodeProgram.Parse;

    Lines := TStringList.Create;
    try
      NodeProgram.Compile(Lines);
      moOut.Text := Lines.Text;
    finally
      Lines.Free;
    end;
  finally
    NodeProgram.Free;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FCommands := TList.Create;
  FContext := TParsingContext.Create;

  FCommands.Add(TNodeCommandList.Create(FCommands, FContext));
  FCommands.Add(TNodeRepeat.Create(FCommands, FContext));
  FCommands.Add(TNodeGo.Create(FCommands, FContext));
  FCommands.Add(TNodeTurn.Create(FCommands, FContext));
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  // Todo : List 내에 있는 컴맨드에 대한 메모리 해제
  FCommands.Free;

  FContext.Free;
end;

end.
