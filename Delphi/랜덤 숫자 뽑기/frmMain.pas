unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RandomChoice, StdCtrls;

type
  TfmMain = class(TForm)
    btInit: TButton;
    Memo1: TMemo;
    btGetRndNo: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btInitClick(Sender: TObject);
    procedure btGetRndNoClick(Sender: TObject);
  private
    { Private declarations }
    FRandomChoice : TRandomChoice;
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btInitClick(Sender: TObject);
begin
  FRandomChoice.Init;
  Memo1.Lines.Clear;
end;

procedure TfmMain.btGetRndNoClick(Sender: TObject);
begin
  Memo1.Lines.Add(IntToStr(FRandomChoice.Get));
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FRandomChoice:= TRandomChoice.Create;
end;

end.
