unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _dmMain, StdCtrls;

type
  TfmMain = class(TAfmMain)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
    procedure Display(Numbers:array of integer); override;
  end;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  Self.on_Start;
end;

procedure TfmMain.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TfmMain.Display(Numbers: array of integer);
var
  Loop : integer;
begin
  Memo1.Lines.Clear;

  for Loop := Low(Numbers) to High(Numbers) do
    Memo1.Lines.Add(IntToStr(Numbers[Loop]));
end;

end.
