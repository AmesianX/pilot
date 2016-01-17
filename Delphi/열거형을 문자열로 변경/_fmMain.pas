unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  TypInfo;

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add(GetEnumName(TypeInfo(TPixelFormat), Integer(pf8bit)));
end;

end.

