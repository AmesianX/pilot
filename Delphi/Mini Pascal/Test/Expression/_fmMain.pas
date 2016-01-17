unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    moSrc: TMemo;
    btCalc: TButton;
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

end.
