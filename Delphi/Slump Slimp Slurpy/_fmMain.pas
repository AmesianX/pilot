unit _fmMain;

interface

uses
  Slurpy,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
var
  Slurpy : TSlurpy;
begin
  Slurpy := TSlurpy.Create;
  try
    Slurpy.Source := 'AH';
    moMsg.Lines.Add(Format('IsSlimp(%s) is %s', [Slurpy.Source, BoolToStr(Slurpy.IsSlimp, true)]));

    Slurpy.Source := 'ABAHC';
    moMsg.Lines.Add(Format('IsSlimp(%s) is %s', [Slurpy.Source, BoolToStr(Slurpy.IsSlimp, true)]));

    Slurpy.Source := 'ADFGC';
    moMsg.Lines.Add(Format('IsSlimp(%s)is %s', [Slurpy.Source, BoolToStr(Slurpy.IsSlimp, true)]));

    Slurpy.Source := 'ABAHD';
    moMsg.Lines.Add(Format('IsSlimp(%s) is %s', [Slurpy.Source, BoolToStr(Slurpy.IsSlimp, true)]));

    Slurpy.Source := 'AHDFG';
    moMsg.Lines.Add(Format('IsSlurpy(%s) is %s', [Slurpy.Source, BoolToStr(Slurpy.IsSlurpy, true)]));

    Slurpy.Source := 'DFGAH';
    moMsg.Lines.Add(Format('IsSlurpy(%s) is %s', [Slurpy.Source, BoolToStr(Slurpy.IsSlurpy, true)]));
  finally
    Slurpy.Free;
  end;
end;

end.
