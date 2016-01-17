unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FFTs, Complexs;

const
  ArraySize = 800;

type
  TfmMain = class(TForm)
    btForwardFFT: TButton;
    btInverseFFT: TButton;
    procedure btForwardFFTClick(Sender: TObject);
    procedure btInverseFFTClick(Sender: TObject);
  private
    FSource, FAfterFFT, FResult : array [0..ArraySize-1] of TComplex;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btForwardFFTClick(Sender: TObject);
var
  Loop : Integer;
begin
  for Loop := 1 to ArraySize-1 do begin
    FSource[Loop].Re := Random(256);
    FSource[Loop].Im := Random(256);
  end;

  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo(0, Round(FSource[0].Re) + 50);
  for Loop := 1 to ArraySize-1 do Canvas.LineTo(Loop*4, Round(FSource[Loop].Re) + 50);

  Canvas.Pen.Color := clRed;
  Canvas.MoveTo(0, Round(FSource[0].Im) + 50);
  for Loop := 1 to ArraySize-1 do Canvas.LineTo(Loop*4, Round(FSource[Loop].Im) + 50);

  ForwardFFT(FSource, FAfterFFT, ArraySize);
end;

procedure TfmMain.btInverseFFTClick(Sender: TObject);
var
  Loop : Integer;
begin
  InverseFFT(FAfterFFT, FResult, ArraySize);

  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo(0, Round(FResult[0].Re) + 400);
  for Loop := 1 to ArraySize-1 do Canvas.LineTo(Loop*4, Round(FResult[Loop].Re) + 400);

  Canvas.Pen.Color := clRed;
  Canvas.MoveTo(0, Round(FResult[0].Im) + 400);
  for Loop := 1 to ArraySize-1 do Canvas.LineTo(Loop*4, Round(FResult[Loop].Im) + 400);
end;

end.
