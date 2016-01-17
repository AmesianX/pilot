unit WhiteBoard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TWhiteBoard = class(TImage)
  protected
    procedure on_MouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
  end;

implementation

{ TWB }

procedure TWhiteBoard.Clear;
begin

end;

constructor TWhiteBoard.Create(AOwner: TComponent);
begin
  inherited;

  Picture.Bitmap.TransparentMode:= tmFixed;
  Picture.Bitmap.TransparentColor:= clWhite;
//  Picture.Bitmap.Transparent:= true;
//  Transparent:= true;

  Picture.Bitmap.Width:= 1024;
  Picture.Bitmap.Height:= 768;

  OnMouseMove:= on_MouseMove;
end;

procedure TWhiteBoard.on_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Picture.Bitmap.Canvas.Pixels[X, Y]:= clBlack;
  Invalidate;
end;

end.
