unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, PolygonBitmap, jpeg, ColorGrd;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Image: TImage;
    ColorGrid1: TColorGrid;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPolygonBitmap : TPolygonBitmap;
  public
  end;

var
  fmMain: TfmMain;

implementation


{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  Points : TPoints;
begin
  SetLength(Points, 5);

  Points[0] := Point(0, 0);
  Points[1] := Point(100, 0);
  Points[2] := Point(100, 100);
  Points[3] := Point(0, 100);
  Points[4] := Point(0, 0);

  FPolygonBitmap.Clear;
  FPolygonBitmap.AddPolygon(Points);

  Image.Picture.Bitmap.PixelFormat := pf32bit;
  FPolygonBitmap.DrawPolygon(0, 0, Image.Picture.Bitmap, 40);
  Image.Repaint;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FPolygonBitmap := TPolygonBitmap.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FPolygonBitmap.Free;
end;

end.
