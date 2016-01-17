unit Background;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

type
  TBackground = class(TGraphicControl)
  protected
    procedure Paint; override;
  private
    FBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap : TBitmap read FBitmap write SetBitmap;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RyuLib', [TBackground]);
end;

{ TBackground }

constructor TBackground.Create(AOwner: TComponent);
begin
  inherited;

  FBitmap := TBitmap.Create;
end;

destructor TBackground.Destroy;
begin
  FreeAndNil(FBitmap);

  inherited;
end;

procedure TBackground.Paint;
var
  FBitmapPreveiw, FBitmapPiece : TBitmap;
  Loop: Integer;
  Src, Dst : TRect;
begin
  if (Width * Height) = 0 then Exit;

  FBitmapPreveiw := TBitmap.Create;
  FBitmapPiece := TBitmap.Create;
  try
    FBitmapPreveiw.Width := Width;
    FBitmapPreveiw.Height := FBitmap.Height;

    FBitmapPiece.Width := FBitmap.Width div 3;
    FBitmapPiece.Height := FBitmap.Height;

    if FBitmapPiece.Width = 0 then Exit;

    Dst := Bounds(0, 0, FBitmapPiece.Width, FBitmapPiece.Height);

    // Repeat middle part.
    Src := Bounds(FBitmapPiece.Width, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
    for Loop := 0 to Width div FBitmapPiece.Width do
      FBitmapPreveiw.Canvas.Draw(Loop * FBitmapPiece.Width, 0, FBitmapPiece);

    // Draw left part.
    Src := Bounds(0, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
      FBitmapPreveiw.Canvas.Draw(0, 0, FBitmapPiece);

    // Draw right part.
    Src := Bounds(FBitmap.Width-FBitmapPiece.Width, 0, FBitmapPiece.Width, FBitmapPiece.Height);
    FBitmapPiece.Canvas.CopyRect(Dst, FBitmap.Canvas, Src);
      FBitmapPreveiw.Canvas.Draw(Width-FBitmapPiece.Width, 0, FBitmapPiece);

    Canvas.Draw(0, 0, FBitmapPreveiw);
  finally
    FBitmapPreveiw.Free;
    FBitmapPiece.Free;
  end;
end;

procedure TBackground.SetBitmap(const Value: TBitmap);
begin
  FBitmap := Value;
end;

end.
