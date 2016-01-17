unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image: TImage;
    Image2: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Angle : Double;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Const PixelMax = 32768;

Type
   pPixelArray = ^TPixelArray;
   TPixelArray = Array[0..PixelMax-1] Of TRGBTriple;

Procedure RotateBitmap_ads(
   SourceBitmap : TBitmap;
   out DestBitmap : TBitmap;
   Center : TPoint;
   Angle : Double) ;
Var
   cosRadians : Double;
   inX : Integer;
   inXOriginal : Integer;
   inXPrime : Integer;
   inXPrimeRotated : Integer;
   inY : Integer;
   inYOriginal : Integer;
   inYPrime : Integer;
   inYPrimeRotated : Integer;
   OriginalRow : pPixelArray;
   Radians : Double;
   RotatedRow : pPixelArray;
   sinRadians : Double;
begin
   DestBitmap.Width := SourceBitmap.Width;
   DestBitmap.Height := SourceBitmap.Height;
   DestBitmap.PixelFormat := pf24bit;
   Radians := -(Angle) * PI / 180;
   sinRadians := Sin(Radians) ;
   cosRadians := Cos(Radians) ;
   For inX := DestBitmap.Height-1 Downto 0 Do
   Begin
     RotatedRow := DestBitmap.Scanline[inX];
     inXPrime := 2*(inX - Center.y) + 1;
     For inY := DestBitmap.Width-1 Downto 0 Do
     Begin
       inYPrime := 2*(inY - Center.x) + 1;
       inYPrimeRotated := Round(inYPrime * CosRadians - inXPrime * sinRadians) ;
       inXPrimeRotated := Round(inYPrime * sinRadians + inXPrime * cosRadians) ;
       inYOriginal := (inYPrimeRotated - 1) Div 2 + Center.x;
       inXOriginal := (inXPrimeRotated - 1) Div 2 + Center.y;
       If
         (inYOriginal >= 0) And
         (inYOriginal <= SourceBitmap.Width-1) And
         (inXOriginal >= 0) And
         (inXOriginal <= SourceBitmap.Height-1)
       Then
       Begin
         OriginalRow := SourceBitmap.Scanline[inXOriginal];
         RotatedRow[inY] := OriginalRow[inYOriginal]
       End
       Else
       Begin
         RotatedRow[inY].rgbtBlue := 255;
         RotatedRow[inY].rgbtGreen := 0;
         RotatedRow[inY].rgbtRed := 0
       End;
     End;
   End;
End;

procedure TForm1.Button1Click(Sender: TObject);
Var
   Center : TPoint;
   Bitmap : TBitmap;
begin
   Bitmap := TBitmap.Create;
   Try
     Center.y := (Image.Height div 2);
     Center.x := (Image.Width div 2);
     RotateBitmap_ads(
       Image.Picture.Bitmap,
       Bitmap,
       Center,
       Angle) ;
     Angle := Angle + 15;
     Image2.Picture.Bitmap.Assign(Bitmap) ;
   Finally
     Bitmap.Free;
   End;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   Angle:= 0;
end;

end.
 