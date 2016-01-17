unit Unit1;

interface

uses
  BlackBoard,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    Image2: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    aBlackBoard : TBlackBoard;
  public
    procedure OnBitMap(Sender : TObject; Image : TBitmap);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  aBlackBoard := TBlackBoard.Create;
  aBlackBoard.Resolution := 8;
  aBlackBoard.OnMonoImage := OnBitMap;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  aBlackBoard.Free;
end;

procedure TForm1.OnBitMap(Sender: TObject; Image: TBitmap);
begin
  Image2.Picture.Bitmap.Assign(Image);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  aBlackBoard.GetBitmap;
end;

end.
