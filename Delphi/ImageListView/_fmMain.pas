unit _fmMain;

interface

uses
  ImageListView,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ScrollBar1: TScrollBar;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    IV : TImageListView;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  B : TBitmap;
begin
  B := TBitmap.Create;
  B.LoadFromFile('C:\Users\LynCompany\Desktop\images.bmp');
  IV.AddPicture(B);
  B.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  IV.SelectedItem := 2;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IV := TImageListView.Create(Self);
  IV.Parent := Self;

  IV.Top := 0;
  IV.Left := 0;
  IV.Width := 500;
  IV.Height := 170;
end;

end.
