unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, VidGrab, WebCam, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    frCam: TfrCam;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  frCam.Start;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  frCam.Stop;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowMessage(IntToStr(frCam.DeviceCount));
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Msg : String;
begin
  Msg := frCam.DeviceNames[0];
  ShowMessage(Msg);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  //
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  Stream : TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  frCam.GetJpeg(Stream, 320, 240);

  Stream.SaveToFile('a.jpg');
  Stream.Free;
end;

end.
