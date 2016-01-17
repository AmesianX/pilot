unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IjlUtils, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    Label4: TLabel;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    Tick, NowTick : Cardinal;
  public
    { Public declarations }
  end;

function BitmapStreamToXR(Data: Pointer; Size: Integer; Quility: Integer; PixelFormat: Integer; Output : PPointer; var OutputSize : Integer): Integer; cdecl;
procedure FreeXR(Data : Pointer);
var
  Form1: TForm1;

implementation

{$R *.dfm}

var loop : integer = 10;

function BitmapStreamToXR; external 'WMPEncDll.dll' name 'BitmapStreamToXR';
procedure FreeXR; external 'WMPEncDll.dll' name 'FreeXR';

procedure TForm1.Button1Click(Sender: TObject);
var
  b : TBitmap;
  ms : TMemoryStream;
  os : TMemoryStream;
  i : Integer;
begin
  if FileExists(Label4.Caption) = false then begin
    Memo1.Lines.Add('File이 없습니다.');
    exit;
  end;
  b := TBitmap.Create;
  ms := TMemoryStream.Create;
  b.SaveToStream(ms);
  try
    b.LoadFromFile(Label4.Caption);
    os := TMemoryStream.Create;
    Tick := GetTickCount;
    for I := 0 to loop - 1 do begin
      os.Clear;
      BitmapToJpeg(b, os, SpinEdit2.Value);
      if CheckBox1.Checked = true then os.SaveToFile('test.jpg');
    end;
    NowTick := GetTickCount;

    Memo1.Lines.Add(PChar(Format('IJL : %dms', [NowTick - Tick])));
  finally
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  b : TBitmap;
  m : Pointer;
  outsize : Integer;
  ms : TMemoryStream;
  os : TMemoryStream;
  i : Integer;
begin
  if FileExists(Label4.Caption) = false then begin
    Memo1.Lines.Add('File이 없습니다.');
    exit;
  end;
  b := TBitmap.Create;
  ms := TMemoryStream.Create;
  try
    b.LoadFromFile(Label4.Caption);
    b.SaveToStream(ms);
    os := TMemoryStream.Create;
    Tick := GetTickCount;
    for I := 0 to loop - 1 do begin
      os.Clear;
      BitmapStreamToXR(ms.Memory, ms.Size, SpinEdit3.Value, 0,  @m, outsize);
      os.Write(m^, outsize);
      if CheckBox1.Checked = true then os.SaveToFile('test.hdp');
      FreeXR(m);
    end;
    NowTick := GetTickCount;
    Memo1.Lines.Add(PChar(Format('XR : %dms', [NowTick - Tick])));
  finally
    ms.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Button1Click(nil);
  Button2Click(nil);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if OpenDialog1.Execute(Self.Handle) then begin
    Label4.Caption := OpenDialog1.FileName;
  end;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  loop := SpinEdit1.Value;
end;

end.
