unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NativeJpeg, StdCtrls, IJLUtils, Mask, JvExMask, JvToolEdit;

type
  TForm1 = class(TForm)
    BtnIJL: TButton;
    BtnNativeJpeg: TButton;
    JvFilenameEdit1: TJvFilenameEdit;
    procedure BtnIJLClick(Sender: TObject);
    procedure BtnNativeJpegClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const Count = 100;

implementation

{$R *.dfm}

procedure TForm1.BtnIJLClick(Sender: TObject);
var
  Bm : TBitmap;
  st : TMemoryStream;
  I: Integer;
  Tick, NowTick : Cardinal;
begin
  if FileExists(JvFilenameEdit1.FileName) = false then exit;
  Bm := TBitmap.Create;
  st := TMemoryStream.Create;
  try
    Bm.LoadFromFile(JvFilenameEdit1.FileName);
    Tick := GetTickCount;
    for I := 0 to Count - 1 do begin
      st.Clear;
      IJLUtils.BitmapToJpeg(Bm, st);
      st.SaveToFile('a2.jpg');
    end;
    NowTick := GetTickCount;

    OutputDebugString(PChar(Format('IJL : %dms', [NowTick - Tick])));
  finally
    Bm.Free;
    st.Free;
  end;
end;

procedure TForm1.BtnNativeJpegClick(Sender: TObject);
var
  Bm : TBitmap;
  sdnj : TsdJpegGraphic;
  st : TMemoryStream;
  I: Integer;
  Tick, NowTick : Cardinal;
begin
  if FileExists(JvFilenameEdit1.FileName) = false then exit;
  Bm := TBitmap.Create;
  st := TMemoryStream.Create;
  sdnj := TsdJpegGraphic.Create;
  try
    Bm.LoadFromFile(JvFilenameEdit1.FileName);
    Tick := GetTickCount;
    for I := 0 to Count - 1 do begin
      st.Clear;
      sdnj.CompressionQuality := 100; 
      sdnj.Assign(Bm);
      sdnj.SaveToStream(st);
      st.SaveToFile('a.jpg');
    end;
    NowTick := GetTickCount;

    OutputDebugString(PChar(Format('Native Jpeg : %dms', [NowTick - Tick])));
  finally
    Bm.Free;
    st.Free;
    sdnj.Free;
  end;
end;

end.
