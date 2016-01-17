unit _fmMain;

interface

uses
  MultiIJL, IjlUtils,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TTestThread = class(TThread)
  private
    FNum : Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(Num : Integer); reintroduce;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Ts : array [1 .. 4] of TThread;
  I: Integer;
begin
  for I := 1 to 4 do begin
    Ts[i] := TTestThread.Create(i);
  end;

  for I := 1 to 4 do begin
    Ts[i].WaitFor;
  end;
end;

{ TTestThread }

constructor TTestThread.Create(Num: Integer);
begin
  FNum := Num;

  inherited Create(False);
end;

procedure TTestThread.Execute;
var
  i : Integer;
  S1, S2 : TMemoryStream;
  IJL : TMultiIJL;
begin
  S1 := TMemoryStream.Create;
  S2 := TMemoryStream.Create;
  IJL := TMultiIJL.Create;
  S1.LoadFromFile(Format('%d.bmp', [FNum]));

  for I := 0 to 5 - 1 do begin
    S1.Position := 0;
    IJL.BitmapStreamToJpeg(S1, S2, 1680, 1050, 100, pf32bit);
    S2.SaveToFile(Format('%d%d.jpg', [Random(MaxInt), GetTickCount]));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i : Integer;
  S1, S2 : TMemoryStream;
begin
  S1 := TMemoryStream.Create;
  S2 := TMemoryStream.Create;
  S1.LoadFromFile(Format('%d.bmp', [1]));

  for I := 0 to 1 - 1 do begin
    S1.Position := 0;
    ijlUtils.BitmapStreamToJpeg(S1, S2, 1680, 1050, 100, pf32bit);
    //IjlUtils.BitmapStreamToJpeg(S1, S2);
    S2.SaveToFile(Format('%d%d.jpg', [Random(MaxInt), GetTickCount]));
    S2.Clear;
  end;
end;

end.
