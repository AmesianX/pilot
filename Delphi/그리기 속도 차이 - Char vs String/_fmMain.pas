unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  MessageString : string = '가나다라 마바사 abcd efg 12345 67890 !@#!##$!$%#$%^';

type
  TfmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    moMsg: TMemo;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  Tick, OldTick : int64;
  Loop: Integer;
begin
  QueryPerformanceCounter(OldTick);

  for Loop := 1 to 1024 do
    Canvas.TextOut(50, 100, MessageString);

  QueryPerformanceCounter(Tick);

  moMsg.Lines.Add(Format('Tick: %d', [Tick - OldTick]));
end;

procedure TfmMain.Button2Click(Sender: TObject);
var
  Tick, OldTick : int64;
  Loop, iX: Integer;
  Idx: Integer;
begin
  QueryPerformanceCounter(OldTick);

  for Loop := 1 to 1024 do begin
    iX := 50;
    for Idx := 1 to Length(MessageString) do begin
      Canvas.TextOut(iX, 100, MessageString[Idx]);
      iX := iX + Canvas.TextWidth(MessageString[Idx]);
    end;
  end;

  QueryPerformanceCounter(Tick);

  moMsg.Lines.Add(Format('Tick: %d', [Tick - OldTick]));
end;

procedure TfmMain.Button3Click(Sender: TObject);
var
  lf: TLogFont;
begin
  GetObject(Canvas.Font.Handle, SizeOf(TLogFont), @lf);
  lf.lfQuality := NONANTIALIASED_QUALITY;
  Canvas.Font.Handle := CreateFontIndirect(lf);
//  Canvas.TextOut(0, 0, 'Hello');
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  Loop, TextWidth: Integer;
begin
  Canvas.Brush.Style := bsClear;

  moMsg.Lines.Add(Format('%d', [Canvas.TextWidth(MessageString)]));

  TextWidth := 0;
  for Loop := 1 to Length(MessageString) do
    TextWidth := TextWidth + Canvas.TextWidth(MessageString[Loop]);
  moMsg.Lines.Add(Format('%d', [TextWidth]));

  moMsg.Lines.Add(Format('%d', [(Canvas.TextWidth(MessageString) - TextWidth) div Length(MessageString)]));
end;

end.
