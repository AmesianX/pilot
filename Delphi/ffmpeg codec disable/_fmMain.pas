unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    btEncoderList: TButton;
    moMsg: TMemo;
    btDecoder: TButton;
    btMux: TButton;
    btDeMux: TButton;
    Button1: TButton;
    procedure btEncoderListClick(Sender: TObject);
    procedure btDecoderClick(Sender: TObject);
    procedure btMuxClick(Sender: TObject);
    procedure btDeMuxClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btDecoderClick(Sender: TObject);
var
  Loop: Integer;
begin
  moMsg.Lines.DelimitedText := moMsg.Text;

  for Loop := 0 to moMsg.Lines.Count-1 do
    moMsg.Lines[Loop] := '--disable-decoder=' + moMsg.Lines[Loop] + ' \';;
end;

procedure TForm2.btDeMuxClick(Sender: TObject);
var
  Loop: Integer;
begin
  moMsg.Lines.DelimitedText := moMsg.Text;

  for Loop := 0 to moMsg.Lines.Count-1 do
    moMsg.Lines[Loop] := '--disable-demuxer=' + moMsg.Lines[Loop] + ' \';;
end;

procedure TForm2.btEncoderListClick(Sender: TObject);
var
  Loop: Integer;
begin
  moMsg.Lines.DelimitedText := moMsg.Text;

  for Loop := 0 to moMsg.Lines.Count-1 do
    moMsg.Lines[Loop] := '--disable-encoder=' + moMsg.Lines[Loop] + ' \';;
end;

procedure TForm2.btMuxClick(Sender: TObject);
var
  Loop: Integer;
begin
  moMsg.Lines.DelimitedText := moMsg.Text;

  for Loop := 0 to moMsg.Lines.Count-1 do
    moMsg.Lines[Loop] := '--disable-muxer=' + moMsg.Lines[Loop] + ' \';;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  Loop: Integer;
begin
  moMsg.Lines.DelimitedText := moMsg.Text;

  for Loop := 0 to moMsg.Lines.Count-1 do
    moMsg.Lines[Loop] := '--disable-parser=' + moMsg.Lines[Loop] + ' \';;
end;

end.
