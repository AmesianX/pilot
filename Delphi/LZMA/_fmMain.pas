unit _fmMain;

interface

uses
  Disk,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

function LzmaCompress(dest: PByte; destLen: PDWORD; const src: PByte; srcLen: DWORD;
  outProps: PByte;
  outPropsSize: PDWORD; { *outPropsSize must be = 5 }
  level: Integer = 9; { 0 <= level <= 9, default = 5 }
  dictSize: DWORD = 16777216; { default = (1 << 24) }
  lc: Integer = 8; { 0 <= lc <= 8, default = 3 }
  lp: Integer = 4; { 0 <= lp <= 4, default = 0 }
  pb: Integer = 4; { 0 <= pb <= 4, default = 2 }
  fb: Integer = 273; { 5 <= fb <= 273, default = 32 }
  numThreads: Integer = 2 { 1 or 2, default = 2 }
  ): Integer;
  stdcall; external 'LZMA.dll' delayed;

function LzmaUncompress(dest: PByte; destLen: PDWORD; const src: PByte;
    srcLen: PDWORD; const props: PByte; propsSize: DWORD): Integer;
    stdcall; external 'LZMA.dll' delayed;

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
const
  LZMA_PROPS_SIZE = 5;
var
  i: integer;
  a: array [0 .. 100] of Byte;
  b: AnsiString;
  destLen, srcLen: Cardinal;
  prop: array [0 .. 4] of Byte;
  propSize: DWORD;
begin
  propSize := LZMA_PROPS_SIZE;
  ZeroMemory(@a, 100);
  b := 'TestValue12341234한글테스트';
  destLen := Length(a);
  Caption := IntToSTr(Length(b));
  srcLen := Length(b);

  i := LzmaCompress(@a, PDWORD(@destLen), PByte(@b[1]), srcLen, @prop,
    PDWORD(@propSize));
  if i = 0 then
  begin
//    FillChar(b[1], srcLen, Ord($20));
    i := LzmaUncompress(PByte(@b[1]), PDWORD(@srcLen), PByte(@a), @destLen,
      @prop, propSize);
    if i = 0 then
      ShowMessage(b);
  end;
end;

end.
