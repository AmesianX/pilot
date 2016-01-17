unit _fmMain;

interface

uses
  Sys, Strg,
  Windows, Messages, SysUtils, Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
var MemoryStatus: TMemoryStatusEx;
begin
  Memo1.Lines.Clear;

  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatusEx(MemoryStatus);

  with MemoryStatus do begin
    Memo1.Lines.Add(IntToStr(dwLength)         + ' Size of ''MemoryStatus'' record') ;
    Memo1.Lines.Add(MemoryStr(dwMemoryLoad)    + '% memory in use') ;
    Memo1.Lines.Add(MemoryStr(MemoryStatus.ullTotalPhys)     + ' Total Physical Memory in bytes') ;
//    Memo1.Lines.Add(MemoryStr(dwAvailPhys)     + ' Available Physical Memory in bytes') ;
//    Memo1.Lines.Add(MemoryStr(dwTotalPageFile) + ' Total Bytes of Paging File') ;
//    Memo1.Lines.Add(MemoryStr(dwAvailPageFile) + ' Available bytes in paging file') ;
//    Memo1.Lines.Add(MemoryStr(dwTotalVirtual)  + ' User Bytes of Address space') ;
//    Memo1.Lines.Add(MemoryStr(dwAvailVirtual)  + ' Available User bytes of address space') ;
   end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Caption := IntToStr(FreePhysicalSize div (1024 * 1024));
end;

end.
