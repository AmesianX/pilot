unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

type
  TSLIST_HEADER = record
    case integer of
      0 : (Alignment : int64);

      1 : (
        Next : pointer;
        Depth : word;
        Sequence : word;
      )
  end;
  PSLIST_HEADER = ^TSLIST_HEADER;

  TSLIST_ENTRY = record
    NextEntry : pointer;
    MyData : integer;
  end;
  PSLIST_ENTRY = ^TSLIST_ENTRY;

procedure InitializeSListHead(AHeader:pointer); stdcall; external kernel32;
function InterlockedFlushSList(AHeader:pointer):pointer; stdcall; external kernel32;
function InterlockedPopEntrySList(AHeader:pointer):pointer; stdcall; external kernel32;
function InterlockedPushEntrySList(AHeader,AEntry:pointer):pointer; stdcall; external kernel32;
function QueryDepthSList(AHeader:pointer):Word; stdcall; external kernel32;

procedure TfmMain.FormCreate(Sender: TObject);
var
  pHeader : PSLIST_HEADER;
  pEntry : PSLIST_ENTRY;
  Loop: Integer;
begin
  New(pHeader);

  InitializeSListHead(pHeader);

  for Loop := 1 to 10 do begin
    New(pEntry);
    pEntry^.MyData := Loop;
    InterlockedPushEntrySList(pHeader, pEntry);
  end;
  moMsg.Lines.Add(Format('After InterlockedPushEntrySList: QueryDepthSList=%d'#13#10, [QueryDepthSList(pHeader)]));

  for Loop := 1 to 10 do begin
    pEntry := InterlockedPopEntrySList(pHeader);
    moMsg.Lines.Add(Format('InterlockedPopEntrySList: %d', [pEntry^.MyData]));
    Dispose(pEntry);
  end;
  moMsg.Lines.Add(Format('After InterlockedPopEntrySList: QueryDepthSList=%d'#13#10, [QueryDepthSList(pHeader)]));

  Dispose(pHeader);
end;

end.
