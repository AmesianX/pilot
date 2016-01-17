unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    btnMakeDataFile: TButton;
    Memo1: TMemo;
    Memo3: TMemo;
    mmDisplay: TMemo;
    btnFileRead: TButton;
    Button1: TButton;
    Edit: TEdit;
    Memo2: TMemo;
    chBox: TCheckBox;
    edDataSize: TEdit;
    Label1: TLabel;
    procedure btnMakeDataFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFileReadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    ZeroBuf   : array[1..4096  ] of char;       // [0000 0000] * 4096
    OneBuf    : array[1..4096  ] of char;       // [1111 1111] * 4096
    IndexBuf  : array[1..100000] of Cardinal;
    dataCount : Cardinal;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FillChar(ZeroBuf , SizeOf(ZeroBuf ), Char($0 ) );
  FillChar(OneBuf  , SizeOf(OneBuf  ), Char($FF) );
  FillChar(IndexBuf, SizeOf(IndexBuf), Char($0 ) );
end;


procedure TfmMain.btnFileReadClick(Sender: TObject);
var
  i          : integer;
  ranIndex   : integer;
  dataSize   : integer;
  fileHandle : integer;
  iCount     : integer;
  startTime  : TDateTime;
  endTime    : TDateTime;
  chkTime    : TDateTime;
  hour       : Word;
  min        : Word;
  sec        : Word;
  mSec       : Word;
  Buffer     : array [1..4096] of char;
begin

  FillChar(Buffer, SizeOf(Buffer), Char($0) );
  memo2.Clear;

  fileHandle := FileOpen('RandomDataFile.Dat', fmOpenRead );

  if fileHandle = -1 then begin
    ShowMessage('Data 파일을 읽는 과정 중에 문제가 발생했습니다.');
  end;

  iCount := StrToIntDef(Edit.Text, 1000);
  startTime := Now;

  for i := 1 to iCount do
  begin
    ranIndex := Random(dataCount) + 1;
    FileSeek(fileHandle, 0, 0);

    if ranIndex = 1 then begin
      dataSize := IndexBuf[1];
    end else begin
      dataSize := IndexBuf[ranIndex] - IndexBuf[ranIndex-1];
      FileSeek(fileHandle, IndexBuf[ranIndex-1]  , 1);
    end;
    FileRead(fileHandle, Buffer, dataSize);

    if chBox.Checked = True then begin
      memo2.Lines.Add('i =' + IntToStr(i) + '  ' + IntToStr(ranIndex));
    end;
  end;

  endTime := Now;
  chkTime := EndTime - StartTime;
  DecodeTime(chkTime, hour, min, sec, mSec);

  memo1.Clear;
  memo1.Lines.Add('sec = ' + IntToStr(sec) + '  mSec = ' + IntToStr(mSec) );

  FileClose(fileHandle);
end;


procedure TfmMain.Button1Click(Sender: TObject);
var
  i          : integer;
  ranIndex   : integer;
  dataSize   : integer;
  fileHandle : integer;
  Buffer     : array [1..4096] of char;
begin

  FillChar(Buffer, SizeOf(Buffer), Char($0) );
  memo2.Clear;

  fileHandle := FileOpen('RandomDataFile.Dat', fmOpenRead );

  if fileHandle = -1 then begin
    ShowMessage('Data 파일을 읽는 과정 중에 문제가 발생했습니다.');
  end;

  FillChar(Buffer, SizeOf(Buffer), Char($ff) );
  FileSeek(fileHandle, 0, 0);
  dataSize := IndexBuf[1];
  FileRead(fileHandle, Buffer, dataSize);

  FillChar(Buffer, SizeOf(Buffer), Char($0) );
  FileSeek(fileHandle, 0, 0);
  dataSize := IndexBuf[2] - IndexBuf[1];
  FileSeek(fileHandle, IndexBuf[2-1]  , 1);
  FileRead(fileHandle, Buffer, dataSize);

  FillChar(Buffer, SizeOf(Buffer), Char($ff) );
  FileSeek(fileHandle, 0, 0);
  dataSize := IndexBuf[3] - IndexBuf[2];
  FileSeek(fileHandle, IndexBuf[3-1]  , 1);
  FileRead(fileHandle, Buffer, dataSize);

  FillChar(Buffer, SizeOf(Buffer), Char($ff) );
  FileSeek(fileHandle, 0, 0);
  dataSize := IndexBuf[dataCount] - IndexBuf[dataCount-1];
  FileSeek(fileHandle, IndexBuf[dataCount-1]  , 1);
  FileRead(fileHandle, Buffer, dataSize);

  FillChar(Buffer, SizeOf(Buffer), Char($0) );
  FileSeek(fileHandle, 0, 0);
  dataSize := IndexBuf[dataCount] - IndexBuf[dataCount-1];
  FileSeek(fileHandle, IndexBuf[dataCount-1]  , 1);
  FileRead(fileHandle, Buffer, dataSize);

  FileClose(fileHandle);
end;



procedure TfmMain.btnMakeDataFileClick(Sender: TObject);
var
  fileHandle  : integer;
  i           : integer;
  chkOdd      : boolean;
  dataSizeSum : cardinal;
  fileSize    : cardinal;
  MBsize      : integer;
begin

  btnFileRead.Enabled := True;
  chkOdd := False;
  fileHandle := FileCreate('RandomDataFile.Dat');

  memo1.Clear;
  i           := 1;
  dataSizeSum := 0;
  dataCount   := 0;

  MBsize := StrToIntDef(edDataSize.Text, 50);

  if (MBsize < 50) or (MBsize > 100) then begin
    MBsize := 50;
  end;

  FileSize := 1024 * 1024 * MBsize;

  while dataSizeSum <= FileSize do begin
    IndexBuf[i] := Random(4097);   // [Data] = 1KB ~ 4KB
    dataCount := dataCount + 1;

    if IndexBuf[i] < 1024 then IndexBuf[i] := IndexBuf[i] + 1024;

    if chkOdd = False then begin
      FileWrite(fileHandle, ZeroBuf, IndexBuf[i] );
      chkOdd := True;
    end else begin
      FileWrite(fileHandle, OneBuf , IndexBuf[i] );
      chkOdd := False;
    end;

    dataSizeSum := dataSizeSum + IndexBuf[i];

    if i > 1 then begin
      IndexBuf[i] := IndexBuf[i] + IndexBuf[i-1];
    end;

    i:= i+1;

  end;

  FileClose(fileHandle);

  memo3.Clear;
  memo3.Lines.Add('[Data 1]의 값   =  ' + IntToStr(IndexBuf[1]));
  memo3.Lines.Add('[Data 1]의 size =  ' + IntToStr(IndexBuf[1]));
  memo3.Lines.Add(' ');

  memo3.Lines.Add('[Data 2]의 값   =  ' + IntToStr(IndexBuf[2]));
  memo3.Lines.Add('[Data 2]의 size =  ' + IntToStr(IndexBuf[2] - IndexBuf[1] ));
  memo3.Lines.Add(' ');

  memo3.Lines.Add('[Data 3]의 값   =  ' + IntToStr(IndexBuf[3]));
  memo3.Lines.Add('[Data 3]의 size =  ' + IntToStr(IndexBuf[3] - IndexBuf[2] ));
  memo3.Lines.Add(' ');

  memo3.Lines.Add('[Last Data - 1]의 값   =  ' + IntToStr(IndexBuf[DataCount-1]));
  memo3.Lines.Add('[Last Data - 1]의 size =  ' + IntToStr(IndexBuf[DataCount-1] - IndexBuf[DataCount-2] ));
  memo3.Lines.Add(' ');

  memo3.Lines.Add('[Last Data]의 값   =  ' + IntToStr(IndexBuf[DataCount]));
  memo3.Lines.Add('[Last Data]의 size =  ' + IntToStr(IndexBuf[DataCount] - IndexBuf[DataCount-1] ));
  memo3.Lines.Add(' ');

  mmDisplay.Clear;
  mmDisplay.Lines.Add('[Data]의 갯수 =  ' + IntToStr(dataCount) );
  mmDisplay.Lines.Add('총 [Data]들의 Size 합 =  ' + IntToStr(dataSizeSum) );

end;

end.
