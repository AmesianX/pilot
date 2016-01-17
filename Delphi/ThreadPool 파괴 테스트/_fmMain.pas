unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    btRun: TButton;
    Memo1: TMemo;
    procedure btRunClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  ThreadPool;

{$R *.dfm}

function ThreadFunction(lpThreadParameter: Pointer): Integer; stdcall;
var
  iCount : integer;
begin
  Result := 0;
  try
    try
      iCount := 0;
      repeat
        iCount := iCount + 1;
        TfmMain(lpThreadParameter).Memo1.Lines.Add(IntToStr(iCount));
        Sleep(1);
      until false;
    finally
    end;
  except
  end;
end;

procedure TfmMain.btRunClick(Sender: TObject);
begin
  QueueWorkItem(ThreadFunction, Self);
end;

end.
