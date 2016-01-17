unit _fmMain;

interface

uses
  Interlocked,
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

procedure TfmMain.FormCreate(Sender: TObject);
var
  Src, Result : integer;
  Src64, Result64 : int64;
begin
  Src := 1;
  Result := InterlockedCompareExchange(Src, 2, 0);
  moMsg.Lines.Add(Format('Result := InterlockedCompareExchange(Src(1), 2, 0) - Src: %d, Result: %d', [Src, Result]));

  Result := InterlockedCompareExchange(Src, 2, 1);
  moMsg.Lines.Add(Format('Result := InterlockedCompareExchange(Src(1), 2, 1) - Src: %d, Result: %d', [Src, Result]));

  Src := 1;
  Result := InterlockedIncrement(Src);
  moMsg.Lines.Add(Format('Result := InterlockedIncrement(Src(1)) - Src: %d, Result: %d', [Src, Result]));

  Src := 1;
  Result := InterlockedDecrement(Src);
  moMsg.Lines.Add(Format('Result := InterlockedDecrement(Src(1)) - Src: %d, Result: %d', [Src, Result]));

  Src := 1;
  Result := InterlockedExchangeAdd(Src, 3);
  moMsg.Lines.Add(Format('Result := InterlockedExchangeAdd(Src(1), 3) - Src: %d, Result: %d', [Src, Result]));

  Src := 0;
  Result := InterlockedExchange(Src, 1);
  moMsg.Lines.Add(Format('Result := InterlockedExchange(Src(0), 1) - Src: %d, Result: %d', [Src, Result]));

  Result := InterlockedExchange(Src, 1);
  moMsg.Lines.Add(Format('Result := InterlockedExchange(Src(1), 1) - Src: %d, Result: %d', [Src, Result]));

  Result := InterlockedExchange(Src, 2);
  moMsg.Lines.Add(Format('Result := InterlockedExchange(Src(1), 2) - Src: %d, Result: %d', [Src, Result]));

  Src64 := 1;
  Result64 := InterlockedExchangeAdd64(Src64, 2);
  moMsg.Lines.Add(Format('Result := InterlockedExchangeAdd64(Src(1), 2) - Src: %d, Result: %d', [Src64, Result64]));

  Result64 := InterlockedExchangeAdd64(Src64, -2);
  moMsg.Lines.Add(Format('Result := InterlockedExchangeAdd64(Src(3), -2) - Src: %d, Result: %d', [Src64, Result64]));
end;

end.
