unit _fmMain;

interface

uses
  GpLockFreeQueue, ThreadRepeater,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FQueue : TGpLockFreeQueue;
    FRepeaters : array [0..32-1] of TThreadRepeater;
    procedure on_Repeat(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  Loop: Integer;
  Item : int64;
begin
  moMsg.Clear;
  for Loop := 1 to 10 do begin
    if FQueue.Dequeue(Item) then moMsg.Lines.Add(Format('Count: %d', [Item]));
  end;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Loop: Integer;
begin
  for Loop := Low(FRepeaters) to High(FRepeaters) do FRepeaters[Loop].Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  Loop: Integer;
begin
  FQueue := TGpLockFreeQueue.Create;

  for Loop := Low(FRepeaters) to High(FRepeaters) do begin
    FRepeaters[Loop] := TThreadRepeater.Create(Self);
    FRepeaters[Loop].Execute(on_Repeat);
  end;
end;

var
  Count : integer = 0;

procedure TfmMain.on_Repeat(Sender: TObject);
var
  Item : int64;
  Loop: Integer;
begin
  for Loop := 1 to $FFFF do begin
    if Odd(Random(32)) then begin
      FQueue.Enqueue(InterlockedIncrement(Count));
    end else begin
      FQueue.Dequeue(Item);
    end;
  end;

  Sleep(1);
end;

end.
