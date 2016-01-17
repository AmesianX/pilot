unit _fmMain;

interface

uses
  ThreadRepeater, DynamicQueue, GpLockFreeQueue, LockFreeStack,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FTick : Cardinal;
    FQueue : TDynamicQueue;
//    FQueue : TGpLockFreeQueue;
    FLockFreeStack : TLockFreeStack;
    FConsumers : array [0..8-1] of TThreadRepeater;
    procedure on_Consumer(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

{ TfmMain }

procedure TfmMain.Button1Click(Sender: TObject);
var
  Loop: Integer;
begin
  FTick := GetTickCount;
  for Loop := Low(FConsumers) to High(FConsumers) do FConsumers[Loop].Execute(on_Consumer);
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  Loop: Integer;
begin
  FQueue := TDynamicQueue.Create(true);
//  FQueue := TGpLockFreeQueue.Create;
//  FLockFreeStack := TLockFreeStack.Create;

  for Loop := Low(FConsumers) to High(FConsumers) do FConsumers[Loop] := TThreadRepeater.Create(Self);
  for Loop := 0 to $FFFFFF do FQueue.Push(Pointer(Random($FFFFFF)));
//  for Loop := 0 to $FFFFFF do FQueue.Enqueue(Random($FFFFFF));
//  for Loop := 0 to $FFFFFF do FLockFreeStack.Push(Pointer(Random($FFFFFF)));
end;

procedure TfmMain.on_Consumer(Sender: TObject);
var
  Loop: Integer;
  Item : pointer;
//  Item : int64;
begin
  for Loop := 0 to $FFFFFF do begin
    if not FQueue.Pop(Item) then Break;
//    if not FQueue.Dequeue(Item) then Break;
//    Item := FLockFreeStack.Pop;
//    if Item = nil then Break;
  end;
  moMsg.Lines.Add(Format('on_Consumer: %d', [GetTickCount-FTick]));
  TThreadRepeater(Sender).Terminate;
end;

end.
