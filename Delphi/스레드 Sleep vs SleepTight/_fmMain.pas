unit _fmMain;

interface

uses
  DebugTools, SimpleThread,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    iTaskStart, iTaskEnd : int64;
  private
    FThreadA : TSimpleThread;
    procedure on_RepeatA(Sender:TObject);
  private
    FThreadB : TSimpleThread;
    procedure on_RepeatB(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

{ TfmMain }

procedure TfmMain.Button1Click(Sender: TObject);
begin
  FThreadA.WakeUp;
end;

procedure TfmMain.Button2Click(Sender: TObject);
begin
  QueryPerformanceCounter( iTaskStart );
  FThreadB.WakeUp;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FThreadA := TSimpleThread.Create(on_RepeatA);
  FThreadB := TSimpleThread.Create(on_RepeatB);
end;

procedure TfmMain.on_RepeatA(Sender: TObject);
begin
  FThreadA.SleepTight;

  QueryPerformanceCounter( iTaskStart );
  Sleep(1);
  QueryPerformanceCounter( iTaskEnd );

  Trace( Format('FThreadA - %d', [iTaskEnd-iTaskStart]) );
end;

procedure TfmMain.on_RepeatB(Sender: TObject);
begin
  FThreadB.SleepTight;
  QueryPerformanceCounter( iTaskEnd );

  Trace( Format('FThreadB - %d', [iTaskEnd-iTaskStart]) );
end;

end.



