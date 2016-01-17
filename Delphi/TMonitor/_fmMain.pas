unit _fmMain;

interface

uses
  SimpleThread,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SyncObjs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FLockObject : TObject;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  System.TMonitor.Enter(FLockObject);
  try
    System.TMonitor.PulseAll( FLockObject );
  finally
    System.TMonitor.Exit(FLockObject);
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FLockObject := TObject.Create;

  TSimpleThread.Create(
    Self,
    procedure (AContext:pointer) begin
      while True do begin
        System.TMonitor.Enter(FLockObject);
        try
          Memo1.Lines.Add( '1 - s' );
          System.TMonitor.Wait( FLockObject, INFINITE );
          Memo1.Lines.Add( '1 - e' );
        finally
          System.TMonitor.Exit(FLockObject);
        end;
      end;
    end
  );

  TSimpleThread.Create(
    Self,
    procedure (AContext:pointer) begin
      while True do begin
        System.TMonitor.Enter(FLockObject);
        try
          Memo1.Lines.Add( '2 - s' );
          System.TMonitor.Wait( FLockObject, INFINITE );
          Memo1.Lines.Add( '2 - e' );
        finally
          System.TMonitor.Exit(FLockObject);
        end;
      end;
    end
  );

  TSimpleThread.Create(
    Self,
    procedure (AContext:pointer) begin
      while True do begin
        System.TMonitor.Enter(FLockObject);
        try
          Memo1.Lines.Add( '3 - s' );
          System.TMonitor.Wait( FLockObject, INFINITE );
          Memo1.Lines.Add( '3 - e' );
        finally
          System.TMonitor.Exit(FLockObject);
        end;
      end;
    end
  );
end;

end.
