unit GameTimer;

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  TGameTimer = class
  private
    FTimer : TTimer;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure on_TimerFired(Sender:TObject);
  public
    constructor Create;
    Destructor Destroy; override;
  published
    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;

implementation

uses
  Logic_Interface;

type
  TFLI = class(TLI)
  end;

{ TGameTimer }

constructor TGameTimer.Create;
begin
  inherited;

  FTimer:= TTimer.Create(Nil);
  FTimer.Interval:= 500;
  FTimer.OnTimer:= Self.on_TimerFired;
  FTimer.Enabled:= False;
end;

destructor TGameTimer.Destroy;
begin
  FTimer.Free;

  inherited;
end;

procedure TGameTimer.on_TimerFired(Sender: TObject);
begin
  FTimer.Enabled:= False;
  try
    TFLI(TLI.GetObject).on_Tick;
  finally
    FTimer.Enabled:= True;
  end;
end;

function TGameTimer.GetEnabled: Boolean;
begin
  Result:= FTimer.Enabled;
end;

procedure TGameTimer.SetEnabled(const Value: Boolean);
begin
  FTimer.Enabled:= Value;
end;

end.
