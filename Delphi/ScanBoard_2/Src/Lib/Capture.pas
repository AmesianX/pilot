unit Capture;

interface

uses
  Classes, SysUtils, Windows, Graphics, ExtCtrls;

type
  TCapture = class(TComponent)
  private
    FTickCount : Cardinal;
    Timer      : TTimer;
    FRect      : TRect;
    procedure Capture;
    procedure on_Timer(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function Get(OldTick: Cardinal; var AData: Pointer; var ASize: Integer): Boolean;
  published
    property Rect: TRect read FRect write FRect;
  end;

implementation

{ TCapture }

procedure TCapture.Capture;
begin
  //Todo: Capture
  FTickCount := GetTickCount;
end;

constructor TCapture.Create(AOwner: TComponent);
begin
  inherited;

  Timer          := TTimer.Create(Self);
  Timer.Interval := 100;
  Timer.Enabled  := True;
  Timer.OnTimer  := on_Timer;
end;

destructor TCapture.Destroy;
begin
  Timer.Free;

  inherited;
end;

function TCapture.Get(OldTick: Cardinal; var AData: Pointer; var ASize: Integer): Boolean;
begin
  Result := FTickCount = OldTick;
end;

procedure TCapture.on_Timer(Sender: TObject);
begin
  Timer.Enabled := False;
  try
    Capture;
  finally
    Timer.Enabled := True;
  end;
end;

end.
