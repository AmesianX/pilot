unit HourGlass;

interface

uses
  Classes, SysUtils;

type
  THourGlass = class
  private
    FOnTimer: TNotifyEvent;
    FInterval: integer;
    FTimeLeft: integer;
    procedure SetInterval(const Value: integer);
    procedure SetTimeLeft(const Value: integer);
  public
    constructor Create;
    
    procedure Init;
    procedure UpsideDown;
    procedure Tick;
  published
    property Interval : integer read FInterval write SetInterval;
    property TimeLeft : integer read FTimeLeft write SetTimeLeft;
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

{ THourGlass }

constructor THourGlass.Create;
begin
  inherited;

  Init;
end;

procedure THourGlass.Init;
begin
  FTimeLeft:= 0;
end;

procedure THourGlass.SetInterval(const Value: integer);
begin
  FInterval := Value;
end;

procedure THourGlass.SetTimeLeft(const Value: integer);
var
  bCondition : boolean;
begin
  if Value < 0 then begin
    raise Exception.Create('¸ð·¡½Ã°Ô¿¡ ±¸¸Û³µ³ªºÁ¿ä ¤Ñ.¤Ñ');
    // Exit´Â ºÒÇÊ¿äÇÑ Áþ°Å¸® ¤Ñ.¤Ñ
    Exit;
  end;

  bCondition:=
    (Value = 0) and (Value <> FTimeLeft) and (Assigned(FOnTimer) = true);
  if bCondition = true then FOnTimer(Self);

  FTimeLeft := Value;
end;

procedure THourGlass.Tick;
begin
  FTimeLeft:= FTimeLeft - 1;
end;

procedure THourGlass.UpsideDown;
begin
  FTimeLeft:= FInterval - FTimeLeft;
end;

end.
