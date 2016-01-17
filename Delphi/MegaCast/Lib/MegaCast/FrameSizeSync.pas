unit FrameSizeSync;

interface

uses
  MegaCastUtils,
  Classes, SysUtils, SyncObjs;

type
  TFrameSizeSync = class
  private
    FFrameSize : TFrameSize;
    FCS : TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetFrameSize(AFrameSize:TFrameSize);
    procedure GetFrameSize(var AFrameSize:TFrameSize);
  end;

implementation

{ TFrameSizeSync }

constructor TFrameSizeSync.Create;
begin
  inherited;

  FCS := TCriticalSection.Create;
end;

destructor TFrameSizeSync.Destroy;
begin
  FreeAndNil(FCS);

  inherited;
end;

procedure TFrameSizeSync.GetFrameSize(var AFrameSize: TFrameSize);
begin
  FCS.Enter;
  try
    AFrameSize := FFrameSize;
  finally
    FCS.Leave;
  end;
end;

procedure TFrameSizeSync.SetFrameSize(AFrameSize: TFrameSize);
begin
  FCS.Enter;
  try
    FFrameSize := AFrameSize;
  finally
    FCS.Leave;
  end;
end;

end.
