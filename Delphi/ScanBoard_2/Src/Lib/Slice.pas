unit Slice;

interface

uses
  ThreadRepeater,
  Classes, SysUtils, Windows, Graphics;

type
  TNeedBitmapEvent = function  (Sender: TObject;  OldTick: Cardinal; var AData: Pointer; var ASize: Integer):boolean of object;
  TNewDataEvent    = procedure (Sender: TObject; var AData: Pointer; var ASize: Integer) of object;

  TSlice = class(TComponent)
  private
    FThreadRepeater : TThreadRepeater;
    FOldTick        : Cardinal;
    procedure do_Execute(Sender: TObject);
    procedure do_Slice(AData: Pointer; ASize: Integer);
  private
    FOnNeedData : TNeedBitmapEvent;
    FOnNewData  : TNewDataEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property OnNeedData : TNeedBitmapEvent read FOnNeedData write FOnNeedData;
    property OnNewData  : TNewDataEvent    read FOnNewData  write FOnNewData;
  end;

implementation

{ TSlice }

constructor TSlice.Create(AOwner: TComponent);
begin
  inherited;
  
  FThreadRepeater := TThreadRepeater.Create(Self);
  FThreadRepeater.Execute(do_Execute);
end;

destructor TSlice.Destroy;
begin
  FThreadRepeater.Stop;
//  FThreadRepeater.Free;

  inherited;
end;

procedure TSlice.do_Execute(Sender: TObject);
var
  Data: Pointer;
  Size: Integer;
begin
  if Assigned(FOnNeedData) then
  begin
    if FOnNeedData(Self, FOldTick, Data, Size) then
    try
      do_Slice(Data, Size);
    finally
      FreeMem(Data);
    end;
  end;
end;

procedure TSlice.do_Slice(AData: Pointer; ASize: Integer);
begin
  // Todo : Repeate
  if Assigned(FOnNewData) then FOnNewData(Self, AData, ASize);  
end;

end.
