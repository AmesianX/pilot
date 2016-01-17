unit ScanBoard;

interface

uses
  Capture, Slice, EncoderList,
  Classes, Windows, SysUtils, Graphics;

type
  TScanBoard = class(TComponent)
  private
    FCapture     : TCapture;
    FSlice       : TSlice;
    FEncoderList : TEncoderList;
    function  on_NeedData(Sender:TObject;  OldTick:cardinal; var Data: Pointer; var Size: Integer): Boolean;
    procedure on_NewData(Sender:TObject;  var AData:pointer; var ASize:integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

implementation

{ TScanBoard }

constructor TScanBoard.Create(AOwner: TComponent);
begin
  inherited;

  FCapture := TCapture.Create(Self);

  FSlice            := TSlice.Create(Self);
  FSlice.OnNeedData := on_NeedData;
  FSlice.OnNewData  := on_NewData;

  FEncoderList      := TEncoderList.Create(Self);
end;

destructor TScanBoard.Destroy;
begin
  FEncoderList.Free;
  FSlice.Free;
  FCapture.Free;

  inherited;
end;

function TScanBoard.on_NeedData(Sender:TObject; OldTick:cardinal;
  var Data: Pointer; var Size: Integer): Boolean;
begin
   Result := FCapture.Get(OldTick, Data, Size);
end;

procedure TScanBoard.on_NewData(Sender: TObject; var AData: pointer;
  var ASize: integer);
begin
  FEncoderList.Add(AData, ASize);
end;

end.
