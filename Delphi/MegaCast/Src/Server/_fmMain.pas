unit _fmMain;

interface

uses
  MegaCastUtils, RoomUnit,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

const
  _RoomCount = 10;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRoomList : array of TRoomUnit;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
var
  Loop: Integer;
begin
  SetLength(FRoomList, _RoomCount);
  for Loop := 0 to _RoomCount - 1 do begin
    FRoomList[Loop] := TRoomUnit.Create(Self);
    FRoomList[Loop].RoomNo := Loop + 1;
    FRoomList[Loop].Start;
  end;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
var
  Loop: Integer;
begin
  for Loop := 0 to _RoomCount - 1 do begin
    FRoomList[Loop].Stop;
    FreeAndNil(FRoomList[Loop]);
  end;
end;

end.
