unit _fmMain;

interface

uses
  MegaCastUtils, BlockBuffer, 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Timer: TTimer;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FBlockBuffer : TBlockBuffer;
    FList : TStringList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  Header : TMegaCastBlockUnitHeader;
  BlockUnit : pointer;
  UnitSize : integer;
  Loop: Integer;
  X, Y : Integer;
begin
  BlockUnit := @Header;
  UnitSize := SizeOf(BlockUnit);

  for Y := 0 to 24 - 1 do
  for X := 0 to 32 - 1 do begin
    Header.X := X;
    Header.Y := Y;
    FBlockBuffer.AddBlockUnit(BlockUnit, UnitSize);
  end;

  Exit;

  for Loop := 1 to 1000 do begin
    Header.X := Random(32);
    Header.Y := Random(24);

    FList.Values[Format('%2d-%2d', [Header.Y, Header.X])] := 'X';

    FBlockBuffer.AddBlockUnit(BlockUnit, UnitSize);
  end;
end;

procedure TfmMain.Button2Click(Sender: TObject);
var
  Data : pointer;
  Size : integer;
  Loop: Integer;
  pHeader : ^TMegaCastBlockUnitHeader;
begin
  for Loop := 1 to 1000 do begin
    if FBlockBuffer.GetBlockUnit(Data, Size) then
      try
        pHeader := Data;
        FList.Values[Format('%2d-%2d', [pHeader^.Y, pHeader^.X])] := '';
      finally
        FreeMem(Data);
      end;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FBlockBuffer := TBlockBuffer.Create;
  FBlockBuffer.SetFrameSize(FrameSize(1023, 765));

  FList := TStringList.Create;
end;

end.
