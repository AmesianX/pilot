unit RotateBlocks;

interface

uses
  Classes, SysUtils, Block, Dialogs;

type
  TRotateBlocks = class
  private
    FBlockList : TList;
    function do_GetStartX:Integer;
    function do_GetEndX:Integer;
    function do_GetEndY:Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Rotate(List:TList);
  end;  

implementation

{ TRotateBlocks }

constructor TRotateBlocks.Create;
begin
  inherited;

  FBlockList:= TList.Create;
end;

destructor TRotateBlocks.Destroy;
begin
  FBlockList.Free;

  inherited;
end;

function TRotateBlocks.do_GetEndX: Integer;
var
  Loop, X : Integer;
begin
	Result:= -100;

  for Loop := 0 to FBlockList.Count - 1 do begin
    X:= TBlock(FBlockList.Items[Loop]).X;
    if X > Result then Result:= X;
  end;

  if Result < 0 then Result:= 0;
end;

function TRotateBlocks.do_GetEndY: Integer;
var
  Loop, Y : Integer;
begin
	Result:= -100;

  for Loop := 0 to FBlockList.Count - 1 do begin
    Y:= TBlock(FBlockList.Items[Loop]).Y;
    if Y > Result then Result:= Y;
  end;

  if Result < 0 then Result:= 0;
end;

function TRotateBlocks.do_GetStartX: Integer;
var
  Loop, X : Integer;
begin
  Result:= 100;

  for Loop := 0 to FBlockList.Count - 1 do begin
    X:= TBlock(FBlockList.Items[Loop]).X;
    if X < Result then Result:= X;
  end;

	if Result > 9 then Result:= 9;
end;

class procedure TRotateBlocks.Rotate(List: TList);
var
  Loop, X, Y, StartX, EndX, EndY : Integer;
  Block : TBlock;
  RotateBlocks : TRotateBlocks;
begin
  RotateBlocks:= TRotateBlocks.Create;
  try
    RotateBlocks.FBlockList.Assign(List);

    StartX:= RotateBlocks.do_GetStartX;
    EndX:= RotateBlocks.do_GetEndX;
    EndY:= RotateBlocks.do_GetEndY;
    
    for Loop := 0 to List.Count - 1 do begin
      Block:= Pointer(List.Items[Loop]);
      X:= Block.X;
      Y:= Block.Y;
      Block.X:= StartX + (EndY - Y);
      Block.Y:= EndY   + (X - EndX);
    end;
  finally
    RotateBlocks.Free;
  end;
end;

end.
