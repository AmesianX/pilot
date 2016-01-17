unit BlockCell;

interface

uses
  Classes, SysUtils, Graphics, Block;

type
  TBlockCell = class
  private
    FCells : Array [0..9, 0..19] of TBlock;
    function do_CheckBlocksInCells(List:TLIst):Boolean;
  public
    constructor Create;
    procedure Draw;
    procedure Clear;
    procedure BlockLanding(List:TList);
    function CheckCollision(List:TList; Key:Word):Boolean;
  end;

implementation

uses
  Logic_Interface;

type
  TFLI = class(TLI)
  end;

procedure TBlockCell.Draw;
var
  LoopX: Integer;
  LoopY: Integer;
begin
  for LoopX := 0 to 10 - 1 do
  for LoopY := 0 to 20 - 1 do
    if FCells[LoopX, LoopY] <> Nil then FCells[LoopX, LoopY].Draw;
end;

procedure TBlockCell.Clear;
var
  LoopX: Integer;
  LoopY: Integer;
begin
  for LoopX := 0 to 10 - 1 do
  for LoopY := 0 to 20 - 1 do
    if FCells[LoopX, LoopY] <> Nil then begin
      FCells[LoopX, LoopY].Free;
      FCells[LoopX, LoopY]:= Nil;
    end;
end;

constructor TBlockCell.Create;
var
  LoopX: Integer;
  LoopY: Integer;
begin
  inherited;

  for LoopX := 0 to 10 - 1 do
  for LoopY := 0 to 20 - 1 do
    FCells[LoopX, LoopY]:= Nil;
end;

procedure TBlockCell.BlockLanding(List:TList);
var
  Loop: Integer;
  Block  : TBlock;
  bGameEnded : Boolean;
begin
  bGameEnded:= False;
  for Loop := 0 to List.Count - 1 do begin
    Block:= Pointer(List.Items[Loop]);
    if (Block.X in [0..9]) and (Block.Y in [0..19]) then
      FCells[Block.X, Block.Y]:= Block
    else
      bGameEnded:= True;
  end;
  List.Clear;

  if bGameEnded = True then TFLI(TLI.GetObject).on_GameEnd;
end;

function TBlockCell.do_CheckBlocksInCells(List: TList): Boolean;
var
  Loop: Integer;
  Block : TBlock;
begin
  Result:= False; 
  for Loop := 0 to List.Count - 1 do begin
    Block:= TBlock(List.Items[Loop]);
    if (Block.X in [0..9]) and (Block.Y in [0..19]) then
    if FCells[Block.X, Block.Y] <> Nil then begin
      Result:= True;
      Break;
    end;
  end;
end;

function TBlockCell.CheckCollision(List:TList; Key:Word):Boolean;
var
  Loop: Integer;
  Block  : TBlock;
  bCondition1, bCondition2, bCondition3 : Boolean;
begin
  Result:= False;

  for Loop := 0 to List.Count - 1 do begin
    Block:= Pointer(List.Items[Loop]);

    bCondition1:= (Block.X < 0) or (Block.X > 9);
    bCondition2:= (Block.Y > 19);
    bCondition3:= do_CheckBlocksInCells(List);
    if bCondition1 or bCondition2 or bCondition3 then begin
      Result:= True;
      Break;
    end;
  end;
end;

end.
