unit BlockShape;

interface

uses
  Classes, SysUtils, Windows, Block;

type
  TBlockShape = class
  private
    FIsClone : Boolean;
    FBlockList : TList;
    function Clone:TBlockShape;
    function GetIsEmpty: Boolean;
    procedure do_MoveLeft;
    procedure do_MoveRight;
    procedure do_MoveDown;
    procedure do_Drop;
    function do_CheckCollision(Key:Word):Boolean;
    procedure do_Move(Key:Word);
    procedure do_CreateBlockShape1;
    procedure do_CreateBlockShape2;
    procedure do_CreateBlockShape3;
    procedure do_CreateBlockShape4;
    procedure do_CreateBlockShape5;
    procedure do_CreateBlockShape6;
    procedure do_CreateBlockShape7;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure Process_KeyDown(Key:Word);
    procedure CreateNext;
  published
    property IsEmpty : Boolean read GetIsEmpty;
  end;

implementation

uses
  Logic_Interface, RotateBlocks;

constructor TBlockShape.Create;
begin
  inherited;

  FIsClone:= False;

  Randomize;

  FBlockList:= TList.Create;
end;

destructor TBlockShape.Destroy;
var
  Loop : Integer;
begin
  for Loop := 0 to FBlockList.Count - 1 do
    TBlock(FBlockList.Items[Loop]).Free;

  FBlockList.Free;

  inherited;
end;

procedure TBlockShape.Draw;
var
  Loop: Integer;
begin
  for Loop := 0 to FBlockList.Count - 1 do
    TBlock(FBlockList.Items[Loop]).Draw;
end;

procedure TBlockShape.CreateNext;
begin
  case Round(Random(7)) of
       0 : do_CreateBlockShape1;
       1 : do_CreateBlockShape2;
       2 : do_CreateBlockShape3;
       3 : do_CreateBlockShape4;
       4 : do_CreateBlockShape5;
       5 : do_CreateBlockShape6;
       6 : do_CreateBlockShape7;
  end;
end;

procedure TBlockShape.do_MoveLeft;
var
  Loop: Integer;
begin
  for Loop := 0 to FBlockList.Count - 1 do
    TBlock(FBlockList.Items[Loop]).MoveLeft;
end;

procedure TBlockShape.do_MoveRight;
var
  Loop: Integer;
begin
  for Loop := 0 to FBlockList.Count - 1 do
    TBlock(FBlockList.Items[Loop]).MoveRight;
end;

function TBlockShape.do_CheckCollision(Key:Word):Boolean;
var
  CloneShape : TBlockShape;
begin
  if FIsClone = False then begin
    CloneShape:= Self.Clone;
    try
      CloneShape.Process_KeyDown(Key);
      Result:= TLI.GetObject.BlockCell.CheckCollision(CloneShape.FBlockList, Key);
    finally
      CloneShape.Free;
    end;
  end else begin
    Result:= False;
  end;
end;

procedure TBlockShape.do_MoveDown;
var
  Loop: Integer;
begin
  for Loop := 0 to FBlockList.Count - 1 do
    TBlock(FBlockList.Items[Loop]).DropDown;
end;

procedure TBlockShape.do_Drop;
var
  bCanMove : Boolean;
begin
  Repeat
    bCanMove:= Self.do_CheckCollision(VK_Down);
    if bCanMove = True then begin
      TLI.GetObject.BlockCell.BlockLanding(Self.FBlockList);
    end
    else
      do_MoveDown;
  until bCanMove = True;
end;

procedure TBlockShape.do_Move(Key:Word);
begin
  case Key of
       VK_Left :  do_MoveLeft;
       VK_Right : do_MoveRight;
       VK_Down :  do_MoveDown;
       VK_Up :    TRotateBlocks.Rotate(FBlockList);
       VK_Space : do_Drop;
  end;
end;

procedure TBlockShape.Process_KeyDown(Key:Word);
begin
  if Key <> VK_Space then
  if Self.do_CheckCollision(Key) = True then begin
    if Key = VK_Down then TLI.GetObject.BlockCell.BlockLanding(Self.FBlockList);    
    Exit;
  end;

  do_Move(Key);
end;

function TBlockShape.GetIsEmpty: Boolean;
begin
  Result:= FBlockList.Count = 0;
end;

function TBlockShape.Clone: TBlockShape;
var
  Loop: Integer;
  Block : TBlock;
begin
  Result:= TBlockShape.Create;
  Result.FIsClone:= True;
  for Loop := 0 to FBlockList.Count - 1 do begin
    Block:= TBlock.Create;
    Block.X:= TBlock(FBlockList.Items[Loop]).X;
    Block.Y:= TBlock(FBlockList.Items[Loop]).Y;
    Result.FBlockList.Add(Block);
  end;
end;

procedure TBlockShape.do_CreateBlockShape1;
var
  Block : TBlock;
begin
	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -1;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -1;
	FBlockList.Add(Block);
end;

procedure TBlockShape.do_CreateBlockShape2;
var
  Block : TBlock;
begin
	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -3;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -1;
	FBlockList.Add(Block);
end;

procedure TBlockShape.do_CreateBlockShape3;
var
  Block : TBlock;
begin
	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -3;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -1;
	FBlockList.Add(Block);
end;

procedure TBlockShape.do_CreateBlockShape4;
var
  Block : TBlock;
begin
	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -3;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -1;
	FBlockList.Add(Block);
end;

procedure TBlockShape.do_CreateBlockShape5;
var
  Block : TBlock;
begin
	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -4;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -3;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -1;
	FBlockList.Add(Block);
end;

procedure TBlockShape.do_CreateBlockShape6;
var
  Block : TBlock;
begin
	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -3;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -1;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -1;
	FBlockList.Add(Block);
end;

procedure TBlockShape.do_CreateBlockShape7;
var
  Block : TBlock;
begin
	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -3;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -2;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 5;
	Block.Y:= -1;
	FBlockList.Add(Block);

	Block:= TBlock.Create;
	Block.X:= 4;
	Block.Y:= -1;
	FBlockList.Add(Block);
end;

end.
