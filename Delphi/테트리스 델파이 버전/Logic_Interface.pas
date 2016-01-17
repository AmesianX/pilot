unit Logic_Interface;

interface

uses
  BlockShape, BlockCell, GameTimer, Windows, Classes, SysUtils;

type
  TLI = class
  private
    function GetGameStarted: Boolean;
  protected
    procedure on_GameEnd;
    procedure on_Tick;
  public
    GameTimer : TGameTimer;
    BlockCell : TBlockCell;
    BlockShape : TBlockShape;
    constructor Create;
    destructor Destroy; override;
    class function GetObject:TLI;
    procedure StartGame;
    procedure Draw;
    procedure Process_KeyDown(Key:Word);
  published
    property GameStarted : Boolean read GetGameStarted;
  end;

implementation

uses
  UI_Interface;

var
  MyObject : TLI = Nil;

{ TLI }

function TLI.GetGameStarted: Boolean;
begin
  Result:= GameTimer.Enabled;
end;

class function TLI.GetObject: TLI;
begin
  if MyObject = Nil then MyObject:= TLI.Create;
  Result:= MyObject;
end;

procedure TLI.StartGame;
begin
  GameTimer.Enabled:= True;
end;

procedure TLI.on_GameEnd;
begin
  GameTimer.Enabled:= False;
  TUI.GetObject.GameEnd;
end;

procedure TLI.on_Tick;
begin
  if BlockShape.IsEmpty = True then BlockShape.CreateNext;
  Self.Process_KeyDown(VK_Down);
end;

constructor TLI.Create;
begin
  inherited;

  GameTimer:=  TGameTimer.Create;
  BlockCell:=  TBlockCell.Create;
  BlockShape:= TBlockShape.Create;
end;

destructor TLI.Destroy;
begin
  GameTimer.Free;
  BlockCell.Free;
  BlockShape.Free;

  inherited;
end;

procedure TLI.Draw;
begin
  TUI.GetObject._fmMain.InitBoardCanvas;
  BlockCell.Draw;
  BlockShape.Draw;
end;

procedure TLI.Process_KeyDown(Key:Word);
begin
  if BlockShape.IsEmpty = False then BlockShape.Process_KeyDown(Key);
  Self.Draw;
end;

end.
