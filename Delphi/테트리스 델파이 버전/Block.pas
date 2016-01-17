unit Block;

interface

uses
  Classes, SysUtils, Graphics;

type
  TBlock = class
  private
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    X, Y : Integer;
    procedure Draw;
    procedure DropDown;
    procedure MoveLeft;
    procedure MoveRight;
  published
    property Width : Integer read GetWidth;
    property Height : Integer read GetHeight;
  end;

implementation

uses
  UI_Interface;

{ TBlock }

procedure TBlock.Draw;
begin
  TUI.GetObject._fmMain.BoardCanvas.Brush.Color:= clLime;
  TUI.GetObject._fmMain.BoardCanvas.FillRect(Rect(X*Width, Y*Height, (X+1)*Width, (Y+1)*Height));
  TUI.GetObject._fmMain.BoardCanvas.Pen.Color:= clRed;
  TUI.GetObject._fmMain.BoardCanvas.Rectangle(Rect(X*Width, Y*Height, (X+1)*Width, (Y+1)*Height));
end;

procedure TBlock.DropDown;
begin
  Y:= Y + 1;
end;

function TBlock.GetHeight: Integer;
begin
  Result:= 12;
end;

function TBlock.GetWidth: Integer;
begin
  Result:= 12;
end;

procedure TBlock.MoveLeft;
begin
  X:= X -1;
end;

procedure TBlock.MoveRight;
begin
  X:= X + 1;
end;

end.
