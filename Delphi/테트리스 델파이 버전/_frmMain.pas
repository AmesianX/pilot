unit _frmMain;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms;

type
  T_fmMain = class(TForm)
  private
  protected
    function GetBoardCanvas: TCanvas; virtual; abstract;
    procedure on_KeyDown(Key:Word);
    procedure on_Start;
  public
    constructor Create(AOwner:TComponent); override;
    procedure InitBoardCanvas;
    procedure GameEnd; virtual; abstract;
  published
    property BoardCanvas : TCanvas read GetBoardCanvas;
  end;

implementation

uses
  UI_Interface;

type
  TFUI = class(TUI)
  end;

{ T_fmMain }

constructor T_fmMain.Create(AOwner: TComponent);
begin
  inherited;

  TUI.GetObject._fmMain:= Self;
end;

procedure T_fmMain.on_KeyDown(Key: Word);
begin
  TFUI(TUI.GetObject).on_KeyDown(Key);
end;

procedure T_fmMain.on_Start;
begin
  TFUI(TUI.GetObject).on_Start;
end;

procedure T_fmMain.InitBoardCanvas;
var
  Loop: Integer;
begin
  BoardCanvas.Brush.Color:= $004F4F4F;
  BoardCanvas.FillRect(Rect(0, 0, 120, 240));

  // 배경에 수직선 그리기
  for Loop := 1 to 10 do begin
    BoardCanvas.Pen.Color:= clWhite;
    BoardCanvas.MoveTo((Loop-1)*12,   0);
    BoardCanvas.LineTo((Loop-1)*12, 240);
  end; 
end;

end.
