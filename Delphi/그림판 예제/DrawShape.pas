unit DrawShape;

interface

uses
  SysUtils, Graphics;

type
  TADrawShape = class
  private
    function GetPen: TPen;
    function GetBrush: TBrush;
  protected
    FCanvas : TCanvas;
    FIsDrawing: boolean;
    procedure SetIsDrawing(const Value: boolean);
  public
    X1, Y1, X2, Y2 : Integer;
    constructor Create(Canvas:TCanvas);
    procedure BeginDraw(X,Y:Integer); virtual; abstract;
    procedure Drawing(X,Y:Integer);   virtual; abstract;
    procedure EndDraw(X,Y:Integer);   virtual; abstract;
  published
    property Pen : TPen read GetPen;
    property Brush : TBrush read GetBrush;
    property IsDrawing : boolean read FIsDrawing write SetIsDrawing;
  end;

  TDrawCircle = class(TADrawShape)
  private
  public
    procedure BeginDraw(X,Y:Integer); override;
    procedure Drawing(X,Y:Integer);   override;
    procedure EndDraw(X,Y:Integer);   override;
  end;

  TDrawBox = class(TADrawShape)
  private
  public
    procedure BeginDraw(X,Y:Integer); override;
    procedure Drawing(X,Y:Integer);   override;
    procedure EndDraw(X,Y:Integer);   override;
  end;

implementation

{ TDrawCircle }

procedure TDrawCircle.BeginDraw(X,Y:Integer);
begin
  X1:= X;
  Y1:= Y;
  X2:= X;
  Y2:= Y;
  IsDrawing:= true;
end;

procedure TDrawCircle.Drawing(X,Y:Integer);
begin
  FCanvas.Ellipse(X1, Y1, X2, Y2);
  X2:= X;
  Y2:= Y;
  FCanvas.Ellipse(X1, Y1, X2, Y2);
end;

procedure TDrawCircle.EndDraw(X,Y:Integer);
begin
  IsDrawing:= false;
  
  X2:= X;
  Y2:= Y;
  FCanvas.Ellipse(X1, Y1, X2, Y2);
end;

{ TDrawBox }

procedure TDrawBox.BeginDraw(X,Y:Integer);
begin
end;

procedure TDrawBox.Drawing(X,Y:Integer);
begin
end;

procedure TDrawBox.EndDraw(X,Y:Integer);
begin
end;

{ TADrawShape }

constructor TADrawShape.Create(Canvas: TCanvas);
begin
  inherited Create;
  
  FCanvas:= Canvas;
end;

function TADrawShape.GetBrush: TBrush;
begin
  Result:= FCanvas.Brush;
end;

function TADrawShape.GetPen: TPen;
begin
  Result:= FCanvas.Pen;
end;

procedure TADrawShape.SetIsDrawing(const Value: boolean);
begin
  FIsDrawing := Value;

  if Value = true then 
    FCanvas.Pen.Mode:= pmNotXor
  else
    FCanvas.Pen.Mode:= pmCopy;
end;

end.
