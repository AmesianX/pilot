unit VisualClass;

interface

uses
  Windows, Classes, SysUtils, Controls, Graphics, ExtCtrls;

type
  TVisualClasses = class;

  TVisualClass = class abstract (TObject)
  private
    FVisualClasses : TVisualClasses;
    FWidth : integer;
    FTop : integer;
    FHeight : integer;
    FLeft : integer;
    FSelected : boolean;
    procedure SetHeight(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetSelected(const Value: boolean);
  public
    constructor Create(VisualClasses:TVisualClasses); reintroduce; virtual;
    destructor Destroy; override;

    procedure Draw(Bitmap:TBitmap); virtual; abstract;
    procedure Refresh;

    procedure OnMouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure OnMouseMove(Shift:TShiftState; X,Y:Integer); virtual;
    procedure OnMouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;

    procedure OnKeyDown(var Key:Word; Shift:TShiftState); virtual;
    procedure OnKeyPress(var Key:Char); virtual;
    procedure OnKeyUp(var Key:Word; Shift:TShiftState); virtual;

    function IsMyArea(X,Y:integer):boolean; virtual; abstract;
  published
    property VisualClasses : TVisualClasses read FVisualClasses;
    property Left : integer read FLeft write SetLeft;
    property Top : integer read FTop write SetTop;
    property Width : integer read FWidth write SetWidth;
    property Height : integer read FHeight write SetHeight;
    property Selected : boolean read FSelected write SetSelected;
  end;

  TVisualClasses = class (TGraphicControl)
  private
    FItems : TList;
    FTimer : TTimer;
    FExDrawTime : cardinal;
    FExMousePos : TPoint;
    FGarbageExist : boolean;
    procedure do_Draw;
    procedure on_Timer(Sender:TObject);
    procedure do_DeleteGarbage;
  private
    FBitmap : TBitmap;
    FSelected : TVisualClass;
    FControl: TControl;
    FOnSelected: TNotifyEvent;
    function GetItems(Index: integer): TVisualClass;
    function GetCount: integer;
    function GetDrawInterval: integer;
    procedure SetDrawInterval(const Value: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(Item:TVisualClass);
    procedure Remove(Item:TVisualClass);

    procedure Draw;
    function Select(X,Y:integer):TVisualClass;

    procedure OnMouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure OnMouseMove(Shift:TShiftState; X,Y:Integer);
    procedure OnMouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:Integer);

    procedure OnKeyDown(var Key:Word; Shift:TShiftState);
    procedure OnKeyPress(var Key:Char);
    procedure OnKeyUp(var Key:Word; Shift:TShiftState);

    property Items [Index:integer] : TVisualClass read GetItems;
  published
    property Bitmap : TBitmap read FBitmap;
    property Control : TControl read FControl write FControl;
    property Selected : TVisualClass read FSelected;
    property DrawInterval : integer read GetDrawInterval write SetDrawInterval;
    property Count : integer read GetCount;
    property OnSelected : TNotifyEvent read FOnSelected write FOnSelected;
  end;

implementation

{ TVisualClasses }

procedure TVisualClasses.Add(Item: TVisualClass);
begin
  FItems.Add(Item);
end;

procedure TVisualClasses.Clear;
var
  Loop : Integer;
  Item : TVisualClass;
begin
  FSelected := nil;

  for Loop := 0 to Count - 1 do begin
    Item := Items[Loop];
    if Item = nil then Continue;

    Item.Free;
  end;

  FItems.Clear;

  FGarbageExist := false;
end;

constructor TVisualClasses.Create(AOwner: TComponent);
begin
  inherited;

  FExDrawTime := 0;
  FSelected := nil;
  FGarbageExist := false;

  FItems := TList.Create;
  FBitmap := TBitmap.Create;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := false;
  FTimer.OnTimer := on_Timer;
end;

destructor TVisualClasses.Destroy;
begin
  Clear;
  FTimer.Free;
  FBitmap.Free;
  FItems.Free;

  inherited;
end;

procedure TVisualClasses.do_DeleteGarbage;
var
  Loop : Integer;
begin
  for Loop := Count-1 downto 0 do
    if FItems[Loop] = nil then FItems.Delete(Loop);
  FGarbageExist := false;
end;

procedure TVisualClasses.do_Draw;
var
  Loop : integer;
begin
  FBitmap.Canvas.Brush.Color := $123456;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));

  if FGarbageExist then do_DeleteGarbage;

  for Loop := 0 to Count - 1 do begin
    if Items[Loop] = nil then begin
      FGarbageExist := true;
      Continue;
    end;
    
    Items[Loop].Draw(Bitmap);
  end;

  FExDrawTime := GetTickCount;

  Repaint;
end;

procedure TVisualClasses.Draw;
var
  iTick : cardinal;
begin
  iTick := GetTickCount;
  
  if iTick < FExDrawTime then do_Draw
  else if (iTick-FExDrawTime) < DrawInterval then FTimer.Enabled := true
  else do_Draw;  
end;

function TVisualClasses.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TVisualClasses.GetDrawInterval: integer;
begin
  Result := FTimer.Interval;
end;

function TVisualClasses.GetItems(Index: integer): TVisualClass;
begin
  Result := Pointer(FItems[Index]);
end;

procedure TVisualClasses.OnKeyDown(var Key: Word; Shift: TShiftState);
var
  Loop : integer;
begin
  for Loop := 0 to Count - 1 do begin
    if Items[Loop] = nil then Continue;

    Items[Loop].OnKeyDown(Key, Shift);
  end;
end;

procedure TVisualClasses.OnKeyPress(var Key: Char);
var
  Loop : integer;
begin
  for Loop := 0 to Count - 1 do begin
    if Items[Loop] = nil then Continue;

    Items[Loop].OnKeyPress(Key);
  end;
end;

procedure TVisualClasses.OnKeyUp(var Key: Word; Shift: TShiftState);
var
  Loop : integer;
begin
  for Loop := 0 to Count - 1 do begin
    if Items[Loop] = nil then Continue;

    Items[Loop].OnKeyUp(Key, Shift);
  end;
end;

procedure TVisualClasses.OnMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Loop : integer;
begin
  FExMousePos := Point(X, Y);

  for Loop := 0 to Count - 1 do begin
    if Items[Loop] = nil then Continue;

    Items[Loop].OnMouseDown(Button, Shift, X, Y);
  end;
end;

procedure TVisualClasses.OnMouseMove(Shift: TShiftState; X, Y: Integer);
var
  Loop : integer;
begin
  for Loop := 0 to Count - 1 do begin
    if Items[Loop] = nil then Continue;

    Items[Loop].OnMouseMove(Shift, X, Y);
  end;
end;

procedure TVisualClasses.OnMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Loop : integer;
begin
  for Loop := 0 to Count - 1 do begin
    if Items[Loop] = nil then Continue;

    Items[Loop].OnMouseUp(Button, Shift, X, Y);
  end;
end;

procedure TVisualClasses.on_Timer(Sender: TObject);
begin
  FTimer.Enabled := false;
  if FBitmap = nil then Exit;

  do_Draw;
end;

procedure TVisualClasses.Paint;
begin
  if csDesigning in ComponentState then
     with inherited Canvas do begin
       Pen.Style :=   psDash;
       Brush.Style := bsClear;
       Rectangle(0, 0, Width, Height);
     end;
  if Width*Height = 0 then Exit;

  Canvas.Draw(0, 0, Bitmap);
end;

procedure TVisualClasses.Remove(Item: TVisualClass);
var
  iIndex : integer;
begin
  if Item = Selected then FSelected := nil;

  iIndex := FItems.IndexOf(Item);
  if iIndex = -1 then Exit;

  FItems[iIndex] := nil;
end;

function TVisualClasses.Select(X, Y: integer): TVisualClass;
var
  Loop : integer;
  Item : TVisualClass;
begin
  Result := nil;

  if FGarbageExist then do_DeleteGarbage;

  for Loop := Count - 1 downto 0 do begin
    Item := Items[Loop];
    if Item = nil then begin
      FGarbageExist := true;
      Continue;
    end;

    if Item.IsMyArea(X, Y) then begin
      Result := Item;
      Break;
    end;
  end;
end;

procedure TVisualClasses.SetDrawInterval(const Value: integer);
begin
  FTimer.Interval := Value;
end;

{ TVisualClass }

constructor TVisualClass.Create(VisualClasses:TVisualClasses);
begin
  inherited Create;

  FSelected := false;

  VisualClasses.Add(Self);
end;

destructor TVisualClass.Destroy;
begin
  VisualClasses.Remove(Self);

  inherited;
end;

procedure TVisualClass.OnKeyDown(var Key: Word; Shift: TShiftState);
begin
end;

procedure TVisualClass.OnKeyPress(var Key: Char);
begin
end;

procedure TVisualClass.OnKeyUp(var Key: Word; Shift: TShiftState);
begin
end;

procedure TVisualClass.OnMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TVisualClass.OnMouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TVisualClass.OnMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TVisualClass.Refresh;
begin
  if VisualClasses <> nil then VisualClasses.Draw;
end;

procedure TVisualClass.SetHeight(const Value: integer);
begin
  FHeight := Value;
  Refresh;
end;

procedure TVisualClass.SetLeft(const Value: integer);
begin
  FLeft := Value;
  Refresh;
end;

procedure TVisualClass.SetSelected(const Value: boolean);
begin
  FSelected := Value;
  Refresh;
end;

procedure TVisualClass.SetTop(const Value: integer);
begin
  FTop := Value;
  Refresh;
end;

procedure TVisualClass.SetWidth(const Value: integer);
begin
  FWidth := Value;
  Refresh;
end;

end.
