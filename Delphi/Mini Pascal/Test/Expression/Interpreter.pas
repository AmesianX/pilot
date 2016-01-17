unit Interpreter;

interface

uses
  Classes, SysUtils;

type
  TContext = class
  private
    FLines : TStringList;
    FCurrentToken: string;
  public
    constructor Create(ALines:string); reintroduce;
    destructor Destroy; override;

    function NextToken:string;
    procedure SkipToken(AToken:string);

    property CurrentToken:string read FCurrentToken;
  end;

  TAbstractNode = class
  private
  public
    procedure Parse(Context:TContext); virtual; abstract;
  end;

  TTerminalNode = class (TAbstractNode)
  private
  public
  end;

  TNonTerminalNode = class (TAbstractNode)
  private
    FNodes : array of TAbstractNode;
    procedure do_AddNode(ANode:TAbstractNode);
  public
  end;

  TExpression = class (TNonTerminalNode)
  private
  public
    procedure Parse(AContext:TContext); override;
  end;

  TTerm = class (TNonTerminalNode)
  private
  public
    procedure Parse(AContext:TContext); override;
  end;

  TSignTerm = class (TNonTerminalNode)
  private
  public
    procedure Parse(AContext:TContext); override;
  end;

  TFactor = class (TNonTerminalNode)
  private
  public
    procedure Parse(AContext:TContext); override;
  end;

  TMulOperator = class (TTerminalNode)
  private
  public
    procedure Parse(AContext:TContext); override;
  end;

  TAddOperator = class (TTerminalNode)
  private
  public
    procedure Parse(AContext:TContext); override;
  end;

implementation

function is_AddOpp(AToken:string):boolean;
begin
  Result := (AToken = '-') or (AToken = '+');
end;

{ TContext }

constructor TContext.Create(ALines: string);
begin
  inherited Create;

  FLines := TStringList.Create;
  FLines.Text := LowerCase(ALines);

  NextToken;
end;

destructor TContext.Destroy;
begin
  FreeAndNil(FLines);

  inherited;
end;

function TContext.NextToken: string;
begin
  if FLines.Count = 0 then begin
    FCurrentToken := '';
  end else begin
    FCurrentToken := FLines[0];
    FLines.Delete(0);
  end;

  Result := FCurrentToken;
end;

procedure TContext.SkipToken(AToken:string);
begin
  AToken := LowerCase(AToken);
  
  if (FLines.Count = 0) or (CurrentToken <> AToken) then
    raise Exception.Create(Format('TContext.SkipToken: %s is expected, but can''t find.', [AToken]));

  NextToken;
end;

{ TExpression }

procedure TExpression.Parse(AContext: TContext);
var
  Node : TAbstractNode;
begin
  while AContext.CurrentToken <> '' do begin
    if is_AddOpp(AContext.CurrentToken) then begin
      Node := TSignTerm.Create;
      Node.Parse(AContext);
    end;

  end;
end;

{ TNonTerminalNode }

procedure TNonTerminalNode.do_AddNode(ANode: TAbstractNode);
begin
  SetLength(FNodes, Length(FNodes)+1);
  FNodes[Length(FNodes)-1] := ANode;
end;

{ TTerm }

procedure TTerm.Parse(AContext: TContext);
begin

end;

{ TFactor }

procedure TFactor.Parse(AContext: TContext);
begin

end;

{ TMulOperator }

procedure TMulOperator.Parse(AContext: TContext);
begin

end;

{ TAddOperator }

procedure TAddOperator.Parse(AContext: TContext);
begin

end;

{ TSignTerm }

procedure TSignTerm.Parse(AContext: TContext);
begin

end;

end.
