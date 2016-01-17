unit Base;

interface

uses
  Classes, SysUtils;

type
  TAbstractExpression = class
  private
    function get_Clone:TAbstractExpression; virtual; abstract;
    function is_MyToken(AToken:string):boolean; virtual; abstract;
  public
  end;

  TExpNull = class(TAbstractExpression)
  private
  public
  end;

  TExpIf = class(TAbstractExpression)
  private
  public
  end;

  TExpRepeat = class(TAbstractExpression)
  private
  public
  end;

  TExpressionList = class
  private
    ///<link>aggregation</link>
    ///  <supplierCardinality>1..*</supplierCardinality>
    FItems : array of TAbstractExpression;
  public
    constructor Create;
    destructor Destroy;

    function GetExpression(AToken:string):TAbstractExpression;
  end;

  TContext = class
  private
  public
  end;

  TParsingResult = class
  private
  public
  end;

  TParser = class
  private
    ///<link>aggregationByValue</link>
    FContext  :TContext;
    ///<link>aggregationByValue</link>
    FParsingResult : TParsingResult;
    ///<link>aggregationByValue</link>
    FExpressionList : TExpressionList;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromText(AText:string);
    procedure LoadFromStrean(AStream:TStream);
    procedure LoadFromFile(AFileName:string);
    
    procedure Execute;
  end;

implementation

{ TExpressionList }

constructor TExpressionList.Create;
begin
  inherited;

  SetLength(FItems, Length(FItems)+1); FItems[Length(FItems)] := TExpIf.Create;
  SetLength(FItems, Length(FItems)+1); FItems[Length(FItems)] := TExpRepeat.Create
end;

destructor TExpressionList.Destroy;
var
  Loop : Integer;
begin
  for Loop := Low(FItems) to High(FItems) do FreeAndNil(FItems[Loop]);

  inherited;
end;

function TExpressionList.GetExpression(AToken: string): TAbstractExpression;
var
  Loop : Integer;
begin
  for Loop := Low(FItems) to High(FItems) do
    if FItems[Loop].is_MyToken(AToken) then begin
      Result := FItems[Loop].get_Clone;
      Exit;
    end;

  Result := TExpNull.Create;
end;

{ TParser }

procedure TParser.Clear;
begin

end;

constructor TParser.Create;
begin
  inherited;

end;

destructor TParser.Destroy;
begin

  inherited;
end;

procedure TParser.Execute;
begin

end;

procedure TParser.LoadFromFile(AFileName: string);
begin

end;

procedure TParser.LoadFromStrean(AStream: TStream);
begin

end;

procedure TParser.LoadFromText(AText: string);
begin

end;

end.
