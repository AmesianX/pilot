unit Tokens;

interface

uses
  ParserUtils, Functions,
  Classes, SysUtils, TypInfo, Math;

const
  _PriorityRightParen = 1;

  _PriorityPlus = _PriorityRightParen + 1;
  _PriorityMius = _PriorityRightParen + 1;

  _PriorityMul = _PriorityPlus + 1;
  _PriorityDiv = _PriorityPlus + 1;

  _PriorityPower = _PriorityMul + 1;

  _PriorityNumber = _PriorityPower + 1;

  _PriorityFunction = _PriorityNumber + 1;
  
  _PriorityLeftParen = _PriorityFunction + 1;

type
  TToken = class
  private
    function GetTokenName: string;
    function GetSize: integer;
  protected
    FIndex : integer;
    FPriority : integer;
    FTokenType : TScannerTokenType;
    FText : string;
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); reintroduce; virtual;

    function IsSameToken(AText:string):boolean;
    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; virtual;

    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; virtual;
    function Copy:TToken; virtual;

    property Index : integer read FIndex;
    property Priority : integer read FPriority;
    property TokenType : TScannerTokenType read FTokenType;
    property Text : string read FText;
    property Size : integer read GetSize;
    property TokenName : string read GetTokenName;
  end;

  TParenthesis  = class abstract (TToken)
  private
  public
  end;

  TLeftParen = class (TParenthesis)
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); override;

    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; override;
    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; override;
    function Copy:TToken; override;
  end;

  TFunction = class (TToken)
  private
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); override;

    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; override;
    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; override;
    function Copy:TToken; override;

    function Calc(AValue:extended):extended;
  end;

  TNumber = class (TToken)
  private
    FValue: extended;
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); override;

    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; override;
    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; override;
    function Copy:TToken; override;
  public
    property Value : extended read FValue;
  end; 

  TOperator = class abstract (TToken)
  private
  public
    function Calc(ALeft,ARight:extended):extended; virtual; abstract;
  end;

  TOperatorPower = class (TOperator)
  private
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); override;

    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; override;
    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; override;
    function Copy:TToken; override;

    function Calc(ALeft,ARight:extended):extended; override;
  end;

  TOperatorMul = class (TOperator)
  private
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); override;

    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; override;
    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; override;
    function Copy:TToken; override;

    function Calc(ALeft,ARight:extended):extended; override;
  end;

  TOperatorDiv = class (TOperator)
  private
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); override;

    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; override;
    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; override;
    function Copy:TToken; override;

    function Calc(ALeft,ARight:extended):extended; override;
  end;

  TOperatorPlus = class (TOperator)
  private
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); override;

    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; override;
    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; override;
    function Copy:TToken; override;

    function Calc(ALeft,ARight:extended):extended; override;
  end;

  TOperatorMinus = class (TOperator)
  private
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); override;

    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; override;
    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; override;
    function Copy:TToken; override;

    function Calc(ALeft,ARight:extended):extended; override;
  end;

  TRightParen = class (TParenthesis)
  public
    constructor Create(ATokenType:TScannerTokenType; AText:string); override;

    function IsMyType(ATokenType:TScannerTokenType; AText:string):boolean; override;
    function Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string):TToken; override;
    function Copy:TToken; override;
  end;

implementation

{ TToken }

function TToken.Clone(AIndex: integer; ATokenType: TScannerTokenType;
  AText: string): TToken;
begin
  Result := nil;
end;

function TToken.Copy: TToken;
begin
  Result := nil;
end;

constructor TToken.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited Create;

  FPriority := 0;

  FTokenType := ATokenType;
  FText := AText;
end;

function TToken.GetSize: integer;
begin
  Result := Length(FText);
end;

function TToken.GetTokenName: string;
begin
  Result := GetEnumName(TypeInfo(TScannerTokenType), Integer(TokenType));
end;

function TToken.IsSameToken(AText: string): boolean;
begin
  Result := LowerCase(Text) = LowerCase(AText);
end;

function TToken.IsMyType(ATokenType: TScannerTokenType; AText: string): boolean;
begin
  Result := false;
end;

{ TLeftParen }

function TLeftParen.Clone(AIndex:integer; ATokenType: TScannerTokenType; AText: string): TToken;
begin
  Result := TLeftParen.Create(ATokenType, AText);
  Result.FIndex := AIndex;
end;

function TLeftParen.Copy: TToken;
begin
  Result := TLeftParen.Create(TokenType, Text);
  Result.FIndex := Index;
end;

constructor TLeftParen.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited;

  FPriority := _PriorityLeftParen;
end;

function TLeftParen.IsMyType(ATokenType: TScannerTokenType;
  AText: string): boolean;
begin
  Result := AText = '(';
end;

{ TFunction }

function TFunction.Calc(AValue: extended): extended;
begin
  Result := TFunctions.Obj.Calc(Text, AValue);
end;

function TFunction.Clone(AIndex: integer; ATokenType: TScannerTokenType;
  AText: string): TToken;
begin
  Result := TFunction.Create(ATokenType, AText);
  Result.FIndex := AIndex;
end;

function TFunction.Copy: TToken;
begin
  Result := TFunction.Create(TokenType, Text);
  Result.FIndex := Index;
end;

constructor TFunction.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited;

  FPriority := _PriorityFunction;
end;

function TFunction.IsMyType(ATokenType: TScannerTokenType;
  AText: string): boolean;
begin
  Result := ATokenType = ttIdentifier;
end;

{ TNumber }

function TNumber.Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string): TToken;
begin
  Result := TNumber.Create(ATokenType, AText);
  Result.FIndex := AIndex;
end;

function TNumber.Copy: TToken;
begin
  Result := TNumber.Create(TokenType, Text);
  Result.FIndex := Index;
end;

constructor TNumber.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited;

  FPriority := _PriorityNumber;
  FValue := StrToFloatDef(AText, 0);
end;

function TNumber.IsMyType(ATokenType: TScannerTokenType;
  AText: string): boolean;
begin
  Result := ATokenType = ttNumber;  
end;

{ TOperatorPower }

function TOperatorPower.Calc(ALeft, ARight: extended): extended;
begin
  Result := Power(ALeft, ARight);
end;

function TOperatorPower.Clone(AIndex: integer; ATokenType: TScannerTokenType;
  AText: string): TToken;
begin
  Result := TOperatorPower.Create(ATokenType, AText);
  Result.FIndex := AIndex;
end;

function TOperatorPower.Copy: TToken;
begin
  Result := TOperatorPower.Create(TokenType, Text);
  Result.FIndex := Index;
end;

constructor TOperatorPower.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited;

  FPriority := _PriorityPower;
end;

function TOperatorPower.IsMyType(ATokenType: TScannerTokenType;
  AText: string): boolean;
begin
  Result := AText = '^';
end;

{ TOperatorMul }

function TOperatorMul.Calc(ALeft, ARight: extended): extended;
begin
  Result := ALeft * ARight;
end;

function TOperatorMul.Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string): TToken;
begin
  Result := TOperatorMul.Create(ATokenType, AText);
  Result.FIndex := AIndex;
end;

function TOperatorMul.Copy: TToken;
begin
  Result := TOperatorMul.Create(TokenType, Text);
  Result.FIndex := Index;
end;

constructor TOperatorMul.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited;

  FPriority := _PriorityMul;
end;

function TOperatorMul.IsMyType(ATokenType: TScannerTokenType;
  AText: string): boolean;
begin
  Result := AText = '*';
end;

{ TOperatorDiv }

function TOperatorDiv.Calc(ALeft, ARight: extended): extended;
begin
  Result := ALeft / ARight;
end;

function TOperatorDiv.Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string): TToken;
begin
  Result := TOperatorDiv.Create(ATokenType, AText);
  Result.FIndex := AIndex;
end;

function TOperatorDiv.Copy: TToken;
begin
  Result := TOperatorDiv.Create(TokenType, Text);
  Result.FIndex := Index;
end;

constructor TOperatorDiv.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited;

  FPriority := _PriorityDiv;
end;

function TOperatorDiv.IsMyType(ATokenType: TScannerTokenType;
  AText: string): boolean;
begin
  Result := AText = '/';
end;

{ TOperatorPlus }

function TOperatorPlus.Calc(ALeft, ARight: extended): extended;
begin
  Result := ALeft + ARight;
end;

function TOperatorPlus.Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string): TToken;
begin
  Result := TOperatorPlus.Create(ATokenType, AText);
  Result.FIndex := AIndex;
end;

function TOperatorPlus.Copy: TToken;
begin
  Result := TOperatorPlus.Create(TokenType, Text);
  Result.FIndex := Index;
end;

constructor TOperatorPlus.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited;

  FPriority := _PriorityPlus;
end;

function TOperatorPlus.IsMyType(ATokenType: TScannerTokenType;
  AText: string): boolean;
begin
  Result := AText = '+';
end;

{ TOperatorMinus }

function TOperatorMinus.Calc(ALeft, ARight: extended): extended;
begin
  Result := ALeft - ARight;
end;

function TOperatorMinus.Clone(AIndex:integer; ATokenType:TScannerTokenType; AText:string): TToken;
begin
  Result := TOperatorMinus.Create(ATokenType, AText);
  Result.FIndex := AIndex;
end;

function TOperatorMinus.Copy: TToken;
begin
  Result := TOperatorMinus.Create(TokenType, Text);
  Result.FIndex := Index;
end;

constructor TOperatorMinus.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited;

  FPriority := _PriorityMius;
end;

function TOperatorMinus.IsMyType(ATokenType: TScannerTokenType;
  AText: string): boolean;
begin
  Result := AText = '-';
end;

{ TRightParen }

function TRightParen.Clone(AIndex:integer; ATokenType: TScannerTokenType;
  AText: string): TToken;
begin
  Result := TRightParen.Create(ATokenType, AText);
  Result.FIndex := AIndex;
end;

function TRightParen.Copy: TToken;
begin
  Result := TRightParen.Create(TokenType, Text);
  Result.FIndex := Index;
end;

constructor TRightParen.Create(ATokenType: TScannerTokenType; AText: string);
begin
  inherited;

  FPriority := _PriorityRightParen;
end;

function TRightParen.IsMyType(ATokenType: TScannerTokenType;
  AText: string): boolean;
begin
  Result := AText = ')';
end;

end.
