unit Calculator;

interface

uses
  ParserUtils, Tokens, ParsingContext, Infix2Postfix, PolishParser,
  Classes, SysUtils;

type
  TCalculator = class
  private
    FParsingContext : TParsingContext;
    FInfix2Postfix : TInfix2Postfix;
    FPolishParser : TPolishParser;
  private
    FResult: extended;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(AFormula:string);
  public
    property Result : extended read FResult;
  end;  

implementation

{ TCalculator }

constructor TCalculator.Create;
begin
  inherited;

  FParsingContext := TParsingContext.Create;
  FInfix2Postfix := TInfix2Postfix.Create;
  FPolishParser := TPolishParser.Create
end;

destructor TCalculator.Destroy;
begin
  FreeAndNil(FParsingContext);
  FreeAndNil(FInfix2Postfix);
  FreeAndNil(FPolishParser);

  inherited;
end;

procedure TCalculator.Execute(AFormula:string);
var
  Token : TToken;
begin
  FParsingContext.Scan(AFormula);

  FInfix2Postfix.Clear;
  Token := FParsingContext.SkipToken;
  while Token <> nil  do begin
    FInfix2Postfix.Push(Token);
    Token := FParsingContext.SkipToken;
  end;
  FInfix2Postfix.EndOfLine;

  FPolishParser.Execute(FInfix2Postfix.Polish);
  FResult := FPolishParser.Result;
end;

end.
