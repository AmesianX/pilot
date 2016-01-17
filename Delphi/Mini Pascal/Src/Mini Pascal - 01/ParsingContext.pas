unit ParsingContext;

interface

uses
  ParserUtils, Scanner, TokenList,
  Classes, SysUtils;

type
  TParsingContext = class
  private
    FIndex : integer;
    FScanner : TScanner;
    FTokenList : TTokenList;
    procedure do_Error(AErrorMsg:string);
    procedure on_Token(Sender:TObject; AIndex:integer; ATokenType:TScannerTokenType; AText:string);
  private
    function GetEOF: boolean;
    function GetTokenCount: integer;
    function GetTokens(AIndex: integer): TTokenInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Scan(ASource:string);

    function SkipToken:TTokenInfo; overload;
    function SkipToken(AToken:string):TTokenInfo; overload;
    function SkipToken(ATokenType:TScannerTokenType):TTokenInfo; overload;

    function CurrentToken:TTokenInfo;

    property EOF : boolean read GetEOF;
    property Index : integer read FIndex;
    property TokenCount : integer read GetTokenCount;
    property Tokens[AIndex:integer] : TTokenInfo read GetTokens;
  end;

implementation

{ TParsingContext }

constructor TParsingContext.Create;
begin
  inherited;

  FTokenList := TTokenList.Create;

  FScanner := TScanner.Create(nil);
  FScanner.OnToken := on_Token;
end;

function TParsingContext.CurrentToken: TTokenInfo;
begin
  Result := FTokenList.Items[FIndex];
end;

destructor TParsingContext.Destroy;
begin
  FreeAndNil(FScanner);
  FreeAndNil(FTokenList);

  inherited;
end;

procedure TParsingContext.do_Error(AErrorMsg: string);
var
  Error : EParsingError;
begin
  Error := EParsingError.Create(AErrorMsg);
  Error.Index := FIndex;
  raise Error;
end;

function TParsingContext.GetEOF: boolean;
begin
  Result := FIndex >= FTokenList.Count; 
end;

function TParsingContext.GetTokenCount: integer;
begin
  Result := FTokenList.Count;
end;

function TParsingContext.GetTokens(AIndex: integer): TTokenInfo;
begin
  Result := FTokenList.Items[AIndex];
end;

procedure TParsingContext.on_Token(Sender: TObject; AIndex: integer;
  ATokenType: TScannerTokenType; AText: string);
begin
  if ATokenType <> ttWhiteSpace then FTokenList.Add(AIndex, ATokenType, AText);
end;

procedure TParsingContext.Scan(ASource: string);
begin
  FIndex := 0;
  FTokenList.Clear;
  FScanner.Execute(ASource);
end;

function TParsingContext.SkipToken:TTokenInfo;
begin
  // Todo : Null Object로 변경
  Result := nil;

  if EOF then Exit;

  Result := CurrentToken;
  Inc(FIndex);
end;

function TParsingContext.SkipToken(ATokenType: TScannerTokenType):TTokenInfo;
var
  Item : TTokenInfo;
begin
  // Todo : Null Object로 변경
  Result := nil;

  if EOF then Exit;

  Item := FTokenList.Items[FIndex];

  if Item.TokenType <> ATokenType then
    do_Error(Format('토큰(%s)을 찾을 수가 없습니다.', [Item.TokenName]));

  Result := CurrentToken;
  Inc(FIndex);
end;

function TParsingContext.SkipToken(AToken: string):TTokenInfo;
var
  Item : TTokenInfo;
begin
  // Todo : Null Object로 변경
  Result := nil;

  if EOF then Exit;

  Item := FTokenList.Items[FIndex];

  if not Item.IsSameToken(AToken) then
    do_Error(Format('토큰(%s)을 찾을 수가 없습니다.', [AToken]));

  Result := CurrentToken;
  Inc(FIndex);
end;

end.
