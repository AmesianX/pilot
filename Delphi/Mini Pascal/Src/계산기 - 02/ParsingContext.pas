unit ParsingContext;

interface

uses
  ParserUtils, Scanner, Tokens, TokenList, TokenBuilder,
  Classes, SysUtils;

type
  TParsingContext = class
  private
    FIndex : integer;
    FScanner : TScanner;
    FTokenBuilder : TTokenBuilder;
    FTokenList : TTokenList;
    procedure do_ClearTokenList;
    procedure do_Error(AIndex,ASize:integer; AErrorMsg:string);
    procedure on_Token(Sender:TObject; AIndex:integer; ATokenType:TScannerTokenType; AText:string);
  private
    function GetEOF: boolean;
    function GetTokenCount: integer;
    function GetTokens(AIndex: integer): TToken;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Scan(ASource:string);

    procedure Rewind;
    function SkipToken:TToken; overload;
    function SkipToken(AToken:string):TToken; overload;
    function SkipToken(ATokenType:TScannerTokenType):TToken; overload;

    function CurrentToken:TToken;

    property EOF : boolean read GetEOF;
    property Index : integer read FIndex;
    property TokenCount : integer read GetTokenCount;
    property Tokens[AIndex:integer] : TToken read GetTokens;
  end;

implementation

{ TParsingContext }

constructor TParsingContext.Create;
begin
  inherited;

  FTokenList := TTokenList.Create;

  FScanner := TScanner.Create(nil);
  FScanner.OnToken := on_Token;

  FTokenBuilder := TTokenBuilder.Create;
end;

function TParsingContext.CurrentToken: TToken;
begin
  Result := FTokenList.Items[FIndex];
end;

destructor TParsingContext.Destroy;
begin
  FreeAndNil(FTokenBuilder);
  FreeAndNil(FScanner);
  FreeAndNil(FTokenList);

  inherited;
end;

procedure TParsingContext.do_ClearTokenList;
var
  Loop: Integer;
begin
  for Loop := 0 to FTokenList.Count-1 do FTokenList.Items[Loop].Free;
  FTokenList.Clear;
end;

procedure TParsingContext.do_Error(AIndex,ASize:integer; AErrorMsg: string);
begin
  raise EParsingError.Create(AErrorMsg, AIndex, ASize);
end;

function TParsingContext.GetEOF: boolean;
begin
  Result := FIndex >= FTokenList.Count; 
end;

function TParsingContext.GetTokenCount: integer;
begin
  Result := FTokenList.Count;
end;

function TParsingContext.GetTokens(AIndex: integer): TToken;
begin
  Result := FTokenList.Items[AIndex];
end;

procedure TParsingContext.on_Token(Sender: TObject; AIndex: integer;
  ATokenType: TScannerTokenType; AText: string);
var
  Token : TToken;
begin
  Token := FTokenBuilder.Execute(AIndex, ATokenType, AText);
  if Token <> nil then FTokenList.Add(Token);
end;

procedure TParsingContext.Rewind;
begin
  FIndex := 0;
end;

procedure TParsingContext.Scan(ASource: string);
begin
  FIndex := 0;
  do_ClearTokenList;
  FScanner.Execute(ASource);
end;

function TParsingContext.SkipToken:TToken;
begin
  // Todo : Null Object로 변경
  Result := nil;

  if EOF then Exit;

  Result := CurrentToken;
  Inc(FIndex);
end;

function TParsingContext.SkipToken(ATokenType: TScannerTokenType):TToken;
var
  Item : TToken;
begin
  // Todo : Null Object로 변경
  Result := nil;

  if EOF then Exit;

  Item := FTokenList.Items[FIndex];

  if Item.TokenType <> ATokenType then
    do_Error(FIndex, Item.Size, Format('토큰(%s)을 찾을 수가 없습니다.', [Item.TokenName]));

  Result := CurrentToken;
  Inc(FIndex);
end;

function TParsingContext.SkipToken(AToken: string):TToken;
var
  Item : TToken;
begin
  // Todo : Null Object로 변경
  Result := nil;

  if EOF then Exit;

  Item := FTokenList.Items[FIndex];

  if not Item.IsSameToken(AToken) then
    do_Error(FIndex, Item.Size, Format('토큰(%s)을 찾을 수가 없습니다.', [AToken]));

  Result := CurrentToken;
  Inc(FIndex);
end;

end.
