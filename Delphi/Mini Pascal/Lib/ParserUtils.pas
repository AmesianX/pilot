unit ParserUtils;

interface

uses
  Classes, SysUtils, TypInfo;
  
const
  SpecialChars = '@#$^*()+-={}|:"<>?[]\;'',./';  

type
  TScannerTokenType = (
    ttWhiteSpace, ttString, ttNumber, ttIdentifier, ttSpecialChars, ttAssignment,
    ttCompilerDirective
  );

  EScanError = class (Exception)
  private
    FIndex, FSize : integer;
  public
    constructor Create(AMsg:string; AIndex,ASize:integer); reintroduce;

    property Index : integer read FIndex;
    property Size : integer read FSize;
  end;

  EParsingError = class (Exception)
  private
  private
    FIndex, FSize : integer;
  public
    constructor Create(AMsg:string; AIndex,ASize:integer); reintroduce;

    property Index : integer read FIndex;
    property Size : integer read FSize;
  end;

  TTokenInfo = class
  private
    function GetTokenName: string;
  public
    Index : integer;
    TokenType : TScannerTokenType;
    Text : string;

    function IsSameToken(AText:string):boolean;

    property TokenName : string read GetTokenName;
  end;  

implementation

{ EScanError }

constructor EScanError.Create(AMsg: string; AIndex, ASize: integer);
begin
  inherited Create(AMsg);

  FIndex := AIndex;
  FSize := ASize;
end;

{ EParsingError }

constructor EParsingError.Create(AMsg: string; AIndex, ASize: integer);
begin
  inherited Create(AMsg);

  FIndex := AIndex;
  FSize := ASize;
end;

{ TTokenInfo }

function TTokenInfo.GetTokenName: string;
begin
  Result := GetEnumName(TypeInfo(TScannerTokenType), Integer(TokenType));
end;

function TTokenInfo.IsSameToken(AText: string): boolean;
begin
  Result := LowerCase(Text) = LowerCase(AText);
end;

end.
