unit TokenList;

interface

uses
  ParserUtils,
  Classes, SysUtils;

type
  TTokenList = class
  private
    FList : TList;
  private
    function GetItems(AIndex: integer): TTokenInfo;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AIndex:integer; ATokenType:TScannerTokenType; AText:string);
    procedure Remove(ATokenInfo:TTokenInfo);
    procedure Delete(AIndex:integer);

    property Count : integer read GetCount;
    property Items[AIndex:integer] : TTokenInfo read GetItems;
  end;

implementation

{ TTokenList }

procedure TTokenList.Add(AIndex:integer; ATokenType:TScannerTokenType; AText:string);
var
  Item : TTokenInfo;
begin
  Item := TTokenInfo.Create;
  Item.Index := AIndex;
  Item.TokenType := ATokenType;
  Item.Text := AText;

  FList.Add(Item);
end;

procedure TTokenList.Clear;
var
  Loop: Integer;
begin
  for Loop := 0 to FList.Count-1 do TObject(FList[Loop]).Free;
  FList.Clear;
end;

constructor TTokenList.Create;
begin
  inherited;

  FList := TList.Create;
end;

procedure TTokenList.Delete(AIndex: integer);
begin
  TObject(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

destructor TTokenList.Destroy;
begin
  FreeAndNil(FList);

  inherited;
end;

function TTokenList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TTokenList.GetItems(AIndex: integer): TTokenInfo;
begin
  Result := Pointer(FList[AIndex]);
end;

procedure TTokenList.Remove(ATokenInfo: TTokenInfo);
begin
  FList.Remove(ATokenInfo);
  ATokenInfo.Free;
end;

end.
