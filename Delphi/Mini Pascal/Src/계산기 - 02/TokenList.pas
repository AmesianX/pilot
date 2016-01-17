unit TokenList;

interface

uses
  Tokens,
  Classes, SysUtils;

type
  TTokenList = class
  private
    FList : TList;
  private
    function GetCount: integer;
    function GetItems(Index: integer): TToken;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AToken:TToken);
    procedure Remove(AToken:TToken);
    procedure Delete(Index:integer);

    property Count : integer read GetCount;
    property Items[Index:integer] : TToken read GetItems;
  end;  

implementation

{ TTokenList }

procedure TTokenList.Add(AToken: TToken);
begin
  FList.Add(AToken);
end;

procedure TTokenList.Clear;
begin
  FList.Clear;
end;

constructor TTokenList.Create;
begin
  inherited;

  FList := TList.Create;
end;

procedure TTokenList.Delete(Index: integer);
begin
  FList.Delete(Index);
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

function TTokenList.GetItems(Index: integer): TToken;
begin
  Result := Pointer(TToken(FList[Index]));
end;

procedure TTokenList.Remove(AToken: TToken);
begin
  FList.Remove(AToken);
end;

end.
