unit ParsingNode;

interface

uses
  ParserUtils, ParsingContext,
  Windows, Classes, SysUtils;

type
  TParsingNode = class abstract
  protected
    FCommands : TList;
    FContext : TParsingContext;
    procedure do_VisitNodesAndCompile(AOutPut:TStringList); virtual;
  public
    Debug : string;

    constructor Create(ACommands:TList; AContext:TParsingContext); reintroduce; virtual;

    function Clone:TParsingNode; virtual;
    function IsMyTurn:boolean; virtual;
    procedure Parse; virtual;
    procedure Compile(AOutPut:TStringList); virtual;
  end;

  TNonTerminalNode = class abstract (TParsingNode)
  private
    procedure do_FreeNodes;
  protected
    FNodes : TList;
    procedure do_VisitNodesAndCompile(AOutPut:TStringList); override;
  public
    constructor Create(ACommands:TList; AContext:TParsingContext); override;
    destructor Destroy; override;

    procedure AddNode(ANode:TParsingNode);
    function FindAppropriateCommandAndParse:boolean;
  end;

  TTerminalNode = class abstract (TParsingNode)
  private
  public
  end;

implementation

{ TParsingNode }

function TParsingNode.Clone:TParsingNode;
begin
  Result := nil;
end;

procedure TParsingNode.Compile(AOutPut: TStringList);
begin
  //
end;

constructor TParsingNode.Create(ACommands:TList; AContext:TParsingContext);
begin
  inherited Create;

  FCommands := ACommands;
  FContext := AContext;
end;

procedure TParsingNode.do_VisitNodesAndCompile(AOutPut: TStringList);
begin
  //
end;

function TParsingNode.IsMyTurn: boolean;
begin
  Result := false;
end;

procedure TParsingNode.Parse;
begin
  //
end;

{ TNonTerminalNode }

procedure TNonTerminalNode.AddNode(ANode: TParsingNode);
begin
  {$IFDEF DEBUG}
    OutputDebugString(PChar(Format('%s.AddNode: %d, %s', [ClassName, FContext.Index, ANode.ClassName])));
  {$ENDIF}

  FNodes.Add(ANode);
  ANode.Parse;
end;

constructor TNonTerminalNode.Create(ACommands:TList; AContext: TParsingContext);
begin
  inherited;

  FNodes := TList.Create;
end;

destructor TNonTerminalNode.Destroy;
begin
  do_FreeNodes;
  FreeAndNil(FNodes);

  inherited;
end;

procedure TNonTerminalNode.do_FreeNodes;
var
  Loop : Integer;
begin
  for Loop := 0 to FNodes.Count-1 do TObject(FNodes[Loop]).Free;
end;

procedure TNonTerminalNode.do_VisitNodesAndCompile(AOutPut: TStringList);
var
  Loop : Integer;
  Node : TParsingNode;
begin
  for Loop := 0 to FNodes.Count-1 do begin
    Node := Pointer(FNodes[Loop]);
    Node.Compile(AOutPut);
  end;
end;

function TNonTerminalNode.FindAppropriateCommandAndParse:boolean;
var
  Loop : Integer;
  Node : TParsingNode;
begin
  Result := false;

  // 본 노드에서 가능한 모든 Command들을 검사하고,
  // 해당 사항이 있으면 해당 Command의 노드 객체를 생성한 후 파싱을 한다.
  for Loop := 0 to FCommands.Count-1 do begin
    Node := Pointer(FCommands[Loop]);
    if Node.IsMyTurn then begin
      AddNode(Node.Clone);
      Result := true;
      Break;
    end;
  end;
end;

end.
