unit Slurpy;

interface

uses
  Classes, SysUtils;

type
  TSlurpy = class;

  TState = class abstract
  private
    FSlurpy : TSlurpy;
  public
    constructor Create(ASlurpy:TSlurpy); reintroduce;

    procedure SetState(AState:TState);

    function Next:AnsiChar;
    function Peek(ACount:integer):AnsiChar;

    procedure Execute; virtual; abstract;
  end;

  TSlurpy = class
  private
    FIsFinished : boolean;
  private
    FState : TState;

    FStateSlimpBase : TState;
    FStateSlimpA : TState;
    FStateSlimpASlump : TState;
    FStateSlimpAB : TState;
    FStateSlimpABSlimp : TState;

    FStateSlumpBase : TState;
    FStateSlumpD : TState;
    FStateSlumpDF : TState;
    FStateSlumpE : TState;
    FStateSlumpEF : TState;
  private
    FSource: AnsiString;
    FIndex : integer;
    procedure SetSource(const Value: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;

    function Next:AnsiChar;
    function Peek(ACount:integer):AnsiChar;

    function IsSlurpy:boolean;
    function IsSlimp:boolean;
    function IsSlump:boolean;
    function IsF:boolean;

    property Source : AnsiString read FSource write SetSource;
  end;

implementation

type
  TStateSlimpBase = class (TState)
  private
  public
    procedure Execute; override;
  end;

  TStateSlimpA = class (TState)
  private
  public
    procedure Execute; override;
  end;

  TStateSlimpAB = class (TState)
  private
  public
    procedure Execute; override;
  end;

  TStateSlimpABSlimp = class (TState)
  private
  public
    procedure Execute; override;
  end;

  TStateSlimpASlump = class (TState)
  private
  public
    procedure Execute; override;
  end;

  TStateSlumpBase = class (TState)
  private
  public
    procedure Execute; override;
  end;

  TStateSlumpD = class (TState)
  private
  public
    procedure Execute; override;
  end;

  TStateSlumpDF = class (TState)
  private
  public
    procedure Execute; override;
  end;

  TStateSlumpE = class (TState)
  private
  public
    procedure Execute; override;
  end;

  TStateSlumpEF = class (TState)
  private
  public
    procedure Execute; override;
  end;

{ TState }

constructor TState.Create(ASlurpy:TSlurpy);
begin
  inherited Create;

  FSlurpy := ASlurpy;
end;

function TState.Next: AnsiChar;
begin
  Result := FSlurpy.Next;
end;

function TState.Peek(ACount: integer): AnsiChar;
begin
  Result := FSlurpy.Peek(ACount);
end;

procedure TState.SetState(AState: TState);
begin
  FSlurpy.FIsFinished := false;
  FSlurpy.FState := AState;
end;

{ TSlurpy }

constructor TSlurpy.Create;
begin
  inherited;

  FStateSlimpBase := TStateSlimpBase.Create(Self);
  FStateSlimpA := TStateSlimpA.Create(Self);
  FStateSlimpAB := TStateSlimpAB.Create(Self);
  FStateSlimpABSlimp := TStateSlimpABSlimp.Create(Self);
  FStateSlimpASlump := TStateSlimpASlump.Create(Self);

  FStateSlumpBase := TStateSlumpBase.Create(Self);
  FStateSlumpD := TStateSlumpD.Create(Self);
  FStateSlumpDF := TStateSlumpDF.Create(Self);
  FStateSlumpE := TStateSlumpE.Create(Self);
  FStateSlumpEF := TStateSlumpEF.Create(Self);
end;

destructor TSlurpy.Destroy;
begin
  FreeAndNil(FStateSlimpBase);
  FreeAndNil(FStateSlimpA);
  FreeAndNil(FStateSlimpAB);
  FreeAndNil(FStateSlimpABSlimp);
  FreeAndNil(FStateSlimpASlump);

  FreeAndNil(FStateSlumpBase);
  FreeAndNil(FStateSlumpD);
  FreeAndNil(FStateSlumpDF);
  FreeAndNil(FStateSlumpE);
  FreeAndNil(FStateSlumpEF);

  inherited;
end;

function TSlurpy.IsF: boolean;
begin
  Result := false;
  if Peek(1) <> 'F' then Exit;

  Result := true;
  while Peek(1) = 'F' do Next;
end;

function TSlurpy.IsSlurpy: boolean;
begin
  try
    Result := IsSlimp and IsSlump;
  except
    Result := false;
  end;
end;

function TSlurpy.IsSlimp: boolean;
begin
  try
    FState := FStateSlimpBase;
    FIsFinished := false;
    repeat
      FState.Execute;
    until FIsFinished;

    Result := true;
  except
    Result := false;
  end;
end;

function TSlurpy.IsSlump: boolean;
begin
  try
    FState := FStateSlumpBase;
    FIsFinished := false;
    repeat
      FState.Execute;
    until FIsFinished;

    Result := true;
  except
    Result := false;
  end;
end;

function GetFirstChar(AStr:AnsiString):AnsiChar;
begin
  if AStr = '' then Result := #0
  else Result := AStr[1];
end;

function TSlurpy.Next: AnsiChar;
begin
  FIndex := FIndex + 1;
  Result := GetFirstChar(Copy(FSource, FIndex, 1));
end;

function TSlurpy.Peek(ACount: integer): AnsiChar;
begin
  Result := GetFirstChar(Copy(FSource, FIndex + ACount, 1));
end;

procedure TSlurpy.SetSource(const Value: AnsiString);
begin
  FIndex := 0;
  FSource := Value;
end;

{ TStateSlimpBase }

procedure TStateSlimpBase.Execute;
begin
  case Next of
    'A': begin
      SetState(FSlurpy.FStateSlimpA);
    end;

    else raise Exception.Create('Error in ' + ClassName);
  end;
end;

{ TStateSlimpA }

procedure TStateSlimpA.Execute;
begin
  case Peek(1) of
    'H': begin
      Next;
      FSlurpy.FIsFinished := true;
    end;

    'B': begin
      Next;
      SetState(FSlurpy.FStateSlimpAB);
    end;

    else begin
      if FSlurpy.IsSlump then FSlurpy.FState := FSlurpy.FStateSlimpASlump
      else raise Exception.Create('Error in ' + ClassName);
    end;
  end;
end;

{ TStateSlimpAB }

procedure TStateSlimpAB.Execute;
begin
  if FSlurpy.IsSlimp then SetState(FSlurpy.FStateSlimpABSlimp)
  else raise Exception.Create('Error in ' + ClassName);
end;

{ TStateSlimpABSlimp }

procedure TStateSlimpABSlimp.Execute;
begin
  case Next of
    'C': begin
      FSlurpy.FIsFinished := true;
    end;

    else raise Exception.Create('Error in ' + ClassName);
  end;
end;

{ TStateSlimpASlimp }

procedure TStateSlimpASlump.Execute;
begin
  case Next of
    'C': begin
      FSlurpy.FIsFinished := true;
    end;

    else raise Exception.Create('Error in ' + ClassName);
  end;
end;

{ TStateSlumpBase }

procedure TStateSlumpBase.Execute;
begin
  case Next of
    'D': begin
      SetState(FSlurpy.FStateSlumpD);
    end;

    'E': begin
      SetState(FSlurpy.FStateSlumpE);
    end;

    else raise Exception.Create('Error in ' + ClassName);
  end;
end;

{ TStateSlumpD }

procedure TStateSlumpD.Execute;
begin
  if FSlurpy.IsF then SetState(FSlurpy.FStateSlumpDF)
  else raise Exception.Create('Error in ' + ClassName);
end;

{ TStateSlumpDF }

procedure TStateSlumpDF.Execute;
begin
  if Peek(1) = 'G' then begin
    Next;
    FSlurpy.FIsFinished := true;
  end else begin
    if FSlurpy.IsSlump then FSlurpy.FIsFinished := true
    else raise Exception.Create('Error in ' + ClassName);
  end;
end;

{ TStateSlumpE }

procedure TStateSlumpE.Execute;
begin
  if FSlurpy.IsF then SetState(FSlurpy.FStateSlumpEF)
  else raise Exception.Create('Error in ' + ClassName);
end;

{ TStateSlumpEF }

procedure TStateSlumpEF.Execute;
begin
  if Peek(1) = 'G' then begin
    Next;
    FSlurpy.FIsFinished := true;
  end else begin
    if FSlurpy.IsSlump then FSlurpy.FIsFinished := true
    else raise Exception.Create('Error in ' + ClassName);
  end;
end;

end.
