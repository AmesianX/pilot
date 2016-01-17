unit PascalParser;

interface

uses
  Classes, SysUtils;

const
  SpecialChars = '@#$^*()+-={}|:"<>?[]\;'',./';  

type
  TPascalParserTokenType = (
    ttNone, ttString, ttNumber, ttIdentifier, ttSpecialChars, ttAssignment,
    ttCompilerDirective
  );

  TTokenEvent = procedure (Sender:TObject; ATokenType:TPascalParserTokenType; AText:string) of object;

  TPascalParser = class;

  TState = class abstract (TComponent)
  private
  protected
    function get_Ch:char;
    procedure do_Error;
    procedure do_Parse(Ch:char); virtual; abstract;
  public
    function Parent:TPascalParser;
    procedure Transfer(Ch:char);
    procedure Parse(Ch:char);
    procedure ActionIn(AOld:TState); virtual;
    procedure ActionOut(ANew:TState); virtual;
  end;

  TStateNormal = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateString = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionIn(AOld:TState); override;
  end;

  TStateQuotation = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateBrace = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionIn(AOld:TState); override;
  end;

  TStateCompilerDirective = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionOut(ANew:TState); override;
  end;

  TStateBraceComment = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateSlash = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateSlashComment = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateParenthesis = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateParenthesisComment = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateAsteriskInComment = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateColon = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateAssignment = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateSpecialChar = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateNumber = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionIn(AOld:TState); override;
  end;

  TStateIdentifier = class abstract (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionIn(AOld:TState); override;
  end;

  TPascalParser = class (TComponent)
  private
    FIndex, FLength : integer;
    FSource : string;
    FBuffer : string;

    FNormal, FSpecialChar, FString, FNumber, FSlash,
    FSlashComment, FQuotation, FAsteriskInComment, FParenthesis,
    FParenthesisComment, FBrace, FBraceComment, FCompilerDirective, FColon,
    FAssignment, FIdentifier : TState;

    FState : TState;
    procedure SetState(const Value: TState);
    property State : TState read FState write SetState;
  private
    FOnToken: TTokenEvent;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    procedure Execute(ASource:string);
  published
    property Source : string read FSource;
    property CurrentIndex : integer read FIndex;
    property OnToken : TTokenEvent read FOnToken write FOnToken;
  end;

implementation

{ TPascalParser }

constructor TPascalParser.Create(AOwner: TComponent);
begin
  inherited;

  FNormal := TStateNormal.Create(Self);
  FString := TStateString.Create(Self);
  FQuotation := TStateQuotation.Create(Self);
  FBrace := TStateBrace.Create(Self);
  FCompilerDirective := TStateCompilerDirective.Create(Self);
  FBraceComment := TStateBraceComment.Create(Self);
  FSlash := TStateSlash.Create(Self);
  FSlashComment := TStateSlashComment.Create(Self);
  FParenthesis := TStateParenthesis.Create(Self);
  FParenthesisComment := TStateParenthesisComment.Create(Self);
  FAsteriskInComment := TStateAsteriskInComment.Create(Self);
  FColon := TStateColon.Create(Self);
  FAssignment := TStateAssignment.Create(Self);
  FSpecialChar := TStateSpecialChar.Create(Self);
  FNumber := TStateNumber.Create(Self);
  FIdentifier := TStateIdentifier.Create(Self);
end;

destructor TPascalParser.Destroy;
begin
  FNormal.Free;
  FString.Free;
  FQuotation.Free;
  FBrace.Free;
  FCompilerDirective.Free;
  FBraceComment.Free;
  FSlash.Free;
  FSlashComment.Free;
  FParenthesis.Free;
  FParenthesisComment.Free;
  FAsteriskInComment.Free;
  FColon.Free;
  FAssignment.Free;
  FSpecialChar.Free;
  FNumber.Free;
  FIdentifier.Free;

  inherited;
end;

procedure TPascalParser.Execute(ASource: string);
begin
  if ASource = '' then Exit;
  if not Assigned(FOnToken) then Exit;

  FSource := ASource;
  FBuffer := '';
  
  FIndex := 0;
  FLength := Length(FSource);

  FState := FNormal;
  repeat
    State.Parse(#0);
  until FIndex >= FLength;
end;

procedure TPascalParser.SetState(const Value: TState);
var
  Temp : TState;
begin
  if Value = FState then Exit;  

  Temp := FState;
  FState := Value;

  Temp.ActionOut(FState);
  FState.ActionIn(Temp);
end;

{ TState }

procedure TState.ActionIn(AOld: TState);
begin
end;

procedure TState.ActionOut(ANew: TState);
begin
end;

procedure TState.do_Error;
begin
  raise Exception.Create(Format('Parsing error index of (%d)', [Parent.FIndex]));
end;

function TState.get_Ch: char;
begin
  if Parent.FIndex >= Parent.FLength then begin
    Result := #0;
    Exit;
  end;

  Inc(Parent.FIndex);
  Result := Parent.FSource[Parent.FIndex];
end;

function TState.Parent: TPascalParser;
begin
  Result := Owner as TPascalParser;
end;

procedure TState.Parse(Ch: char);
begin
  if Ch <> #0 then do_Parse(Ch)
  else do_Parse(get_Ch);
end;

procedure TState.Transfer(Ch: char);
begin
  Parent.State := Self;
  Parse(Ch);
end;

{ TStateNormal }

procedure TStateNormal.do_Parse(Ch: char);
begin
  case Ch of
       '''' : Parent.FString.Transfer(#0);
       '{' : Parent.FBrace.Transfer(#0);
       '/' : Parent.FSlash.Transfer(#0);
       '(' : Parent.FParenthesis.Transfer(#0);
       ':' : Parent.FColon.Transfer(#0);
       '0'..'9' : Parent.FNumber.Transfer(Ch);
       #0..#13, ' ' : ; // WhiteSpace
       else begin
         if Pos(Ch, SpecialChars) > 0 then Parent.FSpecialChar.Transfer(Ch)
         else Parent.FIdentifier.Transfer(Ch);
       end;
  end;
end;

{ TStateString }

procedure TStateString.ActionIn(AOld: TState);
begin
  if AOld = Parent.FQuotation then Parent.FBuffer := Parent.FBuffer + ''''''
  else Parent.FBuffer := '';
end;

procedure TStateString.do_Parse(Ch: char);
begin
  case Ch of
       '''' : Parent.FQuotation.Transfer(#0);
       #13, #10 : do_Error;
       else
         Parent.FBuffer := Parent.FBuffer + Ch;
  end;
end;

{ TStateQuotation }

procedure TStateQuotation.do_Parse(Ch: char);
begin
  case Ch of
       '''' : Parent.FString.Transfer(#0);
       else begin
         Parent.FBuffer := '''' + Parent.FBuffer + '''';
         Parent.FOnToken(Parent, ttString, Parent.FBuffer);
         Parent.FBuffer := '';

         Parent.FNormal.Transfer(#0);
       end;
  end;
end;

{ TStateBrace }

procedure TStateBrace.ActionIn(AOld: TState);
begin
  Parent.FBuffer := '';
end;

procedure TStateBrace.do_Parse(Ch: char);
begin
  case Ch of
       '$' : Parent.FCompilerDirective.Transfer(#0);
       else
         Parent.FBraceComment.Transfer(#0);
  end;
end;

{ TStateCompilerDirective }

procedure TStateCompilerDirective.ActionOut(ANew: TState);
begin
  Parent.FBuffer := '{$' + Parent.FBuffer + '}';
  Parent.FOnToken(Parent, ttCompilerDirective, Parent.FBuffer);
  Parent.FBuffer := '';
end;

procedure TStateCompilerDirective.do_Parse(Ch: char);
begin
  case Ch of
       '}' : Parent.FNormal.Transfer(#0);
       else
         Parent.FBuffer := Parent.FBuffer + Ch;
  end;
end;

{ TStateBraceComment }

procedure TStateBraceComment.do_Parse(Ch: char);
begin
  case Ch of
       '}' : Parent.FNormal.Transfer(#0);
       else ;
  end;
end;

{ TStateSlash }

procedure TStateSlash.do_Parse(Ch: char);
begin
  case Ch of
       '/' : Parent.FSlashComment.Transfer(#0);
       else begin
         Parent.FOnToken(Parent, ttSpecialChars, '/');
         Parent.FNormal.Transfer(Ch);
       end;
  end;
end;

{ TStateSlashComment }

procedure TStateSlashComment.do_Parse(Ch: char);
begin
  case Ch of
       #10, #13 : Parent.FNormal.Transfer(#0);
       else ;
  end;
end;

{ TStateParenthesis }

procedure TStateParenthesis.do_Parse(Ch: char);
begin
  case Ch of
       '*' : Parent.FParenthesisComment.Transfer(#0);
       else begin
         Parent.FOnToken(Parent, ttSpecialChars, '(');
         Parent.FNormal.Transfer(Ch);
       end;
  end;
end;

{ TStateParenthesisComment }

procedure TStateParenthesisComment.do_Parse(Ch: char);
begin
  case Ch of
       '*' : Parent.FAsteriskInComment.Transfer(#0);
       else ;
  end;
end;

{ TStateAsteriskInComment }

procedure TStateAsteriskInComment.do_Parse(Ch: char);
begin
  case Ch of
       ')' : Parent.FNormal.Transfer(#0);
       '*' : ;
       else Parent.FParenthesisComment.Transfer(#0);
  end;
end;

{ TStateColon }

procedure TStateColon.do_Parse(Ch: char);
begin
  case Ch of
       '=' : Parent.FAssignment.Transfer(#0);
       else begin
         Parent.FOnToken(Parent, ttSpecialChars, ':');
         Parent.FNormal.Transfer(Ch);
       end;
  end;
end;

{ TStateAssignment }

procedure TStateAssignment.do_Parse(Ch: char);
begin
  Parent.FOnToken(Parent, ttAssignment, ':=');
  Parent.FNormal.Transfer(Ch);
end;

{ TStateSpecialChar }

procedure TStateSpecialChar.do_Parse(Ch: char);
begin
  Parent.FOnToken(Parent, ttSpecialChars, Ch);
  Parent.FNormal.Transfer(#0);
end;

{ TStateNumber }

procedure TStateNumber.ActionIn(AOld: TState);
begin
  Parent.FBuffer := '';
end;

procedure TStateNumber.do_Parse(Ch: char);
begin
  if not (Ch in ['0'..'9']) then begin
    Parent.FOnToken(Parent, ttNumber, Parent.FBuffer);
    Parent.FBuffer := '';

    Parent.FNormal.Transfer(Ch);
  end else
    Parent.FBuffer := Parent.FBuffer + Ch;
end;

{ TStateIdentifier }

procedure TStateIdentifier.ActionIn(AOld: TState);
begin
  Parent.FBuffer := '';
end;

procedure TStateIdentifier.do_Parse(Ch: char);
var
  bCondition : boolean;
begin
  bCondition := (Ch in [#0..#13, ' ']) or (Pos(Ch, SpecialChars) > 0);

  if not bCondition then
    Parent.FBuffer := Parent.FBuffer + Ch
  else begin
    Parent.FOnToken(Parent, ttIdentifier, Parent.FBuffer);
    Parent.FBuffer := '';

    Parent.FNormal.Transfer(Ch);
  end;
end;

end.
