unit Scanner;

interface

uses
  ParserUtils,
  Classes, SysUtils;

type
  TTokenEvent = procedure (Sender:TObject; AIndex:integer; ATokenType:TScannerTokenType; AText:string) of object;

  TScanner = class;

  TState = class abstract (TComponent)
  private
    FIndex : integer;
  protected
    function get_Ch:char;
    procedure do_Error(AIndex,ASize:integer; AErrorMsg:string);
    procedure do_Parse(Ch:char); virtual; abstract;
  public
    function Parent:TScanner;
    procedure Transfer(Ch:char);
    procedure Parse(Ch:char);
    procedure ActionIn(AOld:TState); virtual;
    procedure ActionOut(ANew:TState); virtual;
  end;

  TStateNormal = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateWhiteSpace = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateString = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionIn(AOld:TState); override;
  end;

  TStateQuotation = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateBrace = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionIn(AOld:TState); override;
  end;

  TStateCompilerDirective = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionOut(ANew:TState); override;
  end;

  TStateBraceComment = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateSlash = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateSlashComment = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateParenthesis = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateParenthesisComment = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateAsteriskInComment = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateColon = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateAssignment = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateSpecialChar = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateNumber = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionIn(AOld:TState); override;
  end;

  TStateExponential = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateExponentialPlus = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateExponentialMinus = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateExponentialNumber = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateNumberPoint = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateNumberPointNumber = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
  end;

  TStateIdentifier = class (TState)
  private
  protected
    procedure do_Parse(Ch:char); override;
  public
    procedure ActionIn(AOld:TState); override;
  end;

  TScanner = class (TComponent)
  private
    FIndex, FLength : integer;
    FSource : string;
    FBuffer : string;

    FNormal, FWhiteSpace, FSpecialChar, FString,
    FNumber, FNumberPoint, FNumberPointNumber,
    FExponential, FExponentialPlus, FExponentialMinus, FExponentialNumber,
    FSlash,
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

{ TScanner }

constructor TScanner.Create(AOwner: TComponent);
begin
  inherited;

  FNormal := TStateNormal.Create(Self);
  FWhiteSpace := TStateWhiteSpace.Create(Self);
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
  FNumberPoint := TStateNumberPoint.Create(Self);
  FNumberPointNumber := TStateNumberPointNumber.Create(Self);

  FExponential := TStateExponential.Create(Self);
  FExponentialPlus := TStateExponentialPlus.Create(Self);
  FExponentialMinus := TStateExponentialMinus.Create(Self);
  FExponentialNumber := TStateExponentialNumber.Create(Self);

  FIdentifier := TStateIdentifier.Create(Self);
end;

destructor TScanner.Destroy;
begin
  FNormal.Free;
  FWhiteSpace.Free;
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
  FNumberPoint.Free;
  FNumberPointNumber.Free;
  FExponential.Free;
  FExponentialPlus.Free;
  FExponentialMinus.Free;
  FExponentialNumber.Free;
  FIdentifier.Free;

  inherited;
end;

procedure TScanner.Execute(ASource: string);
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

  State.Parse(#0);
end;

procedure TScanner.SetState(const Value: TState);
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

procedure TState.do_Error(AIndex,ASize:integer; AErrorMsg:string);
begin
  raise EScanError.Create(AErrorMsg, AIndex, ASize);
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

function TState.Parent: TScanner;
begin
  Result := Owner as TScanner;
end;

procedure TState.Parse(Ch: char);
begin
  if Ch <> #0 then do_Parse(Ch)
  else do_Parse(get_Ch);
end;

procedure TState.Transfer(Ch: char);
begin
  FIndex := Parent.FIndex;
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
       #0..#13, ' ' : Parent.FWhiteSpace.Transfer(Ch);
       else begin
         if Pos(Ch, SpecialChars) > 0 then Parent.FSpecialChar.Transfer(Ch)
         else Parent.FIdentifier.Transfer(Ch);
       end;
  end;
end;

{ TStateWhiteSpace }

procedure TStateWhiteSpace.do_Parse(Ch: char);
begin
  Parent.FOnToken(Parent, FIndex, ttWhiteSpace, Ch);
  Parent.State := Parent.FNormal;
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
       #0, #13, #10 : do_Error(FIndex, Length(Parent.FBuffer)+1, '문자열이 닫혀있지 않습니다.');
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
         Parent.FOnToken(Parent, FIndex, ttString, Parent.FBuffer);
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
  Parent.FOnToken(Parent, FIndex, ttCompilerDirective, Parent.FBuffer);
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
         Parent.FOnToken(Parent, FIndex, ttSpecialChars, '/');
         Parent.FNormal.Transfer(Ch);
       end;
  end;
end;

{ TStateSlashComment }

procedure TStateSlashComment.do_Parse(Ch: char);
begin
  case Ch of
       #10, #13 : Parent.FNormal.Transfer(Ch);
       else ;
  end;
end;

{ TStateParenthesis }

procedure TStateParenthesis.do_Parse(Ch: char);
begin
  case Ch of
       '*' : Parent.FParenthesisComment.Transfer(#0);
       else begin
         Parent.FOnToken(Parent, FIndex, ttSpecialChars, '(');
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
         Parent.FOnToken(Parent, FIndex, ttSpecialChars, ':');
         Parent.FNormal.Transfer(Ch);
       end;
  end;
end;

{ TStateAssignment }

procedure TStateAssignment.do_Parse(Ch: char);
begin
  Parent.FOnToken(Parent, FIndex, ttAssignment, ':=');
  Parent.FNormal.Transfer(Ch);
end;

{ TStateSpecialChar }

procedure TStateSpecialChar.do_Parse(Ch: char);
begin
  Parent.FOnToken(Parent, FIndex, ttSpecialChars, Ch);
  Parent.FNormal.Transfer(#0);
end;

{ TStateNumber }

procedure TStateNumber.ActionIn(AOld: TState);
begin
  Parent.FBuffer := '';
end;

procedure TStateNumber.do_Parse(Ch: char);
begin
  case Ch of
       '0'..'9': Parent.FBuffer := Parent.FBuffer + Ch;

       '.': begin
         Parent.FBuffer := Parent.FBuffer + Ch;
         Parent.FNumberPoint.Transfer(#0);
       end;

       'e', 'E': begin
         Parent.FBuffer := Parent.FBuffer + Ch;
         Parent.FExponential.Transfer(#0);
       end;

       else begin
         Parent.FOnToken(Parent, FIndex, ttNumber, Parent.FBuffer);
         Parent.FBuffer := '';

         Parent.FNormal.Transfer(Ch);
       end;
  end;
end;

{ TStateExponential }

procedure TStateExponential.do_Parse(Ch: char);
begin
  case Ch of
       '0'..'9': Parent.FExponentialNumber.Transfer(Ch);

       '+': begin
         Parent.FBuffer := Parent.FBuffer + Ch;
         Parent.FExponentialPlus.Transfer(#0);
       end;

       '-': begin
         Parent.FBuffer := Parent.FBuffer + Ch;
         Parent.FExponentialMinus.Transfer(#0);
       end

       else do_Error(FIndex, Length(Parent.FBuffer), '지수표현식이 불완전 합니다.');
  end;
end;

{ TStateExponentialPlus }

procedure TStateExponentialPlus.do_Parse(Ch: char);
begin
  case Ch of
       '0'..'9': Parent.FExponentialNumber.Transfer(Ch);
       else do_Error(FIndex, Length(Parent.FBuffer), '지수표현식이 불완전 합니다.');
  end;
end;

{ TStateExponentialMinus }

procedure TStateExponentialMinus.do_Parse(Ch: char);
begin
  case Ch of
       '0'..'9': Parent.FExponentialNumber.Transfer(Ch);
       else do_Error(FIndex, Length(Parent.FBuffer), '지수표현식이 불완전 합니다.');
  end;
end;

{ TStateExponentialNumber }

procedure TStateExponentialNumber.do_Parse(Ch: char);
begin
  case Ch of
       '0'..'9': Parent.FBuffer := Parent.FBuffer + Ch;

       else begin
         Parent.FOnToken(Parent, FIndex, ttNumber, Parent.FBuffer);
         Parent.FBuffer := '';

         Parent.FNormal.Transfer(Ch);
       end;
  end;
end;

{ TStateNumberPoint }

procedure TStateNumberPoint.do_Parse(Ch: char);
begin
  case Ch of
       '0'..'9': Parent.FNumberPointNumber.Transfer(Ch);
       else do_Error(FIndex, Length(Parent.FBuffer), '소숫점 이후에 숫자가 와야 합니다.');
  end;
end;

{ TStateNumberPointNumber }

procedure TStateNumberPointNumber.do_Parse(Ch: char);
begin
  case Ch of
       '0'..'9': Parent.FBuffer := Parent.FBuffer + Ch;

       'e', 'E': begin
         Parent.FBuffer := Parent.FBuffer + Ch;
         Parent.FExponential.Transfer(#0);
       end;

       '.': do_Error(FIndex, Length(Parent.FBuffer), '소숫점이 반복되고 있습니다.');

       else begin
         Parent.FOnToken(Parent, FIndex, ttNumber, Parent.FBuffer);
         Parent.FBuffer := '';

         Parent.FNormal.Transfer(Ch);
       end;
  end;
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
    Parent.FOnToken(Parent, FIndex, ttIdentifier, Parent.FBuffer);
    Parent.FBuffer := '';

    Parent.FNormal.Transfer(Ch);
  end;
end;

end.
