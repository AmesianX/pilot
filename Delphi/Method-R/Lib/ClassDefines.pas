unit ClassDefines;

interface

uses
  ValueList, PascalParser, Disk, Strg,
  Classes, SysUtils;

type
  TClassDefine = class;
  
  TMemberDefine = class
  private
    FClassDefine : TClassDefine;
    FText : string;
    FTokens : TStringList;
    FParser : TPascalParser;
    procedure on_Token(Sender:TObject; ATokenType:TPascalParserTokenType; AText:string);
  public
    Header, Body : string;

    constructor Create(AClassDefine:TClassDefine); reintroduce;
    destructor Destroy; override;

    procedure Translate(AText:string); virtual;
  end;

  TMethodDefine = class (TMemberDefine)
  private
  public
    procedure Translate(AText:string); override;
  end;

  TEventTypeDefine = class (TMemberDefine)
  private
  public
    Name : string;
    Templet : string;
    procedure Translate(AText:string); override;
  end;

  TEventDefine = class (TMemberDefine)
  private
  public
    Name, MethodName : string;
    EventType : string;
    procedure Translate(AText:string); override;
  end;

  TClassDefine = class
  private
    procedure do_EventType(ALine:string);
    procedure do_Procedure(ALine:string);
    procedure do_Function(ALine:string);
    procedure do_Event(ALine:string);

    function get_Uses(AClassDefines:array of TClassDefine):string;
    function get_Type:string;
    function get_Private:string; overload;
    function get_EventHandlerInterface(AClassDefines:array of TClassDefine):string; 
    function get_EventHandlerImplementation(AClassDefines:array of TClassDefine):string;
    function get_Public:string; overload;
    function get_Public(AClassDefines:array of TClassDefine):string; overload;
    function get_Published:string;
    function get_Constructor(AClassDefines:array of TClassDefine):string;
    function get_SetEventHandler(AClassDefines:array of TClassDefine):string;
    function get_Destructor(AClassDefines:array of TClassDefine):string;
    function get_Implementation:string;
  private
    FName: string;
    FMethods : array of TMethodDefine;
    FEvents : array of TEventDefine;
    FEventTypes : array of TEventTypeDefine;
    FReferenceName: string;
    function GetEventTypes(Index: integer): TEventTypeDefine;
    function GetEvents(Index: integer): TEventDefine;
    function GetMethods(Index: integer): TMethodDefine;
    function GetEventCount: integer;
    function GetEventTypeCount: integer;
    function GetMethodCount: integer;
  public
    procedure Translate(AText:string);

    procedure SaveBaseClassToPath(APath:string; AClassDefines:array of TClassDefine);
    procedure SaveRootClassToPath(APath:string);

    property Name : string read FName;
    property ReferenceName : string read FReferenceName;

    property MethodCount : integer read GetMethodCount;
    property Methods[Index:integer] : TMethodDefine read GetMethods;

    property EventCount : integer read GetEventCount;
    property Events[Index:integer] : TEventDefine read GetEvents;

    property EventTypeCount : integer read GetEventTypeCount;
    property EventTypes[Index:integer] : TEventTypeDefine read GetEventTypes;
  end;

implementation

{ TClassDefine }

procedure TClassDefine.do_Event(ALine: string);
var
  iIndex : integer;
  EventDefine : TEventDefine;
begin
  EventDefine := TEventDefine.Create(Self);
  EventDefine.Translate(ALine);

  iIndex := Length(FEvents);
  SetLength(FEvents, iIndex + 1);

  FEvents[iIndex] := EventDefine;
end;

procedure TClassDefine.do_EventType(ALine: string);
var
  iIndex : integer;
  EventTypeDefine : TEventTypeDefine;
begin
  EventTypeDefine := TEventTypeDefine.Create(Self);
  EventTypeDefine.Translate(ALine);

  iIndex := Length(FEventTypes);
  SetLength(FEventTypes, iIndex + 1);

  FEventTypes[iIndex] := EventTypeDefine;
end;

procedure TClassDefine.do_Function(ALine: string);
var
  iIndex : integer;
  MethodDefine : TMethodDefine;
begin
  MethodDefine := TMethodDefine.Create(Self);
  MethodDefine.Header := 'function';
  MethodDefine.Translate(ALine);

  iIndex := Length(FMethods);
  SetLength(FMethods, iIndex + 1);

  FMethods[iIndex] := MethodDefine;
end;

procedure TClassDefine.do_Procedure(ALine: string);
var
  iIndex : integer;
  MethodDefine : TMethodDefine;
begin
  MethodDefine := TMethodDefine.Create(Self);
  MethodDefine.Header := 'procedure';
  MethodDefine.Translate(ALine);

  iIndex := Length(FMethods);
  SetLength(FMethods, iIndex + 1);

  FMethods[iIndex] := MethodDefine;
end;

function TClassDefine.GetEventTypeCount: integer;
begin
  Result := Length(FEventTypes);
end;

function TClassDefine.GetEventTypes(Index: integer): TEventTypeDefine;
begin
  Result := FEventTypes[Index];
end;

function TClassDefine.GetEventCount: integer;
begin
  Result := Length(FEvents);
end;

function TClassDefine.GetEvents(Index: integer): TEventDefine;
begin
  Result := FEvents[Index];
end;

function TClassDefine.GetMethodCount: integer;
begin
  Result := Length(FMethods);
end;

function TClassDefine.GetMethods(Index: integer): TMethodDefine;
begin
  Result := FMethods[Index];
end;

function TClassDefine.get_Constructor(
  AClassDefines: array of TClassDefine): string;
var
  Loop : Integer;
  ReferenceName, ClassName : string;
begin
  Result := '';
  if Length(AClassDefines) < 3 then Exit;

  for Loop := 2 to Length(AClassDefines)-1 do begin
    ClassName := AClassDefines[Loop].Name;
    ReferenceName := AClassDefines[Loop].ReferenceName;
    Result := Result + Format(#13#10'  %s := %s.Create(Self);', [ReferenceName, ClassName]);
  end;
end;

function TClassDefine.get_Destructor(
  AClassDefines: array of TClassDefine): string;
var
  Loop : Integer;
  ReferenceName, ClassName : string;
begin
  Result := '';
  if Length(AClassDefines) < 3 then Exit;

  for Loop := 2 to Length(AClassDefines)-1 do begin
    ClassName := AClassDefines[Loop].Name;
    ReferenceName := AClassDefines[Loop].ReferenceName;
    Result := Result + Format('  FreeAndNil(%s);'#13#10, [ReferenceName]);
  end;
end;

function TClassDefine.get_Implementation: string;
var
  Loop : Integer;
begin
  Result := '';
  for Loop := 0 to MethodCount - 1 do
    with Methods[Loop] do begin
      Result :=
        Result +
        Format(#13#10'%s %s.%s'#13#10, [Header, Name, Body]) +
        'begin'#13#10#13#10'end;'#13#10#13#10;
    end;
end;

function TClassDefine.get_Private: string;
var
  Loop : Integer;
begin
  Result := '';
  for Loop := 0 to EventCount - 1 do
    with Events[Loop] do
      Result := Result + Format(#13#10'    F%s : %s', [Name, EventType]);
end;

function GetEventTypeDefine(AEventTypeName:string; AClassDefines:array of TClassDefine): TEventTypeDefine;
var
  Loop1, Loop2: Integer;
begin
  Result := nil;
  for Loop1 := 0 to Length(AClassDefines)-1 do begin
    for Loop2 := 0 to AClassDefines[Loop1].EventTypeCount-1 do begin
      if AEventTypeName = AClassDefines[Loop1].EventTypes[Loop2].Name then begin
        Result := AClassDefines[Loop1].EventTypes[Loop2];
        Break;
      end;
    end;
  end;
end;

function TClassDefine.get_EventHandlerImplementation(
  AClassDefines: array of TClassDefine): string;
var
  Loop1, Loop2 : Integer;
  Line, MethodName : string;
  EventDefine : TEventDefine;
  EventTypeDefine : TEventTypeDefine;
begin
  Result := '';
  if Length(AClassDefines) < 3 then Exit;

  // Root 클래스들에게 Event가 있으면 핸들러를 작성한다.
  for Loop1 := 2 to Length(AClassDefines)-1 do begin
    for Loop2 := 0 to AClassDefines[Loop1].EventCount-1 do begin
      EventDefine := AClassDefines[Loop1].Events[Loop2];

      EventTypeDefine := GetEventTypeDefine(EventDefine.EventType, AClassDefines);
      if EventTypeDefine = nil then
        raise Exception.Create('TClassDefine.get_Private: ' + EventDefine.EventType);

      MethodName := Format('on_%s_%s', [AClassDefines[Loop1].ReferenceName, EventDefine.MethodName]);

      Line := EventTypeDefine.Templet;
      Line := StringReplace(Line, '@Name', Name + '.' + MethodName, []);

      Result := Result + #13#10 + Line + #13#10'begin'#13#10#13#10'end;'#13#10;
    end;
  end;
end;

function TClassDefine.get_EventHandlerInterface(AClassDefines: array of TClassDefine): string;
var
  Loop1, Loop2 : Integer;
  Line, MethodName : string;
  EventDefine : TEventDefine;
  EventTypeDefine : TEventTypeDefine;
begin
  Result := '';
  if Length(AClassDefines) < 3 then Exit;

  // Root 클래스들에게 Event가 있으면 핸들러를 작성한다.
  for Loop1 := 2 to Length(AClassDefines)-1 do begin
    for Loop2 := 0 to AClassDefines[Loop1].EventCount-1 do begin
      EventDefine := AClassDefines[Loop1].Events[Loop2];

      EventTypeDefine := GetEventTypeDefine(EventDefine.EventType, AClassDefines);
      if EventTypeDefine = nil then
        raise Exception.Create('TClassDefine.get_Private: ' + EventDefine.EventType);

      MethodName := Format('on_%s_%s', [AClassDefines[Loop1].ReferenceName, EventDefine.MethodName]);

      Line := EventTypeDefine.Templet;
      Line := StringReplace(Line, '@Name', MethodName, []);

      Result := Result + #13#10'    ' + Line;
    end;
  end;
end;

function TClassDefine.get_Public: string;
var
  Loop : Integer;
begin
  Result := '';
  for Loop := 0 to MethodCount - 1 do
    with Methods[Loop] do
      Result := Result + Format(#13#10'    %s %s', [Header, Body]);
end;

function TClassDefine.get_Public(AClassDefines: array of TClassDefine): string;
var
  Loop : Integer;
  ReferenceName, ClassName : string;
begin
  Result := '';

  for Loop := 2 to Length(AClassDefines)-1 do begin
    ClassName := AClassDefines[Loop].Name;
    ReferenceName := AClassDefines[Loop].ReferenceName;
    Result := Result + Format(#13#10'    %s : %s; ', [ReferenceName, ClassName]);
  end;

  if Length(AClassDefines) > 2 then Result := Result + #13#10;

  for Loop := 0 to MethodCount - 1 do
    with Methods[Loop] do
      Result := Result + Format(#13#10'    %s %s', [Header, Body]);
end;

function TClassDefine.get_Published: string;
var
  Loop : Integer;
begin
  if EventCount = 0 then begin
    Result := '';
    Exit;
  end;

  Result := #13#10'  published';
  for Loop := 0 to EventCount - 1 do
    with Events[Loop] do
      Result := Result + Format(#13#10'    %s %s', [Header, Body]);
end;

function TClassDefine.get_SetEventHandler(
  AClassDefines: array of TClassDefine): string;
var
  Loop1, Loop2 : Integer;
  Line, MethodName : string;
  EventDefine : TEventDefine;
  EventTypeDefine : TEventTypeDefine;
begin
  Result := '';
  if Length(AClassDefines) < 3 then Exit;

  Result := #13#10;
  for Loop1 := 2 to Length(AClassDefines)-1 do begin
    for Loop2 := 0 to AClassDefines[Loop1].EventCount-1 do begin
      EventDefine := AClassDefines[Loop1].Events[Loop2];

      EventTypeDefine := GetEventTypeDefine(EventDefine.EventType, AClassDefines);
      if EventTypeDefine = nil then
        raise Exception.Create('TClassDefine.get_Private: ' + EventDefine.EventType);

      MethodName := Format('on_%s_%s', [AClassDefines[Loop1].ReferenceName, EventDefine.MethodName]);

      Line := EventTypeDefine.Templet;
      Line := StringReplace(Line, '@Name', MethodName, []);

      Result := Result + Format(#13#10'  %s.%s := %s;', [AClassDefines[Loop1].Name, EventDefine.Name, MethodName]);
    end;
  end;
end;

function TClassDefine.get_Type: string;
var
  Loop : Integer;
begin
  if EventTypeCount = 0 then begin
    Result := '';
    Exit;
  end;

  Result := '';
  for Loop := 0 to EventTypeCount - 1 do
    with EventTypes[Loop] do
      Result := Result + Format(#13#10'  %s %s', [Header, Body]);

  Result := Result + #13#10;
end;

function TClassDefine.get_Uses(AClassDefines: array of TClassDefine): string;
var
  Loop : Integer;
  ReferenceName, ClassName : string;
begin
  if Length(AClassDefines) < 3 then begin
    Result := '';
    Exit;
  end;

  Result := '  ';
  for Loop := 2 to Length(AClassDefines)-1 do begin
    ClassName := AClassDefines[Loop].Name;
    ReferenceName := AClassDefines[Loop].ReferenceName;
    Result := Result + Format('%s, ', [ReferenceName]);
  end;

  Result := Result + #13#10;
end;

procedure TClassDefine.SaveBaseClassToPath(APath: string;
  AClassDefines: array of TClassDefine);
var
  Lines : TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(GetExecPath + 'Templet.txt');

    Lines.Text := StringReplace(Lines.Text, '//UnitName//', Copy(Name, 2, Length(Name)), []);
    Lines.Text := StringReplace(Lines.Text, '//Uses//', get_Uses(AClassDefines), []);
    Lines.Text := StringReplace(Lines.Text, '//ClassName//', Name, [rfReplaceAll]);

    Lines.Text := StringReplace(Lines.Text, '//Type//', get_Type, []);
    Lines.Text := StringReplace(Lines.Text, '//EventHandlerInterface//', get_EventHandlerInterface(AClassDefines), []);
    Lines.Text := StringReplace(Lines.Text, '//PropertyPrivate//', get_Private, []);
    Lines.Text := StringReplace(Lines.Text, '//Public//', get_Public(AClassDefines), []);
    Lines.Text := StringReplace(Lines.Text, '//Published//', get_Published, []);

    Lines.Text := StringReplace(Lines.Text, '//Constructor//', get_Constructor(AClassDefines), []);
    Lines.Text := StringReplace(Lines.Text, '//SetEventHandler//', get_SetEventHandler(AClassDefines), []);
    Lines.Text := StringReplace(Lines.Text, '//Destructor//', get_Destructor(AClassDefines), []);

    Lines.Text := StringReplace(Lines.Text, '//EventHandlerImplementation//', get_EventHandlerImplementation(AClassDefines), []);
    Lines.Text := StringReplace(Lines.Text, '//Implementation//', get_Implementation, []);

    Lines.SaveToFile(APath + Copy(Name, 2, Length(Name)) + '.pas');
  finally
    Lines.Free;
  end;
end;

procedure TClassDefine.SaveRootClassToPath(APath: string);
var
  Lines : TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(GetExecPath + 'Templet.txt');

    Lines.Text := StringReplace(Lines.Text, '//UnitName//', Copy(Name, 2, Length(Name)), []);
    Lines.Text := StringReplace(Lines.Text, '//Uses//', '', []);
    Lines.Text := StringReplace(Lines.Text, '//ClassName//', Name, [rfReplaceAll]);

    Lines.Text := StringReplace(Lines.Text, '//Type//', get_Type, []);
    Lines.Text := StringReplace(Lines.Text, '//EventHandlerInterface//', '', []);
    Lines.Text := StringReplace(Lines.Text, '//Private//', '', []);
    Lines.Text := StringReplace(Lines.Text, '//PropertyPrivate//', get_Private, []);
    Lines.Text := StringReplace(Lines.Text, '//Public//', get_Public, []);
    Lines.Text := StringReplace(Lines.Text, '//Published//', get_Published, []);

    Lines.Text := StringReplace(Lines.Text, '//Constructor//', '', []);
    Lines.Text := StringReplace(Lines.Text, '//SetEventHandler//', '', []);
    Lines.Text := StringReplace(Lines.Text, '//Destructor//', '', []);

    Lines.Text := StringReplace(Lines.Text, '//EventHandlerImplementation//', '', []);
    Lines.Text := StringReplace(Lines.Text, '//Implementation//', get_Implementation, []);

    Lines.SaveToFile(APath + Copy(Name, 2, Length(Name)) + '.pas');
  finally
    Lines.Free;
  end;
end;

procedure TClassDefine.Translate(AText: string);
var
  Loop : Integer;
  Line : string;
  Lines : TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := AText;

    FName := Lines.Strings[0];
    FReferenceName := Copy(FName, 2, Length(FName));

    for Loop := 1 to Lines.Count - 1 do begin
      Line := Lines.Strings[Loop];

           if Pos('=', Line) > 0 then do_EventType(Line)
      else if LowerCase(Copy(Line, 1, Length('procedure'))) = 'procedure' then do_Procedure(Line)
      else if LowerCase(Copy(Line, 1, Length('function'))) = 'function' then do_Function(Line)
      else if LowerCase(Copy(Line, 1, Length('event'))) = 'event' then do_Event(Line)
    end;
  finally
    Lines.Free;
  end;
end;

{ TMemberDefine }

constructor TMemberDefine.Create(AClassDefine:TClassDefine);
begin
  inherited Create;

  FClassDefine := AClassDefine;

  FTokens := TStringList.Create;

  FParser := TPascalParser.Create(nil);
  FParser.OnToken := on_Token;
end;

destructor TMemberDefine.Destroy;
begin
  FParser.Free;
  FTokens.Free;

  inherited;
end;

procedure TMemberDefine.on_Token(Sender: TObject;
  ATokenType: TPascalParserTokenType; AText: string);
begin
  FTokens.Add(AText);
end;

procedure TMemberDefine.Translate(AText: string);
begin
  FText := AText;
  FParser.Execute(AText);
end;

{ TMethodDefine }

procedure TMethodDefine.Translate(AText: string);
begin
  inherited;

  Body := AText;

  try
    Body := DeleteLeft(Body, FTokens[1]);
  except
    raise Exception.Create('TMethodDefine.Translate: ' + AText);
  end;
end;

{ TEventDefine }

procedure TEventDefine.Translate(AText: string);
begin
  inherited;

  Header := 'property';

  // event OnNewData : TNewDataEvent;
  try
    Name := FTokens[1];

    MethodName := Name;
    Delete(MethodName, 1, 2);

    EventType := FTokens[3];
    Body := Format(': %s read F%s write F%s;', [EventType, Name, Name]);
  except
    raise Exception.Create('TEventDefine.Translate: ' + AText);
  end;
end;

{ TEventTypeDefine }

procedure TEventTypeDefine.Translate(AText: string);
begin
  inherited;

  // TNotifyEvent = procedure (Sender:TObject) of object;
  try
    Name := FTokens[0];
    Header := FTokens[0];
    Body := DeleteLeft(AText, FTokens[1]);

    // 왼쪽 뜯어내기
    Templet := Trim(DeleteLeftPlus(AText, FTokens[2]));

    // 오른쪽 뜯어내기
    Templet := DeleteRight(Templet, ')');

    Templet := Format('%s @Name%s;', [FTokens[2], Templet]);
  except
    raise Exception.Create('TEventTypeDefine.Translate: ' + AText);
  end;
end;

end.
