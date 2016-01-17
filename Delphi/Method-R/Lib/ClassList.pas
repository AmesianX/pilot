unit ClassList;

interface

uses
  ClassDefines, ValueList, Disk, Strg,
  Classes, SysUtils;

type
  TClassList = class
  private
    FBuffer : TStringList;
    FClassDefines : array of TClassDefine;
    procedure do_Clear;
    procedure do_Pop;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Translate(AText:string);
    procedure SaveToPath(APath:string);

    property Count : integer read GetCount;
  end;

implementation

{ TClassList }

constructor TClassList.Create;
begin
  inherited;

  FBuffer := TStringList.Create;
end;

destructor TClassList.Destroy;
begin
  do_Clear;
  FBuffer.Free;

  inherited;
end;

procedure TClassList.do_Clear;
var
  Loop: Integer;
begin
  for Loop := 0 to Length(FClassDefines) - 1 do FClassDefines[Loop].Free;
  SetLength(FClassDefines, 0);
end;

procedure TClassList.do_Pop;
var
  iIndex : integer;
begin
  if FBuffer.Count = 0 then Exit;
                   
  iIndex := Length(FClassDefines);
  SetLength(FClassDefines, iIndex + 1);

  FClassDefines[iIndex] := TClassDefine.Create;
  FClassDefines[iIndex].Translate(FBuffer.Text);
  FBuffer.Clear;
end;

function TClassList.GetCount: integer;
begin
  Result := Length(FClassDefines);
end;

procedure TClassList.SaveToPath(APath: string);
var
  Loop : Integer;
begin
  if Count = 0 then Exit;  

  if Copy(APath, Length(APath), 1) <> '\' then APath := APath + '\';
  ForceDirectories(APath);
  EraseFiles(APath + '*.*');

  FClassDefines[1].SaveBaseClassToPath(APath, FClassDefines);
  for Loop := 2 to Count-1 do FClassDefines[Loop].SaveRootClassToPath(APath);
end;

procedure TClassList.Translate(AText: string);
var
  Loop : Integer;
  Line : string;
  Lines : TStringList;
begin
  do_Clear;

  Lines := TStringList.Create;
  try
    Lines.Text := AText;

    for Loop := 0 to Lines.Count - 1 do begin
      // 주석 및 공백 문자 제거
      Line := Trim(Lines.Strings[Loop]);
      if Line = '' then begin
        do_Pop;
      end else begin
        Line := DeleteRightPlus(Line, '//');
        Line := Trim(Line);
        if Line <> '' then FBuffer.Add(Line);
      end;
    end;

    if FBuffer.Count > 0 then do_Pop;    
  finally
    Lines.Free;
  end;
end;

end.
