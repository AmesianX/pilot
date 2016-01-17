unit _dmMain;

interface

uses
  SysUtils, Classes, SearchDir, Dialogs;

type
  TdmMain = class(TDataModule)
    SearchDir: TSearchDir;
    procedure SearchDirFindFile(Sender: TObject; Folder, FileName: string);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLines : TStringList;
    function do_EscapeHanGul(Text:string):string;
    procedure do_SetNumber;
    function do_RevertHanGul(Text:string):string;
  public
    procedure Start(Path:string);
  end;

var
  dmMain: TdmMain;

implementation

uses
  Disk, Bit, Strg;

{$R *.dfm}

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  FLines:= TStringList.Create;
end;

procedure TdmMain.DataModuleDestroy(Sender: TObject);
begin
  FLines.Free;
end;

function TdmMain.do_EscapeHanGul(Text:string): string;
var
  Loop : integer;
begin
  Result:= '';
  for Loop := 1 to Length(Text) do
    if Text[Loop] > #127 then
      Result:= Result + '{$kR' + IntToHex(Byte(Text[Loop]), 2) + '}'
    else
      Result:= Result + Text[Loop];
end;

function TdmMain.do_RevertHanGul(Text: string): string;
var
  iIndex, iLength : integer;
begin
  Result:= '';

  iIndex:= 1;
  iLength:= Length(Text);
  while iIndex <= iLength do begin
    if Copy(Text, iIndex, 4) = '{$kR' then begin
      Result:= Result + Char(HexToByte(Copy(Text, iIndex+4, 2)));
      iIndex:= iIndex + 4 + 2;
    end else
      Result:= Result + Text[iIndex];

    iIndex:= iIndex + 1;
  end;
end;

procedure TdmMain.do_SetNumber;
var
  Loop : integer;
begin
  for Loop := 0 to FLines.Count - 1 do
    FLines.Strings[Loop]:= Format('%4d : %s', [Loop, FLines.Strings[Loop]]);
end;

procedure TdmMain.SearchDirFindFile(Sender: TObject; Folder,
  FileName: string);
var
  sExt, sFolder, sSrcFile, sTempFile, sParam : string;
begin
  sExt:= LowerCase(ExtractFileExt(FileName));
  if (sExt = '.pas') or (sExt = '.dpr') then begin
    sFolder:= Folder;
    while Pos('\', sFolder) > 0 do DeleteLeftPlus(sFolder, '\');
    sSrcFile:= Folder + '\' + FileName;
    sTempFile:= GetExecPath + 'TempFile';

    // 한글을 코드화
    FLines.LoadFromFile(sSrcFile);
    FLines.Text:= #13 + do_EscapeHanGul(FLines.Text);

    // 번호 붙이고 저장
    do_SetNumber;
    FLines.SaveToFile(sTempFile + '.tmp1');

    // Pascal --> html
    sParam:= '/c PasH.exe < TempFile.tmp1 > TempFile.tmp2';
    ShellExecuteWait('cmd', sParam, GetExecPath);

    // 한글코드를 한글로 복원
    FLines.LoadFromFile(sTempFile + '.tmp2');
    FLines.Text:= do_RevertHanGul(FLines.Text);
    FLines.SaveToFile(sSrcFile + '.html');
  end;
end;

procedure TdmMain.Start(Path: string);
begin
  SearchDir.Search(Path, false);

  Disk.EraseFiles(GetExecPath+'*.tmp1');
  Disk.EraseFiles(GetExecPath+'*.tmp2');
end;

initialization
  dmMain:= TdmMain.Create(nil);
finalization
  dmMain.Free;
end.
