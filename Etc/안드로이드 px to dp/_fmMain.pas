unit _fmMain;

interface

uses
  Scanner, Strg,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    moIn: TMemo;
    PopupMenu: TPopupMenu;
    miSelectAll: TMenuItem;
    cbDensity: TComboBox;
    btConvert: TButton;
    procedure miSelectAllClick(Sender: TObject);
    procedure btConvertClick(Sender: TObject);
    procedure cbDensityKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbDensityKeyPress(Sender: TObject; var Key: Char);
    procedure cbDensityKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure cbDensityChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRatio : Double;
    FScanner : TScanner;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

function isPX(AText:string):boolean;
var
  iPos : integer;
begin
  iPos := Pos('px', LowerCase(AText));
  Result := (iPos > 0) and (iPos = (Length(AText) - 1));
end;

function getPX(AText:string):integer;
begin
  Result := StrToIntDef(Trim(Copy(AText, 1, Length(AText) - 2)), 0);
end;

procedure TfmMain.btConvertClick(Sender: TObject);
type
  TStatus = (stNormal, stString);
var
  Loop: Integer;
  Status : TStatus;
  sIn, sResult, sBuffer : string;

  procedure do_Normal(AIndex:integer);
  begin
    if sIn[AIndex] = '"' then begin
      Status := stString;
      sResult := sResult + sBuffer;
      sBuffer := '';
    end else begin
      sBuffer := sBuffer + sIn[AIndex];
    end;
  end;

  procedure do_String(AIndex:integer);
  var
    px : integer;
    dp : Double;
  begin
    if sIn[AIndex] = '"' then begin
      Status := stNormal;

      if not isPX(sBuffer) then begin
        sResult := sResult + Format('"%s"', [sBuffer]);
      end else begin
        px := getPX(sBuffer);
        dp := px * FRatio;
        sResult := sResult + Format('"%ddp"', [Round(dp)]);
      end;

      sBuffer := '';
    end else begin
      sBuffer := sBuffer + sIn[AIndex];
    end;
  end;

  procedure do_Flush;
  begin
    if sBuffer <> '' then begin
      Status := stNormal;
      sResult := sResult + sBuffer;
      sBuffer := '';
    end;
  end;

begin
  sIn := moIn.Text;
  sResult := '';
  sBuffer := '';

  Status := stNormal;

  for Loop := 1 to Length(sIn) do begin
    case Status of
      stString: do_String(Loop);
      else do_Normal(Loop);
    end;
  end;

  do_Flush;

  moIn.Text := sResult;
end;

procedure TfmMain.cbDensityChange(Sender: TObject);
begin
  case cbDensity.ItemIndex of
    0: FRatio := 160 / 120;
    2: FRatio := 160 / 240;
    3: FRatio := 160 / 320;
    4: FRatio := 160 / 480;
    5: FRatio := 160 / 640;
    else FRatio := 1.0;
  end;
end;

procedure TfmMain.cbDensityKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then Exit;

  Key := 0;
end;

procedure TfmMain.cbDensityKeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;
end;

procedure TfmMain.cbDensityKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then Exit;

  Key := 0;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  cbDensity.ItemIndex := 2;
  FRatio := 160 / 240;

  FScanner := TScanner.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScanner);
end;

procedure TfmMain.miSelectAllClick(Sender: TObject);
begin
  moIn.SelectAll;
end;

end.
