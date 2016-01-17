unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, jpeg, Menus;

const
  DefaultMemoString : String = '['#51228#47785']'#13#10 +
                               '1 : '#13#10 +
                               '2 : '#13#10 +
                               '3 :';
type
  TForm1 = class(TForm)
    Memo: TMemo;
    Panel1: TPanel;
    EditTitle: TEdit;
    BtnTitle: TButton;
    ImageLog: TImage;
    EditLog1: TEdit;
    EditLog2: TEdit;
    BtnLog: TSpeedButton;
    ImageRoot: TImage;
    EditRoot1: TEdit;
    EditRoot2: TEdit;
    BtnRoot: TSpeedButton;
    ImageDIV: TImage;
    EditDiv2: TEdit;
    EditDiv1: TEdit;
    BtnDiv: TSpeedButton;
    EditPow1: TEdit;
    EditPow2: TEdit;
    BtnPower: TSpeedButton;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    New1: TMenuItem;
    SaveAs1: TMenuItem;
    ImageINFINITY: TImage;
    BtnInfinite: TSpeedButton;
    BtnAddAnswer: TButton;
    ImagePI: TImage;
    BtnPI: TSpeedButton;
    procedure EditTitleMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure BtnTitleClick(Sender: TObject);
    procedure BtnLogClick(Sender: TObject);
    procedure BtnRootClick(Sender: TObject);
    procedure BtnDivClick(Sender: TObject);
    procedure BtnPowerClick(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure BtnInfiniteClick(Sender: TObject);
    procedure BtnAddAnswerClick(Sender: TObject);
    procedure BtnPIClick(Sender: TObject);
  private
    FFileName : String;
    procedure SetFileName(const Value: String);
    property FileName : String read FFileName write SetFileName;
    procedure InsertString(Data : String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BtnAddAnswerClick(Sender: TObject);
var
  Num : Integer;
begin
  Num := Memo.Lines.Count;
  Memo.Lines.Add(Format('%d : ',[Num]));
end;

procedure TForm1.BtnDivClick(Sender: TObject);
begin
  InsertString(Format('%s/%s',[EditDiv1.Text, EditDiv2.Text]));
  EditDiv1.Text := '';
  EditDiv2.Text := '';
end;

procedure TForm1.BtnInfiniteClick(Sender: TObject);
begin
  InsertString('INFINIT');
end;

procedure TForm1.BtnLogClick(Sender: TObject);
begin
  InsertString(Format('LOG{%s,%s}',[EditLog1.Text, EditLog2.Text]));
  EditLog1.Text := '';
  EditLog2.Text := '';
end;

procedure TForm1.BtnPIClick(Sender: TObject);
begin
  InsertString('PI');
end;

procedure TForm1.BtnPowerClick(Sender: TObject);
begin
  InsertString(Format('%s^%s',[EditPow1.Text, EditPow2.Text]));
  EditPow1.Text := '';
  EditPow2.Text := '';
end;

procedure TForm1.BtnRootClick(Sender: TObject);
begin
  if EditRoot1.Text = '' then EditRoot1.Text := '2';
  InsertString(Format('ROOT{%s,%s}',[EditRoot1.Text, EditRoot2.Text]));
  EditRoot1.Text := '';
  EditRoot2.Text := '';
end;

procedure TForm1.BtnTitleClick(Sender: TObject);
begin
  Memo.Lines[0] := '[' + EditTitle.Text + ']';
end;

procedure TForm1.EditTitleMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
var
  Edit : TEdit Absolute Sender;
begin
  Edit.SelStart := 0;
  Edit.SelLength := Length(Edit.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FileName := '';
  Memo.Lines.Text := DefaultMemoString;
end;

procedure TForm1.InsertString(Data: String);
var
  BeforePos : TPoint;
  OrginalString : WideString;
  LeftString, RightString : WideString;
begin
  BeforePos := Memo.CaretPos;
  OrginalString := Memo.Lines[BeforePos.Y];
  LeftString := Copy(OrginalString, 1, BeforePos.X);
  RightString := Copy(OrginalString, BeforePos.X + 1, Length(OrginalString) - BeforePos.X);
  Memo.Lines[BeforePos.Y] := LeftString + Data + RightString;
  BeforePos.X := Length(LeftString) + Length(Data);
  Memo.CaretPos := BeforePos;
end;

procedure TForm1.Load1Click(Sender: TObject);
begin
  if OpenDialog.Execute(Self.Handle) then begin
    Memo.Lines.LoadFromFile(OpenDialog.FileName);
    FileName := OpenDialog.FileName;
  end;
end;

procedure TForm1.New1Click(Sender: TObject);
begin
  FileName := '';
  Memo.Lines.Text := DefaultMemoString;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if FileName <> '' then Memo.Lines.SaveToFile(FileName);
  if SaveDialog.Execute(Self.Handle) then begin
    FileName := SaveDialog.FileName;
    Memo.Lines.SaveToFile(FileName);
  end;
end;

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog.Execute(Self.Handle) then begin
    FileName := SaveDialog.FileName;
    Memo.Lines.SaveToFile(FileName);
  end;
end;

procedure TForm1.SetFileName(const Value: String);
begin
  FFileName := Value;
  Caption := '정답 입력기 - ' + ExtractFileName(FFileName);
end;

end.
