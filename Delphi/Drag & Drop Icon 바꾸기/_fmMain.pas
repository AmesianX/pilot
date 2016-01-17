unit _fmMain;

interface

uses
  Disk, NewPanel,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Panel1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FNewPanel : TNewPanel;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

var
  OldCursor : HCursor;

procedure HideCursor;
var
  Cursor : HCursor;
begin
 Cursor := LoadCursorFromFile(PChar(GetExecPath+'blank.cur'));
 SetSystemCursor(Cursor, OCR_NORMAL);
end;

procedure ShowCursor;
begin
  SetSystemCursor(OldCursor, OCR_NORMAL);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FNewPanel := TNewPanel.Create(Self);
  FNewPanel.Parent := Self;
  FNewPanel.DragMode := dmAutomatic;

  // 그물로 바꾸기
  Screen.Cursors[crNoDrop] := LoadCursorFromFile(PChar(GetExecPath+'NoDrop.cur'));
end;

procedure TfmMain.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  Caption := 'Panel1DragDrop';
end;

procedure TfmMain.Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Caption := 'Panel1DragOver';
end;

procedure TfmMain.Panel1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  Caption := 'Panel1EndDrag';
end;

begin
 OldCursor := CopyIcon(Screen.Cursors[crDefault]);
end.
