unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TNewDragObject = class(TDragObject)
    Source : TTreeNode;
  end;

  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Memo1: TMemo;
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeView1StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  Caption:=
    TTreeNode(TNewDragObject(Source).Source).Text + ' --> ' +
    TreeView1.DropTarget.Text;
end;

procedure TForm1.TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept:= true;
end;

procedure TForm1.TreeView1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  DragObject:= TNewDragObject.Create;
  TNewDragObject(DragObject).Source:= TreeView1.Selected;
end;

end.
