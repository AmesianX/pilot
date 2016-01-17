unit _fmMain;

interface

uses
  ClassList, Disk,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btOpen: TButton;
    btSave: TButton;
    btExecute: TButton;
    moSrc: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
  private
    FClassList : TClassList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btExecuteClick(Sender: TObject);
begin
  FClassList.Translate(moSrc.Text);
  FClassList.SaveToPath(GetExecPath + 'OutPut');
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FClassList := TClassList.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FClassList.Free;
end;

end.
