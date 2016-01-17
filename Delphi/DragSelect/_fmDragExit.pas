unit _fmDragExit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, _fmDragSelect;

type
  TfmDragExit = class(TForm)
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    DragSelect : TfmDragSelect;
  end;

var
  fmDragExit: TfmDragExit;

implementation

{$R *.dfm}

procedure TfmDragExit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DragSelect.Close;
  Action := caFree;
end;

procedure TfmDragExit.FormShow(Sender: TObject);
begin
  SetWindowPos(Handle, HWND_TOPMOST , 0 , 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TfmDragExit.SpeedButton1Click(Sender: TObject);
begin
  Self.Close;
end;

end.
