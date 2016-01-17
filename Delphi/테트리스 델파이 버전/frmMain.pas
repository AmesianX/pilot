unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, _frmMain, ExtCtrls, CanvasPanel, StdCtrls;

type
  TfmMain = class(T_fmMain)
    MainMenu: TMainMenu;
    miStart: TMenuItem;
    plGameBoard: TPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miStartClick(Sender: TObject);
  private
    { Private declarations }
    function GetBoardCanvas: TCanvas; override;
  public
    { Public declarations }
    procedure GameEnd; override;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Self.on_KeyDown(Key);
end;

procedure TfmMain.GameEnd;
begin
  ShowMessage('Game over!!!');
end;

function TfmMain.GetBoardCanvas: TCanvas;
begin
  Result:= plGameBoard.Canvas;
end;

procedure TfmMain.miStartClick(Sender: TObject);
begin
  Self.on_Start;
end;

end.
