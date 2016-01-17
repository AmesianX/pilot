unit UI_Interface;

interface

uses
  Classes, SysUtils, _frmMain;

type
  TUI = class
  protected
    procedure on_KeyDown(Key:Word);
    procedure on_Start;
  public
    _fmMain : T_fmMain;
    class function GetObject:TUI;
    procedure GameEnd;
  end;

implementation

uses
  Logic_Interface;

var
  MyObject : TUI = Nil;

{ TUI }

class function TUI.GetObject: TUI;
begin
  if MyObject = Nil then MyObject:= TUI.Create;
  Result:= MyObject;
end;

procedure TUI.on_Start;
begin
  TLI.GetObject.StartGame;
end;

procedure TUI.on_KeyDown(Key:Word);
begin
  if TLI.GetObject.GameStarted = True then TLI.GetObject.Process_KeyDown(Key);
end;

procedure TUI.GameEnd;
begin
  Self._fmMain.GameEnd;
end;

end.
