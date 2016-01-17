unit _dmMain;

interface

uses
  SysUtils, Classes, Forms;

type
  TAfmMain = class(TForm)
  private
  protected
    procedure on_Start;
  public
    procedure Display(Numbers:array of integer); virtual; abstract;
  end;

  TARandNo = class(TComponent)
  private
  protected
    Numbers : array of integer;
    procedure on_Execute;
  public
    procedure Execute; virtual; abstract;
  end;

  TdmMain = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
  public
    fmMain : TAfmMain;
    RandNo : TARandNo;
  end;

var
  dmMain: TdmMain;

implementation

uses
  _fmMain, RandNo;

{$R *.dfm}

{ TAfmMain }

procedure TAfmMain.on_Start;
begin
  dmMain.RandNo.Execute;
end;

{ TRandNo }

procedure TARandNo.on_Execute;
begin
  dmMain.fmMain.Display(Numbers);
end;

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  Application.CreateForm(TfmMain, Self.fmMain);
  Self.RandNo:= TRandNo.Create(Self);
end;

end.
