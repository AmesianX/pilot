unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  PackageModule : HModule;
  AClass : TPersistentClass;
begin
  PackageModule := LoadPackage('./pkgTest.bpl');

  if PackageModule = 0 then Exit;

  AClass := GetClass('TfmTest');

  if AClass = nil then Exit;

  try
    with TComponentClass(AClass).Create(Application) as TCustomForm do begin
      ShowModal;
      Free;
    end;
  finally
    UnloadPackage(PackageModule);
  end;
end;

end.
