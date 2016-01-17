unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FSaveDwmCompositionEnabled : boolean;
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  DwmApi;

{$R *.dfm}

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FSaveDwmCompositionEnabled then
    DwmEnableComposition(DWM_EC_ENABLECOMPOSITION);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
 FSaveDwmCompositionEnabled := DwmCompositionEnabled;
  if FSaveDwmCompositionEnabled then
    DwmEnableComposition(DWM_EC_DISABLECOMPOSITION);
end;

end.
