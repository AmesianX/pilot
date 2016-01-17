unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, rkPathViewer, rkVistaProBar,
  rkVistaPanel, rkSmartPath, rkGlassButton, rkAeroTabs, _frShellControl;

type
  TfmMain = class(TForm)
    rkAeroTabs1: TrkAeroTabs;
    rkSmartPath1: TrkSmartPath;
    frShellControl: TfrShellControl;
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

end.
