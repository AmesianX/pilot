unit _fmMain;

interface

uses
  VirtualSocketServer, VirtualSocketServerProviderIndy9, DeskCamServer,
  DeskCamSampleUtils,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServerProvider: TVirtualSocketServerProvider;
    FServer: TDeskCamServer;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FServerProvider := TVirtualSocketServerProviderIndy9.Create(Self);
  FServer := TDeskCamServer.Create(Self, FServerProvider.CreateSocket);

  FServerProvider.Port := SERVER_PORT;
  FServerProvider.Start;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FServer.Free;
  FServerProvider.Free;
end;

end.
