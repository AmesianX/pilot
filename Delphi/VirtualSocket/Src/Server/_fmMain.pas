unit _fmMain;

interface

uses
  VirtualSocketUtils, VirtualSocketServer,
  DeskServer, TextServer, CamServer,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FProvider : TVirtualSocketServerProvider;
    DeskServer : TDeskServer;
    TextServer : TTextServer;
    CamServer : TCamServer;
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  VirtualSocketServerProviderIndy9;

{$R *.dfm}

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FProvider.Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FProvider := TVirtualSocketServerProviderIndy9.Create(Self);

  // 중요도 순으로 생성한다.  클라이언트와 순서를 맞춰야 한다.
  DeskServer := TDeskServer.Create(Self, FProvider.CreateSocket);
  TextServer := TTextServer.Create(Self, FProvider.CreateSocket);
  CamServer  := TCamServer.Create (Self, FProvider.CreateSocket);

  FProvider.Port := 1234;
  FProvider.Start;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FProvider.Free;
end;

end.
