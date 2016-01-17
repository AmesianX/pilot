unit _wmMain;

interface

uses
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  System.SysUtils, System.Classes, Web.HTTPApp;

type
  TwmMain = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
  public
  end;

procedure StartWebServer(APort:integer);
procedure StopWebServer;

implementation

var
  wmMain : TIdHTTPWebBrokerBridge;

procedure StartWebServer(APort:integer);
begin
  wmMain.Bindings.Clear;
  wmMain.DefaultPort := APort;
  wmMain.Active := True;
end;

procedure StopWebServer;
begin
  wmMain.Active := False;
  wmMain.Bindings.Clear;
end;

{$R *.dfm}

procedure TwmMain.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '<html><heading/><body>Web Server Application</body></html>';
end;

initialization
  if WebRequestHandler <> nil then WebRequestHandler.WebModuleClass := TwmMain;

  wmMain := TIdHTTPWebBrokerBridge.Create(nil);

finalization
  // fmMain.Free;
end.
