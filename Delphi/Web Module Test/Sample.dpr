program Sample;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  _fmMain in '_fmMain.pas' {Form1},
  WebServer in '..\..\..\Lib\RyuLib\Delphi\XE2\WebServer.pas' {WebServerModule: TWebModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
