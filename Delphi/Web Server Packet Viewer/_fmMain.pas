unit _fmMain;

interface

uses
  IdContext,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdTCPServer, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    IdTCPServer1: TIdTCPServer;
    moMsg: TMemo;
    procedure IdTCPServer1Execute(AContext: TIdContext);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.IdTCPServer1Execute(AContext: TIdContext);
var
  sName: String;
begin
  sName := AContext.Connection.Socket.ReadLn;
  // Send a response to the client
  AContext.Connection.Socket.WriteLn('Hello, ' + sName + '.');
  AContext.Connection.Socket.WriteLn('Would you like to play a game?');
  // We're done with our session
  AContext.Connection.Disconnect;
end;

end.
