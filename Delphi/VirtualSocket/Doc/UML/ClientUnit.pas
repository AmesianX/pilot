unit ClientUnit;

interface

uses
  Base,
  Classes, SysUtils;

type
  TVirtualSocketClientIndy9 = class(TVirtualSocketClientProvider)
  strict private
  strict protected
    procedure on_Connected(AChannel:byte); override;
    procedure on_Disconnected(AChannel:byte); override;
    procedure on_Received(AChannel:byte; AData:pointer; ASize:integer); override; 
  public
    function Connect:boolean; override;
    procedure Disconnect; override;

    procedure Send(AData:pointer; ASize:integer); override;
  end;

implementation

{ TVirtualSocketClientIndy9 }

function TVirtualSocketClientIndy9.Connect: boolean;
begin

end;

procedure TVirtualSocketClientIndy9.Disconnect;
begin

end;

procedure TVirtualSocketClientIndy9.on_Connected(AChannel: byte);
begin

end;

procedure TVirtualSocketClientIndy9.on_Disconnected(AChannel: byte);
begin

end;

procedure TVirtualSocketClientIndy9.on_Received(AChannel: byte; AData: pointer;
  ASize: integer);
begin

end;

procedure TVirtualSocketClientIndy9.Send(AData: pointer; ASize: integer);
begin

end;

end.
