unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
  TForm1 = class(TForm)
    IdHTTP: TIdHTTP;
    Button1: TButton;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    Memo.Text :=
//      IdHTTP.Get('http://api.wecandeo.com/video/default/BOKNS9AQWrFSTupSbNE3M60gpF3OhwV3Pn28AxwtqfFIMkUxiicgTWIMOwkBTzaHcz9qXdAa754BDM55nuoXj7MJteyHOb4J7WZHhNSvMRcoie');
//      IdHTTP.Get('http://fms.wecandeo.com/100/645/2015/06/26/14/V1874409.mp4?key=OagwnG0nzkt04lcxWWdYVEfwvDyDw20TcAH6W0ZJpRebxoG5kN7dQleqU5QQWOesaJrE%2BipssaIe3V7pWA9hDttMpaO3M69gBrc%2F2U4cNQo%3D&packageId=1002466&videoId=5717392');
      IdHTTP.Get('http://14.0.89.42:80/100/317/100-3439317-8e45.mp4?_metahint_fn=/100/645/2015/06/26/14/V1874409.mp4&key=OagwnG0nzkt04lcxWWdYVEfwvDyDw20TcAH6W0ZJpRebxoG5kN7dQleqU5QQWOesaJrE+ipssaIe3V7pWA9hDttMpaO3M69gBrc/2U4cNQo=&packageId=1002466&videoId=5717392');
  except
    on E : Exception do Memo.Text := IdHTTP.Response.RawHeaders.Text
  end;
end;

end.
