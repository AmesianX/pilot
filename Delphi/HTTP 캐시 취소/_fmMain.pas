unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    moMsg: TMemo;
    IdHTTP: TIdHTTP;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  Expires : TDateTime;
begin
  try
    Expires := 0;
    IdHTTP.Request.CacheControl := 'no-cache';
    IdHTTP.Request.Pragma := 'no-cache';
    IdHTTP.Request.Expires := Expires;
    IdHTTP.Request.ContentType := 'application/json';
    IdHTTP.Request.ContentEncoding := 'utf-8';
//    IdHTTP.Request.ContentType := 'application/octet-stream';

//    IdHTTP.Get('http://www.mediawave.kr/files/HiMyTV/FileList.json');
//    moMsg.Lines.Text := IdHTTP.Request.RawHeaders.Text;

    moMsg.Lines.Add(IdHTTP.Get('http://www.mediawave.kr/files/HiMyTV/FileList.json'));
  except
    on E: Exception do begin
      OutputDebugString(PChar(Format('TFileList.LoadFromWeb: %s', [E.Message])));
    end;
  end;
end;

end.
