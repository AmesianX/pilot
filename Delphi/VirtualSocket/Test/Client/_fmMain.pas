unit _fmMain;

interface

uses
  VirtualSocketUtils, VirtualSocketClient, ValueList,
  DeskClient, TextClient, CamClient,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Image2: TImage;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FProvider : TVirtualSocketClientProvider;
    DeskClient : TDeskClient;
    TextClient : TTextClient;
    CamClient : TCamClient;
    procedure on_TextReceived(Sender:TObject; APacket:TValueList);
    procedure on_CamReceived(Sender:TObject; ACamID,AUserID:string; AData:pointer; ASize:integer);
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  VirtualSocketClientProviderIndy9;

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
var
  Mem : TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  Image1.Picture.Bitmap.SaveToStream(Mem);
  Mem.Position := 0;
  CamClient.SendCam('Lyn', Mem);
  Mem.Free;
end;

procedure TfmMain.Button2Click(Sender: TObject);
begin
  CamClient.NeedCam('Lyn');
end;

procedure TfmMain.Button3Click(Sender: TObject);
begin
  TextClient.sp_Login('Lyn', '1234');
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FProvider.Disconnect;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FProvider := TVirtualSocketClientProviderIndy9.Create(Self);

  // 중요도 순으로 생성한다.  클라이언트와 순서를 맞춰야 한다.
  DeskClient := TDeskClient.Create(Self, FProvider.CreateSocket);
  
  TextClient := TTextClient.Create(Self, FProvider.CreateSocket);
  TextClient.OnReceived := on_TextReceived;
  
  CamClient  := TCamClient.Create (Self, FProvider.CreateSocket);
  CamClient.OnReceived := on_CamReceived;

  FProvider.Host := '127.0.0.1';
  FProvider.Port := 1234;
  if not FProvider.Connect then begin
    MessageDlg('서버 접속에 실패하였습니다.', mtError, [mbOk], 0);
    Application.Terminate;
  end;

  //TextClient.sp_Login('Ryu', '1234');
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FProvider.Free;
end;

procedure TfmMain.on_CamReceived(Sender: TObject; ACamID, AUserID: string;
  AData: pointer; ASize: integer);
var
  Mem : tMemoryStream;
begin
  Mem := TMemoryStream.Create;
  Mem.Write(AData^, ASize);
  Mem.Position := 0;
  Image2.Picture.Bitmap.LoadFromStream(Mem);
  Mem.Free;
end;

procedure TfmMain.on_TextReceived(Sender: TObject; APacket: TValueList);
begin
  //moMsg.Lines.Add(APacket.Text);
end;

end.
