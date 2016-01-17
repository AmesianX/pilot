unit _fmMain;

interface

uses
  p2pUtils, p2pServer, IdSocketHandle, ValueList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    EditIP: TLabeledEdit;
    lblError: TLabel;
    lblPing: TLabel;
    lblSpeed: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    p2p : Tp2pServer;
    Data : TMemoryStream;
    SendCount : Integer;
    UITick : Cardinal;

    procedure do_WMUser(var Msg: TMessage); message WM_USER;
    procedure do_RecivePacket(var Msg: TMessage); message WM_USER + 1;
    procedure on_AddressChanged(Sender: TObject);
    procedure MakeData;
    procedure on_Received(Sender: TObject; ABinding: TIdSocketHandle;
      AData: pointer; ASize: word);
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Data := TMemoryStream.Create;
  SendCount := 0;
  UITick := GetTickCount;
  MakeData;

  p2p := Tp2pServer.Create(Self);
  p2p.Gateway.URL := 'station1.megachannel.co.kr:10000';
  p2p.OnAddressChanged := on_AddressChanged;
  p2p.OnReceived := on_Received;
  p2p.Open;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  p2p.Free;
  Data.Free;
end;

procedure TfmMain.MakeData;
var
  Value : Integer;
  i : Integer;
begin
  RandSeed := 0;
  for i := 0 to 256 - 1 do begin
    Value := Random(MAXLONG);
    Data.Write(Value, sizeof(Value));
  end;
end;

procedure TfmMain.on_AddressChanged(Sender: TObject);
begin
  PostMessage(Handle, WM_USER, 0, 0);
end;

procedure TfmMain.do_RecivePacket(var Msg: TMessage);
var
  Packet : TStringList;
  NowTick : Cardinal;
begin
  NowTick := GetTickCount;
  Packet := Pointer(Msg.WParam);
  if (NowTick - UITick) > 100 then begin
    lblError.Caption := Packet.Values['Error'];
    lblSpeed.Caption := Packet.Values['Speed'];
    lblPing.Caption := Packet.Values['Ping'];
    UITick := GetTickCount;
  end;

  Packet.Free;

end;

procedure TfmMain.do_WMUser(var Msg: TMessage);
begin
  EditIP.Text := p2p.MyAddress.URL;
end;

procedure TfmMain.on_Received(Sender: TObject; ABinding: TIdSocketHandle;
  AData: pointer; ASize: word);
var
  Packet : TMemoryStream;
  RPacket : TStringList;
begin
  RPacket := TStringList.Create;
  RPacket.Text := PChar(AData);

  PostMessage(Handle, WM_USER + 1, Integer(RPacket),0);
  Packet := TMemoryStream.Create;
  try
    Packet.Write(SendCount, sizeof(SendCount));
    Packet.Write(Data.Memory^, Data.Size);
    p2p.SendStream(ABinding.PeerIP, ABinding.PeerPort, Packet);
    Inc(SendCount);
  finally
    Packet.Free;
  end;
end;

end.
