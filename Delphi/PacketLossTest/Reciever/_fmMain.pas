unit _fmMain;

interface

uses
  p2pUtils, p2pServer, IdSocketHandle,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmMain = class(TForm)
    EditIP: TLabeledEdit;
    EditPeerIP: TLabeledEdit;
    btnAccept: TButton;
    lblErrorPercent: TStaticText;
    lblSpeed: TStaticText;
    lblPing: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Data : TMemoryStream;
    p2p : Tp2pServer;
    RecvCount : Integer;
    AllCount, Failed : Integer;
    StartTick : Cardinal;
    BeforeUITick : Cardinal;
    BeforePingTick : Cardinal;
    PingTickSum : Int64;
    MaxPing, MinPing : Integer;

    procedure MakeData;

    procedure on_AddressChanged(Sender: TObject);
    procedure do_WMUser(var Msg: TMessage); message WM_USER;
    procedure do_PerCentUpdate(var Msg: TMessage); message WM_USER+1;
    procedure on_Received(Sender: TObject; ABinding: TIdSocketHandle;
      AData: pointer; ASize: word);

    function CompareStream(S1, S2 : TMemoryStream):Boolean;
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
  RecvCount := 0;
  AllCount := 0;
  Failed := 0;
  MakeData;

  p2p := Tp2pServer.Create(Self);
  p2p.Gateway.URL := 'station1.megachannel.co.kr:10000';
  p2p.OnAddressChanged := on_AddressChanged;
  p2p.OnReceived := on_Received;
  p2p.Open;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  p2p.Close;
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

procedure TfmMain.btnAcceptClick(Sender: TObject);
var
  Host : string;
  Port : integer;
begin
  if EditPeerIP.Text = '' then exit;

  URLtoAddress(EditPeerIP.Text, Host, Port);
  RecvCount := 0;
  AllCount := 0;
  Failed := 0;
  PingTickSum := 0;
  MaxPing := 0;
  MinPing := MaxInt;

  p2p.SendText(Host, Port, #0);
  StartTick := GetTickCount;
  BeforePingTick := StartTick;
end;

function TfmMain.CompareStream(S1, S2: TMemoryStream): Boolean;
var
  i : Integer;
  pS1, pS2 : PInteger;
begin
  Result := False;
  if (S1.Size + 4) <> S2.Size then exit;

  pS1 := S1.Memory;
  pS2 := S2.Memory;

  if RecvCount = 0 then begin
    RecvCount := pS2^;
  end
  else if RecvCount <> pS2^ then begin
    RecvCount := pS2^;
    exit;
  end;

  Inc(pS2);

  for i := 0 to 256 - 1 do begin
    if pS1^ <> pS2^ then exit;
    Inc(pS1);
    Inc(pS2);
  end;

  Result := true;
end;

procedure TfmMain.do_PerCentUpdate(var Msg: TMessage);
var
  Str : String;
begin
  if (GetTickCount - BeforeUITick) < 150 then exit;
  BeforeUITick := GetTickCount;

  Str := Format('오류율 : %d/%d, %f%%', [Failed, AllCount, ((Failed / AllCount) * 100)]);
  lblErrorPercent.Caption := Str;

  if (GetTickCount - StartTick) < 1000 then exit;

  Str := Format('속도 : %fKBps', [(AllCount / (GetTickCount - StartTick)) * 1000 ]);
  lblSpeed.Caption := Str;

  Str := Format('평균핑 : %dms, 최대 : %dms, 최소 %dms', [(PingTickSum div AllCount), MaxPing, MinPing]);
  lblPing.Caption := Str;
end;

procedure TfmMain.do_WMUser(var Msg: TMessage);
begin
  EditIP.Text := p2p.MyAddress.URL;
end;

procedure TfmMain.on_Received(Sender: TObject; ABinding: TIdSocketHandle;
  AData: pointer; ASize: word);
var
  Stream : TMemoryStream;
  a : Char;
  NowTick : Cardinal;
  NowPing : Cardinal;
  Packet : TStringList;
begin
  Stream := TMemoryStream.Create;
  Packet := TStringList.Create;
  try
    Stream.Write(AData^, ASize);
    if CompareStream(Data, Stream) <> true then begin
      Inc(Failed);
    end;
    Inc(AllCount);
    Inc(RecvCount);

//Tick Ping Calc Begin
    NowTick := GetTickCount;
    NowPing := (NowTick - BeforePingTick);
    PingTickSum := PingTickSum + NowPing;
    BeforePingTick := NowTick;

    if MaxPing < NowPing then MaxPing := NowPing;
    if MinPing > NowPing then MinPing := NowPing;
//Tick Ping Calc End;

    PostMessage(Handle, WM_User+1, 0, 0);

//Make Packet
    Stream.Clear;
    
    Packet.Values['Error'] := Format('오류율 : %d/%d', [Failed, AllCount]);
    if GetTickCount - StartTick > 1 then begin
      Packet.Values['Speed'] := Format('속도 : %fKBps', [(AllCount / (GetTickCount - StartTick)) * 1000 ]);
    end;    
    Packet.Values['Ping'] := Format('평균핑 : %dms, 최대 : %dms, 최소 %dms', [(PingTickSum div AllCount), MaxPing, MinPing]);
    Stream.Write(Packet.GetText^, Length(Packet.Text));

    p2p.SendStream(ABinding.PeerIP, ABinding.PeerPort, Stream);
  finally
    Stream.Free;
    Packet.Free
  end;
end;

end.
