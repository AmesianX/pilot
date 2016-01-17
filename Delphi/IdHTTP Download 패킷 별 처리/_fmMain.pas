unit _fmMain;

interface

uses
  ThreadRepeater,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    IdHTTP1: TIdHTTP;
    moMsg: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    procedure IdHTTP1Work(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure FormCreate(Sender: TObject);
    procedure IdHTTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure IdHTTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure IdHTTP1Disconnected(Sender: TObject);
    procedure IdHTTP1Connected(Sender: TObject);
    procedure IdHTTP1Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
  private
    FRepeater : TThreadRepeater;
    procedure on_Repeat(Sender:TObject);
  public
  end;

var
  fmMain: TfmMain;

implementation

type
  THTTPSream = class (TStream)
  private
    FSize : int64;
    FPosition : int64;
  protected
    function GetSize: Int64; override;
  public
    constructor Create;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

{$R *.dfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  IdHTTP1.Disconnect;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FRepeater := TThreadRepeater.Create(Self);
  FRepeater.Execute(on_Repeat);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FRepeater.Stop;

  FreeAndNil(FRepeater);
end;

procedure TfmMain.IdHTTP1Connected(Sender: TObject);
begin
  fmMain.moMsg.Lines.Add('IdHTTP1Connected');
end;

procedure TfmMain.IdHTTP1Disconnected(Sender: TObject);
begin
  fmMain.moMsg.Lines.Add('IdHTTP1Disconnected');
end;

procedure TfmMain.IdHTTP1Status(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  fmMain.moMsg.Lines.Add('IdHTTP1Status: ' + AStatusText);
end;

procedure TfmMain.IdHTTP1Work(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  //
end;

procedure TfmMain.IdHTTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  fmMain.moMsg.Lines.Add(Format('AWorkCountMax: %d', [AWorkCountMax]));
end;

procedure TfmMain.IdHTTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  fmMain.moMsg.Lines.Add('IdHTTP1WorkEnd');
end;

procedure TfmMain.on_Repeat(Sender: TObject);
var
  HTTPSream : THTTPSream;
begin
  HTTPSream := THTTPSream.Create;
  try
    try
      IdHTTP1.Get('http://app.showstock.co.kr/downloads/030821.ssr', HTTPSream);
    except
      fmMain.moMsg.Lines.Add('IdHTTP1.Get Error.');
    end;
  finally
    HTTPSream.Free;
    FRepeater.Terminate;
  end;
end;

{ THTTPSream }

constructor THTTPSream.Create;
begin
  inherited;

  FSize := 0;
  FPosition := 0;
end;

function THTTPSream.GetSize: Int64;
begin
  Result := FSize;
end;

function THTTPSream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

function THTTPSream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FPosition;
end;

function THTTPSream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FPosition;
end;

function THTTPSream.Write(const Buffer; Count: Integer): Longint;
begin
  FSize := FSize + Count;
  fmMain.moMsg.Lines.Add(Format('Position: %d, Size: %d, Count: %d', [Position, Size, Count]));
end;

end.
