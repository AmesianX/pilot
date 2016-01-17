unit _fmMain;

interface

uses
  Disk,
  IdFTPCommon, IdReplyRFC,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdContext, Vcl.StdCtrls,
  IdCustomTCPServer, IdTCPServer, IdCmdTCPServer, IdExplicitTLSClientServerBase,
  IdFTPServer, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdFTP;

type
  TForm1 = class(TForm)
    IdFTP1: TIdFTP;
    IdFTPServer1: TIdFTPServer;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure IdFTPServer1UserLogin(ASender: TIdFTPServerContext;
      const AUsername, APassword: string; var AAuthenticated: Boolean);
    procedure IdFTPServer1Exception(AContext: TIdContext;
      AException: Exception);
    procedure Button2Click(Sender: TObject);
    procedure IdFTPServer1StoreFile(ASender: TIdFTPServerContext;
      const AFileName: string; AAppend: Boolean; var VStream: TStream);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

type
  TUserStream = class (TStream)
  private
    FPosition : int64;
  protected
    function GetSize: Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  UserStream : TUserStream;
begin
  IdFTP1.Disconnect;

  IdFTP1.Username := 'aa';
  IdFTP1.Password := 'aa';
  IdFTP1.TransferType:= ftBinary;
  IdFTP1.Connect;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  UserStream : TUserStream;
begin
  UserStream := TUserStream.Create;
  try
    try
      IdFTP1.Put(UserStream, 'Test.dat');
    except
      Memo1.Lines.Add('* Error: ');
    end;
  finally
    UserStream.Free;
  end;
end;

procedure TForm1.IdFTPServer1Exception(AContext: TIdContext;
  AException: Exception);
begin
  Memo1.Lines.Add(AException.Message);
end;

procedure TForm1.IdFTPServer1StoreFile(ASender: TIdFTPServerContext;
  const AFileName: string; AAppend: Boolean; var VStream: TStream);
begin
  VStream := TFileStream.Create(GetExecPath+AFileName, fmCreate);
end;

procedure TForm1.IdFTPServer1UserLogin(ASender: TIdFTPServerContext;
  const AUsername, APassword: string; var AAuthenticated: Boolean);
begin
  AAuthenticated := true;
end;

{ TUserStream }

constructor TUserStream.Create;
begin
  inherited;

  FPosition := 0;
end;

destructor TUserStream.Destroy;
begin

  inherited;
end;

function TUserStream.GetSize: Int64;
begin
  Size := 64 * 1024 * 1024;
end;

function TUserStream.Read(var Buffer; Count: Integer): Longint;
begin
  if (Count + FPosition) > 1024 * 1024 then Count := 1024 * 1024 - FPosition;

  Result := Count;
  FPosition := FPosition + Count;
end;

function TUserStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FPosition;
end;

function TUserStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FPosition;
end;

function TUserStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FPosition;
end;

end.
