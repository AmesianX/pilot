unit CamClient;

interface

uses
  VirtualClient, ValueList, MixedPacket, DataTypes, Option, WebCam,
  JPegEncoder,
  Windows, Classes, Graphics, Jpeg, SysUtils, Controls, ExtCtrls;

type
  TCamClient = class(TComponent)
  private
    FOnCamImage: TCamImageEvent;
    FOnGetNextUserID: TGetNextUserIDEvent;
    FWebCam: TfrWebCam;
    FSendCamTimer: TTimer;
    FUserID: string;
    FIsUserIDSended: Boolean;

    procedure WaitByBandwidth;
    function GetCamData(const AData: Pointer; const ASize: Integer;
      var AUserID: string; var ACamImage: TBitmap): Boolean;
    procedure NeedCamData;
    procedure SendCamData(AJpegStream: TStream);

    procedure sp_SetUserID(AUserID: String);
    procedure sp_NeedCamData(AUserID: String);
    procedure sp_SendCamData(AJpegStream: TStream);

    procedure on_SendCamTimer(Sender: TObject);
  private
    FActiveGetData: Boolean;
    FActiveSendData: Boolean;
    FSocket: TVirtualClient;
    function GetParent: TWinControl;
    procedure SetParent(const Value: TWinControl);
    procedure SetUserID(const Value: string);
    procedure SetActiveGetData(const Value: Boolean);
    procedure SetActiveSendData(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DataIn(const AData: Pointer; const ASize: Integer);

    property ActiveSendData: Boolean read FActiveSendData write SetActiveSendData;
    property ActiveGetData: Boolean read FActiveGetData write SetActiveGetData;

    property Socket: TVirtualClient read FSocket write FSocket;
    property UserID: string read FUserID write SetUserID;
    property Parent: TWinControl read GetParent write SetParent;

    property OnGetNextUserID: TGetNextUserIDEvent read FOnGetNextUserID write FOnGetNextUserID;
    property OnCamImage: TCamImageEvent read FOnCamImage write FOnCamImage;
  end;

implementation

uses
  Global;

{ TCamClient }

procedure TCamClient.WaitByBandwidth;
begin
// Todo:
end;

constructor TCamClient.Create(AOwner: TComponent);
begin
  inherited;

  FIsUserIDSended := False;

  FWebCam := TfrWebCam.Create(Self);

  FSendCamTimer := TTimer.Create(Self);
  FSendCamTimer.Interval := 250;
  FSendCamTimer.Enabled := False;
  FSendCamTimer.OnTimer := on_SendCamTimer;
end;

procedure TCamClient.DataIn(const AData: Pointer; const ASize: Integer);
var
  UserID: string;
  bmCamImage: TBitmap;
begin
  if Assigned(FOnCamImage) then
  begin
    bmCamImage := TBitmap.Create;
    try
      if GetCamData(AData, ASize, UserID, bmCamImage) then
      begin
        FOnCamImage(Self, UserID, bmCamImage);
      end else
      begin
        FOnCamImage(Self, UserID, nil);
      end;
    finally
      bmCamImage.Free;
    end;
  end;

  WaitByBandwidth;
  NeedCamData;
end;

destructor TCamClient.Destroy;
begin
  ActiveSendData := False;
  ActiveGetData := False;

  FSendCamTimer.Free;
  FWebCam.Free;

  inherited;
end;

function TCamClient.GetCamData(const AData: Pointer; const ASize: Integer;
  var AUserID: string; var ACamImage: TBitmap): Boolean;
var
  Packet : TMixedPacket;
  Stream : TMemoryStream;
  JpegImage : TJpegImage;
begin
  Packet := TMixedPacket.Create;
  Stream := TMemoryStream.Create;
  JpegImage := TJPEGImage.Create;
  try
    Packet.SetData(AData, ASize);
    Packet.GetPacket(0, AUserID);

    if Packet.Count = 1 then
    begin
      Result := False;
    end else
    begin
      Packet.GetPacket(1, Stream);
      try
        JpegImage.LoadFromStream(Stream);
        ACamImage.Assign(JpegImage);
        Result := True;
      except
        Result := False;
      end;
    end;
  finally
    JpegImage.Free;
    Packet.Free;
    Stream.Free;
  end;
end;

function TCamClient.GetParent: TWinControl;
begin
  Result := FWebCam.Parent;
end;

procedure TCamClient.on_SendCamTimer(Sender: TObject);
var
  Stream : TMemoryStream;
  Bitmap : TBitmap;
begin
  FSendCamTimer.Enabled := False;
  Stream := TMemoryStream.Create;
  try
    Bitmap := TBitmap.Create;
    try
      if not FWebCam.GetBitmap(Bitmap, 160 , 140) then
        Exit;

      Stream.Position := 0;
      BitmapToJpeg(Bitmap, Stream, 70);
    finally
      Bitmap.Free;
    end;

    SendCamData(Stream);
  finally
    Stream.Free;
    FSendCamTimer.Enabled := True;
  end;
end;

procedure TCamClient.NeedCamData;
var
  NextUserID: string;
begin
  if not FActiveGetData and not Assigned(FOnGetNextUserID) then Exit;

  FOnGetNextUserID(Self, NextUserID);
  sp_SetUserID(UserID);
  sp_NeedCamData(NextUserID);
end;

procedure TCamClient.sp_SendCamData(AJpegStream: TStream);
var
  Packet : TMixedPacket;
  Data : Pointer;
  Size : Integer;
  Command : TValueList;
begin
  Packet := TMixedPacket.Create;
  Command := TValueList.Create;
  try
    Command.Values['Code'] := 'CamData';
    Command.Values['UserID'] := FUserID;

    Packet.AddString(Command.Text);

    if AJpegStream <> nil then
    begin
      AJpegStream.Position := 0;
      Packet.AddStream(AJpegStream);
    end;

    Packet.GetData(Data, Size);

    FSocket.Send(Ord(ptCam), Data, Size);
  finally
    Command.Free;
    FreeMem(Data);
    Packet.Free;
  end;
end;

procedure TCamClient.SendCamData(AJpegStream: TStream);
begin
  sp_SetUserID(UserID);
  sp_SendCamData(AJpegStream);
end;

procedure TCamClient.SetActiveGetData(const Value: Boolean);
begin
  Assert(Assigned(FOnGetNextUserID), 'OnGetNextUserID event handler must be seted.');

  FActiveGetData := Value;
  if FActiveGetData then
    NeedCamData;
end;

procedure TCamClient.SetActiveSendData(const Value: Boolean);
begin
  FActiveSendData := Value;

  if FActiveSendData then
  begin
    FWebCam.Start;
    FSendCamTimer.Enabled := True;
  end else
  begin
    FSendCamTimer.Enabled := False;
    FWebCam.Stop;

    SendCamData(nil);
  end;
end;

procedure TCamClient.SetParent(const Value: TWinControl);
begin
  FWebCam.Parent := Value;
  FWebCam.Align := alClient;
end;

procedure TCamClient.SetUserID(const Value: string);
begin
  if FUserID <> Value then
  begin
    FUserID := Value;
    FIsUserIDSended := False;
  end;
end;

procedure TCamClient.sp_NeedCamData(AUserID: String);
var
  Packet : TMixedPacket;
  Data : Pointer;
  Size : Integer;
  Command : TValueList;
begin
  Packet := TMixedPacket.Create;
  Command := TValueList.Create;
  try
    Command.Values['Code'] := 'NeedCamData';
    Command.Values['UserID'] := AUserID;

    Packet.AddString(Command.Text);

    Packet.GetData(Data, Size);
    FSocket.Send(Ord(ptCam), Data, Size);
  finally
    Command.Free;
    FreeMem(Data);
    Packet.Free;
  end;
end;

procedure TCamClient.sp_SetUserID(AUserID: String);
var
  Packet : TValueList;
begin
  if FIsUserIDSended then Exit;
  if Trim(AUserID) = '' then Exit;

  Packet := TValueList.Create;
  try
    Packet.Values['Code'] := 'SetUserID';
    Packet.Values['UserID'] := AUserID;
    FSocket.Send(Ord(ptCommand), Packet.Text);
  finally
    Packet.Free;
  end;

  FIsUserIDSended := True;
end;

end.
