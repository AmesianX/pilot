unit _fmMain;

interface

uses
  RyuMPEG,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

const
  READ_BUFFER_SIZE = 16 * 1024 * 1024;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    Panel1: TPanel;
    btOpen: TButton;
    OpenDialog: TOpenDialog;
    btReadPacket: TButton;
    ScrollBox: TScrollBox;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure btReadPacketClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHandle : pointer;
    FReadBuffer : pointer;
    FVideoBuffer : pointer;
    procedure open_File(AFileName:AnsiString);
    procedure close_File;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then open_File(OpenDialog.FileName);
end;

procedure TfmMain.btReadPacketClick(Sender: TObject);
var
  DataSize, PacketType, Postion : integer;
begin
  while True do begin
    if not readData(FHandle, FReadBuffer, DataSize, PacketType, Postion) then Exit;

    moMsg.Lines.Add(Format('DataSize: %d, PacketType: %d, Postion: %d', [DataSize, PacketType, Postion]));

    if decodeVideoData(FHandle, FReadBuffer, DataSize, FVideoBuffer) then begin
      BitmapUpsideDown(FVideoBuffer, Image.Picture.Bitmap);
      Image.Repaint;

      Break;
    end;
  end;
end;

procedure TfmMain.close_File;
begin
  if FHandle <> nil then begin
    closeRyuMPEG(FHandle);
    FHandle := nil;
  end;

  if FVideoBuffer <> nil then begin
    FreeMem(FVideoBuffer);
    FVideoBuffer := nil;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FHandle := nil;
  GetMem(FReadBuffer, READ_BUFFER_SIZE);

  FVideoBuffer := nil;

  Image.Picture.Bitmap.PixelFormat := pf32bit;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeMem(FReadBuffer);

  if FVideoBuffer <> nil then FreeMem(FVideoBuffer);
end;

procedure TfmMain.open_File(AFileName: AnsiString);
var
  iErrorCode : integer;
  MediaInfo : TMediaInfo;
begin
  close_File;
  FHandle := openRyuMPEG(PAnsiChar(AFileName), iErrorCode);

  if iErrorCode = 0 then begin
    getMediaInfo(FHandle, MediaInfo);
    moMsg.Lines.Add(StrPas(MediaInfo.VideoInfoString));
    moMsg.Lines.Add(StrPas(MediaInfo.AudioInfoString));
    moMsg.Lines.Add(Format('Duration: %d', [MediaInfo.Duration]));

    Image.Picture.Bitmap.Width  := MediaInfo.VideoInfo.Width;
    Image.Picture.Bitmap.Height := MediaInfo.VideoInfo.Height;

    GetMem(FVideoBuffer, MediaInfo.VideoInfo.Width * MediaInfo.VideoInfo.Height * PIXEL_SIZE);
  end else begin
    FHandle := nil;
  end;
end;

end.
