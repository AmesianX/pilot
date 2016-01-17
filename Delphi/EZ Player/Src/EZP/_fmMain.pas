unit _fmMain;

interface

uses
  RyuMPEG, VideoDecoder, AudioDecoder,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btOpen: TButton;
    OpenDialog: TOpenDialog;
    btPlay: TButton;
    ScrollBox: TScrollBox;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure btPlayClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FStream : TRyuMPEGStream;
    FVideoDecoder : TVideoDecoder;
    FAudioDecoder : TAudioDecoder;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    if FStream.Open(OpenDialog.FileName) then begin
      FVideoDecoder.Open(FStream);
      FAudioDecoder.Open(FStream);
    end;
  end;
end;

procedure TfmMain.btPlayClick(Sender: TObject);
begin
  while True do begin
    if FAudioDecoder.IsBusy then begin
      Application.ProcessMessages;
      Continue;
    end;

    if not FStream.ReadPacket then Exit;

    case FStream.PacketType of
      VIDEO_PACKET:
        if FVideoDecoder.Decode(FStream.PacketBuffer, FStream.PacketSize) then begin
          Image.Picture.Bitmap.Assign(FVideoDecoder.Bitmap);
          Image.Repaint;
        end;

      AUDIO_PACKET: FAudioDecoder.DataIn(FStream.PacketBuffer, FStream.PacketSize);
    end;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.PixelFormat := pf32bit;

  FStream := TRyuMPEGStream.Create;
  FVideoDecoder := TVideoDecoder.Create;
  FAudioDecoder := TAudioDecoder.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FStream);
  FreeAndNil(FVideoDecoder);
  FreeAndNil(FAudioDecoder);
end;

end.
