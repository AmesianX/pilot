unit _fmMain;

interface

uses
  MegaCastUtils, MegaCastRecorder, MegaCastPlayer, TickCount,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    btStart: TButton;
    ScrollBox: TScrollBox;
    Image: TImage;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
  private
    FMegaCastPlayer : TMegaCastPlayer;
    FMegaCastRecorder : TMegaCastRecorder;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  FMegaCastRecorder.Start;
  Timer.Enabled := true;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FMegaCastPlayer.Stop;
  FMegaCastRecorder.Stop;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FMegaCastPlayer := TMegaCastPlayer.Create(Self);
  FMegaCastRecorder := TMegaCastRecorder.Create(Self);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
var
  Data : pointer;
  Size : integer;
begin
  Timer.Enabled := false;
  try
    while FMegaCastRecorder.GetBlockUnit(Data, Size) do begin
      try
        FMegaCastPlayer.BlockUnitIn(Data, Size);
      finally
        if Data <> nil then FreeMem(Data);
      end;
    end;
  finally
    Timer.Enabled := true;
  end;

  FMegaCastPlayer.AudioSync(TTickCount.Obj.Get);
  if FMegaCastPlayer.GetBitmap(Image.Picture.Bitmap) then Image.Repaint;
end;

end.
