unit _frPlayer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, lImageButton, ExtCtrls, PngImage, PlayerTrackBar,
  JvExControls, JvLabel, StdCtrls;

type
  TStatusChangeEvent = procedure (Sender : TObject; Playing : Boolean) of Object;

  TfrPlayer = class(TFrame)
    ControlPanel: TPanel;
    BtnSmallPlay: TlImageButton;
    PlayerTrackBar: TPlayerTrackBar;
    lblTime: TJvLabel;
    VolumeTrackBar: TPlayerTrackBar;
    BtnSmallPause: TlImageButton;
    BtnBigPause: TlImageButton;
    BtnBigPlay: TlImageButton;
    ImgScreen: TImage;
    BtnVolume: TlImageButton;
    ImgControlPanelBackground: TImage;
    procedure FrameResize(Sender: TObject);
    procedure BtnVolumeClick(Sender: TObject);
    procedure BtnPauseMouseDown(Sender: TObject);
    procedure BtnPlayMouseDown(Sender: TObject);
    procedure BtnSmallPlayMouseDown(Sender: TObject);
    procedure BtnSmallPauseMouseDown(Sender: TObject);
  private
    FPlaying: boolean;
    FOnStatusChange: TStatusChangeEvent;
    procedure SetPlaying(const Value: boolean);
    procedure SetOnStatusChange(const Value: TStatusChangeEvent);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Playing : boolean read FPlaying write SetPlaying;
    property OnStatusChange : TStatusChangeEvent read FOnStatusChange write SetOnStatusChange;
    property Screen : TImage read ImgScreen;
  end;

implementation

{$R *.dfm}

procedure TfrPlayer.BtnPlayMouseDown(Sender: TObject);
begin
  Playing := true;
//이미지 갱신용
  BtnBigPause.Enter;
  BtnBigPlay.Enter;
end;

procedure TfrPlayer.BtnSmallPauseMouseDown(Sender: TObject);
begin
  Playing := false;
end;

procedure TfrPlayer.BtnSmallPlayMouseDown(Sender: TObject);
begin
  Playing := true;
end;

procedure TfrPlayer.BtnPauseMouseDown(Sender: TObject);
begin
  Playing := false;
//이미지 갱신용
  BtnBigPause.Enter;
  BtnBigPlay.Enter;
end;

procedure TfrPlayer.BtnVolumeClick(Sender: TObject);
begin
  VolumeTrackBar.Visible := not VolumeTrackBar.Visible;
end;

constructor TfrPlayer.Create(AOwner: TComponent);
begin
  inherited;

  FPlaying := false;
  FOnStatusChange := nil;
end;

procedure TfrPlayer.FrameResize(Sender: TObject);
begin
  VolumeTrackBar.Top := ControlPanel.Top - VolumeTrackBar.Height;
  VolumeTrackBar.Left := Width - VolumeTrackBar.Width;

  BtnBigPause.Left := (Width - BtnBigPause.Width) div 2;
  BtnBigPause.Top := ((Height - ControlPanel.Height)  - BtnBigPause.Height) div 2;
  BtnBigPlay.Left := (Width - BtnBigPlay.Width) div 2;
  BtnBigPlay.Top := ((Height - ControlPanel.Height)  - BtnBigPlay.Height) div 2;

  BtnSmallPlay.Left := 0;
  //BtnSmallPlay.Top := 0;

  BtnVolume.Top := 0;
  BtnVolume.Left := Width - BtnVolume.Width;

  LblTime.Top := 0;
  LblTime.Left := BtnVolume.Left - LblTime.Width;

  PlayerTrackBar.Left := BtnSmallPlay.Width;
  PlayerTrackBar.Width := ControlPanel.Width - BtnVolume.Width - lblTime.Width - BtnSmallPlay.Width - 5;
end;

procedure TfrPlayer.SetOnStatusChange(const Value: TStatusChangeEvent);
begin
  FOnStatusChange := Value;
end;

procedure TfrPlayer.SetPlaying(const Value: boolean);
begin
  FPlaying := Value;

  BtnSmallPlay.Visible := not Value;
  BtnSmallPause.Visible := Value;
  BtnBigPlay.Visible := not Value;
  BtnBigPause.Visible := Value;

  if Assigned(FOnStatusChange) then FOnStatusChange(Self, FPlaying);  
end;

end.
