unit _frRecordSize;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrRecordSize = class(TFrame)
    SBoxDefaultSize      : TScrollBox;
    PanelDefaultSize     : TPanel;
    RBtn_DS_1280x720     : TRadioButton;
    RBtn_DS_480x320      : TRadioButton;
    RBtn_DS_320x240      : TRadioButton;
    RBtn_DS_640x480      : TRadioButton;
    RBtn_DS_1024x768     : TRadioButton;
    RBtn_DS_720x480      : TRadioButton;
    RBtn_DS_800x600      : TRadioButton;
    Panel                : TPanel;
    RBtnDefaultSize      : TRadioButton;
    RBtnMobileSize       : TRadioButton;
    RBtnRatio            : TRadioButton;
    SBoxMobileSize       : TScrollBox;
    PanelRatio           : TPanel;
    RBtn_16_9            : TRadioButton;
    RBtn_4_3             : TRadioButton;
    RBtn_16_10           : TRadioButton;
    PanelMobileSize      : TPanel;
    RBtn_Atree           : TRadioButton;
    RBtn_Cyon            : TRadioButton;
    RBtn_Samsung         : TRadioButton;
    RBtn_Sony            : TRadioButton;
    RBtn_Apple           : TRadioButton;
    RBtn_Iriver          : TRadioButton;
    RBtn_Anycall         : TRadioButton;
    RBtn_Cowon           : TRadioButton;
    Panel_MS_Samsung     : TPanel;
    RBtn_Samsung_480x272 : TRadioButton;
    Panel_MS_Cyon        : TPanel;
    RBtn_Cyon_320x240    : TRadioButton;
    Panel_MS_Sony        : TPanel;
    RBtn_Sony_368x208    : TRadioButton;
    RBtn_Sony_320x240    : TRadioButton;
    RBtn_Sony_480x272    : TRadioButton;
    Panel_MS_Iriver      : TPanel;
    RBtn_Iriver_320x240  : TRadioButton;
    RBtn_Iriver_480x272  : TRadioButton;
    Panel_MS_Anycall     : TPanel;
    RBtn_Anycall_720x400 : TRadioButton;
    RBtn_Anycall_640x360 : TRadioButton;
    Panel_MS_Apple       : TPanel;
    RBtn_Apple_480x320   : TRadioButton;
    Panel_MS_Atree       : TPanel;
    RBtn_Atree_480x272   : TRadioButton;
    Panel_MS_Cowon       : TPanel;
    RBtn_Cowon_480x272   : TRadioButton;
    RBtn_Cowon_320x240   : TRadioButton;
    RBtn_Cowon_624x352   : TRadioButton;
    RBtn_Sony_720x480    : TRadioButton;
    procedure RBtnDefaultSizeClick(Sender: TObject);
    procedure RBtnMobileSizeClick(Sender: TObject);
    procedure RBtnRatioClick(Sender: TObject);
    procedure RBtn_SamsungClick(Sender: TObject);
    procedure RBtn_CyonClick(Sender: TObject);
    procedure RBtn_SonyClick(Sender: TObject);
    procedure RBtn_IriverClick(Sender: TObject);
    procedure RBtn_AnycallClick(Sender: TObject);
    procedure RBtn_AppleClick(Sender: TObject);
    procedure RBtn_AtreeClick(Sender: TObject);
    procedure RBtn_CowonClick(Sender: TObject);
  private
  public
    procedure HideAllMobilePanel(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TfrRecordSize.HideAllMobilePanel(Sender: TObject);
begin
  //    MS -> Mobile Size
  Panel_MS_Samsung.Visible := False;
  Panel_MS_Cyon.Visible    := False;
  Panel_MS_Sony.Visible    := False;
  Panel_MS_Iriver.Visible  := False;
  Panel_MS_Anycall.Visible := False;
  Panel_MS_Apple.Visible   := False;
  Panel_MS_Atree.Visible   := False;
  Panel_MS_Cowon.Visible   := False;
end;

procedure TfrRecordSize.RBtnDefaultSizeClick(Sender: TObject);
begin
  SBoxDefaultSize.Visible := True;
  SBoxMobileSize.Visible  := False;
  PanelRatio.Visible      := False;
  HideAllMobilePanel(Sender);

  RBtn_DS_320x240.Checked := True;
  SBoxDefaultSize.VertScrollBar.Position := 0;
end;

procedure TfrRecordSize.RBtnMobileSizeClick(Sender: TObject);
begin
  SBoxDefaultSize.Visible := False;
  SBoxMobileSize.Visible  := True;
  PanelRatio.Visible      := False;

  RBtn_Samsung.Checked    := True;
  SBoxMobileSize.VertScrollBar.Position := 0;

  RBtn_SamsungClick(Sender);
end;

procedure TfrRecordSize.RBtnRatioClick(Sender: TObject);
begin
  SBoxDefaultSize.Visible := False;
  SBoxMobileSize.Visible  := False;
  PanelRatio.Visible      := True;
  HideAllMobilePanel(Sender);

  RBtn_4_3.Checked        := True;
end;

procedure TfrRecordSize.RBtn_AnycallClick(Sender: TObject);
begin
  HideAllMobilePanel(Sender);
  Panel_MS_Anycall.Visible     := True;
  RBtn_Anycall_640x360.Checked := True;
end;

procedure TfrRecordSize.RBtn_AppleClick(Sender: TObject);
begin
  HideAllMobilePanel(Sender);
  Panel_MS_Apple.Visible     := True;
  RBtn_Apple_480x320.Checked := True;
end;

procedure TfrRecordSize.RBtn_AtreeClick(Sender: TObject);
begin
  HideAllMobilePanel(Sender);
  Panel_MS_Atree.Visible     := True;
  RBtn_Atree_480x272.Checked := True;
end;

procedure TfrRecordSize.RBtn_CowonClick(Sender: TObject);
begin
  HideAllMobilePanel(Sender);
  Panel_MS_Cowon.Visible     := True;
  RBtn_Cowon_320x240.Checked := True;
end;

procedure TfrRecordSize.RBtn_CyonClick(Sender: TObject);
begin
  HideAllMobilePanel(Sender);
  Panel_MS_Cyon.Visible     := True;
  RBtn_Cyon_320x240.Checked := True;
end;

procedure TfrRecordSize.RBtn_IriverClick(Sender: TObject);
begin
  HideAllMobilePanel(Sender);
  Panel_MS_Iriver.Visible     := True;
  RBtn_Iriver_320x240.Checked := True;
end;

procedure TfrRecordSize.RBtn_SamsungClick(Sender: TObject);
begin
  HideAllMobilePanel(Sender);
  Panel_MS_Samsung.Visible     := True;
  RBtn_Samsung_480x272.Checked := True;
end;

procedure TfrRecordSize.RBtn_SonyClick(Sender: TObject);
begin
  HideAllMobilePanel(Sender);
  Panel_MS_Sony.Visible     := True;
  RBtn_Sony_320x240.Checked := True;
end;

end.
