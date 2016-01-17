object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'MegaCast - '#49688#44053#51088#50857
  ClientHeight = 536
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inline frDeskTopScreen: TfrDeskTopScreen
    Left = 0
    Top = 0
    Width = 701
    Height = 536
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 701
    ExplicitHeight = 536
    inherited ScrollBox: TScrollBox
      Width = 701
      Height = 536
      ExplicitWidth = 701
      ExplicitHeight = 536
    end
  end
end
