object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 542
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 639
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btStart: TButton
      Left = 15
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btStart'
      TabOrder = 0
      OnClick = btStartClick
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 41
    Width = 639
    Height = 501
    Align = alClient
    TabOrder = 1
    object Image: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 112
    Top = 264
  end
end
