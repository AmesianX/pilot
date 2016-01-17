object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = #51204#51088#47700#51068' '#52628#52636
  ClientHeight = 576
  ClientWidth = 768
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 768
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btStart: TButton
      Left = 12
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = btStartClick
    end
  end
  object moMsg: TMemo
    Left = 0
    Top = 41
    Width = 768
    Height = 535
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
