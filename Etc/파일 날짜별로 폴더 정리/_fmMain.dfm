object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = #54028#51068' '#45216#51676#48324#47196' '#54260#45908' '#51221#47532
  ClientHeight = 290
  ClientWidth = 554
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
    Width = 554
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btStart: TButton
      Left = 8
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
    Width = 554
    Height = 249
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
