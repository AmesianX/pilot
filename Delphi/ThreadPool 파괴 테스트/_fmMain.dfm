object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 245
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btRun: TButton
    Left = 389
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btRun'
    TabOrder = 0
    OnClick = btRunClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 173
    Height = 141
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
end
