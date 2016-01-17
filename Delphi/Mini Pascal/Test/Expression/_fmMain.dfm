object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 609
  ClientWidth = 663
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
    Width = 663
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 248
    ExplicitTop = 304
    ExplicitWidth = 185
    object btCalc: TButton
      Left = 12
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btCalc'
      TabOrder = 0
    end
  end
  object moSrc: TMemo
    Left = 0
    Top = 41
    Width = 663
    Height = 568
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    Lines.Strings = (
      '('
      '1'
      '+'
      '2'
      ')'
      '*'
      '2')
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
