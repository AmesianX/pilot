object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 597
  ClientWidth = 643
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
  object Splitter1: TSplitter
    Left = 0
    Top = 269
    Width = 643
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 328
  end
  object ListBox: TListBox
    Left = 0
    Top = 272
    Width = 643
    Height = 325
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 643
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btStart: TButton
      Left = 12
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btStart'
      TabOrder = 0
      OnClick = btStartClick
    end
  end
  object moSrc: TMemo
    Left = 0
    Top = 41
    Width = 643
    Height = 228
    Align = alTop
    ImeName = 'Microsoft Office IME 2007'
    Lines.Strings = (
      'if (a = b) then'
      '  if (b = c) then imp'
      '  else imp;'
      ''
      '123'
      'a123'
      '123.123'
      '123.123.123'
      '123E123'
      '123E-123'
      '123E+123'
      '123.123E123'
      '123.123E-123'
      '123.123E+123')
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
