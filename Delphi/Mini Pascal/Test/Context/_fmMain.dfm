object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 616
  ClientWidth = 654
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
  object Splitter1: TSplitter
    Left = 0
    Top = 269
    Width = 654
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 328
  end
  object ListBox: TListBox
    Left = 0
    Top = 272
    Width = 654
    Height = 344
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    ItemHeight = 13
    TabOrder = 0
    ExplicitWidth = 643
    ExplicitHeight = 325
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 654
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 643
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
    Width = 654
    Height = 228
    Align = alTop
    ImeName = 'Microsoft Office IME 2007'
    Lines.Strings = (
      'if (a = b) then'
      '  if (b = c) then imp'
      '  else imp;')
    ScrollBars = ssBoth
    TabOrder = 2
    ExplicitWidth = 643
  end
end
