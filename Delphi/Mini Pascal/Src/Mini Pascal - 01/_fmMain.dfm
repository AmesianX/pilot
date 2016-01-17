object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 612
  ClientWidth = 714
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
    Width = 714
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 328
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 714
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
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
    Width = 714
    Height = 228
    Align = alTop
    ImeName = 'Microsoft Office IME 2007'
    Lines.Strings = (
      'program My_First_Program;'
      ''
      'repeat'
      '  Go 5;'
      '  Turn Left;'
      '  Go 5;'
      'for 4 times;'
      ''
      'end.')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object moOut: TMemo
    Left = 0
    Top = 272
    Width = 714
    Height = 340
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
