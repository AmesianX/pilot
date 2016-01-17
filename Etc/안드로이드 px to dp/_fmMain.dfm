object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'px to dp'
  ClientHeight = 562
  ClientWidth = 784
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 324
    ExplicitTop = 312
    ExplicitWidth = 185
    object cbDensity: TComboBox
      Left = 8
      Top = 11
      Width = 145
      Height = 21
      ImeName = 'Microsoft IME 2010'
      TabOrder = 0
      Text = 'High density'
      OnChange = cbDensityChange
      OnKeyDown = cbDensityKeyDown
      OnKeyPress = cbDensityKeyPress
      OnKeyUp = cbDensityKeyUp
      Items.Strings = (
        'Low density'
        'Medium density'
        'High density'
        'Extra High density'
        'Double Extra High density'
        'Triple Extra High density')
    end
    object btConvert: TButton
      Left = 159
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Convert'
      TabOrder = 1
      OnClick = btConvertClick
    end
  end
  object moIn: TMemo
    Left = 0
    Top = 41
    Width = 784
    Height = 521
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    PopupMenu = PopupMenu
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitTop = 40
    ExplicitWidth = 786
    ExplicitHeight = 654
  end
  object PopupMenu: TPopupMenu
    AutoHotkeys = maManual
    Left = 388
    Top = 352
    object miSelectAll: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = miSelectAllClick
    end
  end
end
