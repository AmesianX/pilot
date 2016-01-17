object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'BitmapOutput'
  ClientHeight = 290
  ClientWidth = 554
  Color = clTeal
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object btOpen: TButton
    Left = 20
    Top = 20
    Width = 75
    Height = 25
    Caption = 'btOpen'
    TabOrder = 0
    OnClick = btOpenClick
  end
  object Button1: TButton
    Left = 20
    Top = 51
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Timer: TTimer
    Enabled = False
    Interval = 25
    Left = 272
    Top = 152
  end
end
