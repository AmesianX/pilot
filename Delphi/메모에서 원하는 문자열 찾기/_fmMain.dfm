object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 293
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Memo1: TMemo
    Left = 16
    Top = 8
    Width = 301
    Height = 261
    ImeName = 'Microsoft IME 2003'
    Lines.Strings = (
      'Memo1'
      'Hi !!Ryu!!  hm...'
      'dfsdf')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 328
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
end
