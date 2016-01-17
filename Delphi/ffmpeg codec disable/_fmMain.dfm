object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 412
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btEncoderList: TButton
    Left = 544
    Top = 24
    Width = 75
    Height = 25
    Caption = 'btEncoderList'
    TabOrder = 0
    OnClick = btEncoderListClick
  end
  object moMsg: TMemo
    Left = 24
    Top = 16
    Width = 481
    Height = 369
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      'moMsg')
    TabOrder = 1
  end
  object btDecoder: TButton
    Left = 544
    Top = 55
    Width = 75
    Height = 25
    Caption = 'btDecoder'
    TabOrder = 2
    OnClick = btDecoderClick
  end
  object btMux: TButton
    Left = 544
    Top = 86
    Width = 75
    Height = 25
    Caption = 'btMux'
    TabOrder = 3
    OnClick = btMuxClick
  end
  object btDeMux: TButton
    Left = 544
    Top = 117
    Width = 75
    Height = 25
    Caption = 'btDeMux'
    TabOrder = 4
    OnClick = btDeMuxClick
  end
  object Button1: TButton
    Left = 544
    Top = 148
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 5
    OnClick = Button1Click
  end
end
