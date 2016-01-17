object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 301
  ClientWidth = 467
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
  object btInit: TButton
    Left = 372
    Top = 36
    Width = 75
    Height = 25
    Caption = 'btInit'
    TabOrder = 0
    OnClick = btInitClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 12
    Width = 317
    Height = 265
    ImeName = 'Microsoft IME 2003'
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object btGetRndNo: TButton
    Left = 372
    Top = 67
    Width = 75
    Height = 25
    Caption = 'btGetRndNo'
    TabOrder = 2
    OnClick = btGetRndNoClick
  end
end
