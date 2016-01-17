object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 337
  ClientWidth = 635
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
  object Btn1List: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Btn1List'
    TabOrder = 0
    OnClick = Btn1ListClick
  end
  object BtnMultiList: TButton
    Left = 24
    Top = 72
    Width = 75
    Height = 25
    Caption = 'BtnMultiList'
    TabOrder = 1
    OnClick = BtnMultiListClick
  end
  object Button1: TButton
    Left = 24
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
end
