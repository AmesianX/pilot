object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 290
  ClientWidth = 554
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
  object bt1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'bt1'
    TabOrder = 0
    OnClick = bt1Click
  end
  object dlgOpen: TOpenDialog
    Filter = 'All files|*.*'
    Left = 108
    Top = 20
  end
end
