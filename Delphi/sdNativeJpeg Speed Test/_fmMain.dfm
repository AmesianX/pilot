object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnIJL: TButton
    Left = 464
    Top = 8
    Width = 75
    Height = 25
    Caption = 'IJL'
    TabOrder = 0
    OnClick = BtnIJLClick
  end
  object BtnNativeJpeg: TButton
    Left = 464
    Top = 39
    Width = 75
    Height = 25
    Caption = 'sdNativeJpeg'
    TabOrder = 1
    OnClick = BtnNativeJpegClick
  end
  object JvFilenameEdit1: TJvFilenameEdit
    Left = 8
    Top = 8
    Width = 433
    Height = 21
    TabOrder = 2
  end
end
