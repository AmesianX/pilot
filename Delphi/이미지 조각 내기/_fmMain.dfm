object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 550
  ClientWidth = 823
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 580
    Top = 41
    Height = 509
    Align = alRight
    ExplicitLeft = 332
    ExplicitTop = 288
    ExplicitHeight = 100
  end
  object pl1: TPanel
    Left = 0
    Top = 0
    Width = 823
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 376
    ExplicitTop = 288
    ExplicitWidth = 185
    object btOpen: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btOpen'
      TabOrder = 0
      OnClick = btOpenClick
    end
  end
  object pl2: TPanel
    Left = 583
    Top = 41
    Width = 240
    Height = 509
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 544
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 41
    Width = 580
    Height = 509
    Align = alClient
    TabOrder = 2
    object imgMain: TImage
      Left = 0
      Top = 0
      Width = 32
      Height = 32
      AutoSize = True
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Image file (png, bmp, jpg)|*.png|*.bmp|*.jpg'
    Left = 404
    Top = 280
  end
end
