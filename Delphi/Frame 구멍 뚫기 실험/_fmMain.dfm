object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 559
  ClientWidth = 701
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btMakeHole: TSpeedButton
    Left = 528
    Top = 167
    Width = 75
    Height = 22
    Caption = 'MakeHole'
    OnClick = btMakeHoleClick
  end
  inline frMain: TfrMain
    Left = 92
    Top = 76
    Width = 320
    Height = 240
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    Visible = False
    ExplicitLeft = 92
    ExplicitTop = 76
  end
  object Edit1: TEdit
    Left = 76
    Top = 376
    Width = 121
    Height = 21
    ImeName = 'Microsoft IME 2010'
    TabOrder = 1
    Text = 'Edit1'
  end
end
