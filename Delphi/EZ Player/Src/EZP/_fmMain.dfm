object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 524
  ClientWidth = 716
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
    Width = 716
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btOpen: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btOpenClick
    end
    object btPlay: TButton
      Left = 89
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Play'
      TabOrder = 1
      OnClick = btPlayClick
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 41
    Width = 716
    Height = 483
    Align = alClient
    TabOrder = 1
    ExplicitTop = 213
    ExplicitHeight = 311
    object Image: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All file(s)|*.*'
    Left = 272
    Top = 152
  end
end
