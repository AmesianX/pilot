object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'fmMain'
  ClientHeight = 575
  ClientWidth = 794
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
  object Image: TImage
    Left = 8
    Top = 264
    Width = 105
    Height = 105
    AutoSize = True
  end
  object btStart: TButton
    Left = 334
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btStart'
    TabOrder = 0
    OnClick = btStartClick
  end
  object btStop: TButton
    Left = 334
    Top = 39
    Width = 75
    Height = 25
    Caption = 'btStop'
    TabOrder = 1
    OnClick = btStopClick
  end
  object plCam: TPanel
    Left = 8
    Top = 8
    Width = 320
    Height = 240
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 2
  end
  object moJPeg: TMemo
    Left = 460
    Top = 8
    Width = 326
    Height = 110
    ImeName = 'Microsoft Office IME 2007'
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object moWPeg: TMemo
    Left = 460
    Top = 138
    Width = 326
    Height = 110
    ImeName = 'Microsoft Office IME 2007'
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 12
    Top = 204
  end
end
