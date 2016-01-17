object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 202
  ClientWidth = 304
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 304
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 64
    ExplicitTop = 56
    ExplicitWidth = 185
    object btCapture: TButton
      Left = 4
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Capture'
      TabOrder = 0
      OnClick = btCaptureClick
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 41
    Width = 304
    Height = 161
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 84
    ExplicitTop = 76
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Image: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
end
