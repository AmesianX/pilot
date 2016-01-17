object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 530
  ClientWidth = 648
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
    Width = 648
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btCapture: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Caption = 'btCapture'
      TabOrder = 0
      OnClick = btCaptureClick
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 41
    Width = 648
    Height = 489
    Align = alClient
    TabOrder = 1
    object Image: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
end
