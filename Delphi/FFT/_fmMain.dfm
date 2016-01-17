object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 626
  ClientWidth = 968
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btForwardFFT: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btForwardFFT'
    TabOrder = 0
    OnClick = btForwardFFTClick
  end
  object btInverseFFT: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btInverseFFT'
    TabOrder = 1
    OnClick = btInverseFFTClick
  end
end
