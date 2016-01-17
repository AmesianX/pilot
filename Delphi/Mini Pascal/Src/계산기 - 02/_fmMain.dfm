object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = #44228#49328#44592
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object moMsg: TMemo
    Left = 0
    Top = 0
    Width = 554
    Height = 290
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyPress = moMsgKeyPress
  end
end
