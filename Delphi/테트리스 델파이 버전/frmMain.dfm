object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = #53580#53944#47532#49828
  ClientHeight = 315
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object plGameBoard: TPanel
    Left = 4
    Top = 4
    Width = 120
    Height = 240
    BevelOuter = bvNone
    Color = 5197647
    TabOrder = 0
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 192
    object miStart: TMenuItem
      Caption = 'Start'
      OnClick = miStartClick
    end
  end
end
