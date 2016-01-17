object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'DeskCam Sender'
  ClientHeight = 115
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu: TMainMenu
    AutoHotkeys = maManual
    Left = 192
    Top = 64
    object N1: TMenuItem
      Caption = #47196#44536
      object mi_ViewLog: TMenuItem
        Caption = #47196#44536#48372#44592
        OnClick = mi_ViewLogClick
      end
    end
  end
end
