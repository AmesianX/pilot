object fmmsSelector: TfmmsSelector
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'fmmsSelector'
  ClientHeight = 168
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 129
    Height = 13
    AutoSize = False
    Caption = #52712#49548#45716' ESC '#47484' '#45572#47476#49464#50836
  end
  object BtnSelect: TlImageButton
    Left = 16
    Top = 48
    Width = 100
    Height = 32
    GroupIndex = 0
    UseDownButton = False
    OnMouseDown = BtnSelectClick
    OnClick = BtnSelectClick
    TransParentsColor = clBlack
    Down = False
    Caption = #49440#53469
  end
  object BtnCancel: TlImageButton
    Left = 16
    Top = 91
    Width = 100
    Height = 27
    GroupIndex = 0
    UseDownButton = False
    OnMouseDown = BtnCancelClick
    OnClick = BtnCancelClick
    TransParentsColor = clBlack
    Down = False
    Caption = #52712#49548
  end
  object LblMonitorNum: TLabel
    Left = 136
    Top = 48
    Width = 200
    Height = 13
    AutoSize = False
    Caption = 'N'#48264' '#47784#45768#53552
  end
  object lblMonitorRect: TLabel
    Left = 136
    Top = 67
    Width = 200
    Height = 13
    AutoSize = False
    Caption = 'Rect'
  end
  object lblWidth: TLabel
    Left = 136
    Top = 86
    Width = 200
    Height = 13
    AutoSize = False
    Caption = 'Width'
  end
  object lblHeight: TLabel
    Left = 136
    Top = 105
    Width = 200
    Height = 13
    AutoSize = False
    Caption = 'Height'
  end
end
