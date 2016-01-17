object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 530
  ClientWidth = 726
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnToolBar: TPanel
    Left = 0
    Top = 0
    Width = 726
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      726
      41)
    object BtnReflash: TSpeedButton
      Left = 663
      Top = 11
      Width = 26
      Height = 21
      Anchors = [akTop, akRight]
      Caption = #174
      OnClick = BtnReflashClick
    end
    object BtnOut: TSpeedButton
      Left = 695
      Top = 11
      Width = 23
      Height = 21
      Anchors = [akTop, akRight]
      Caption = #8593
      OnClick = BtnOutClick
    end
    object cbAppList: TJvComboBox
      Left = 8
      Top = 11
      Width = 649
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ImeName = 'Microsoft Office IME 2007'
      ItemHeight = 13
      ParentFlat = False
      TabOrder = 0
      OnChange = cbAppListChange
    end
  end
  object pnScreen: TPanel
    Left = 0
    Top = 41
    Width = 726
    Height = 489
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
