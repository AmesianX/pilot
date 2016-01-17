object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 728
  ClientWidth = 605
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 605
    Height = 41
    Align = alTop
    TabOrder = 0
    object Edit1: TEdit
      Left = 170
      Top = 10
      Width = 415
      Height = 21
      ImeName = 'Microsoft IME 2003'
      TabOrder = 0
      Text = 'Edit1'
      OnKeyPress = Edit1KeyPress
    end
    object BitBtn1: TBitBtn
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'BitBtn1'
      Enabled = False
      TabOrder = 1
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Caption = 'BitBtn2'
      Enabled = False
      TabOrder = 2
      OnClick = BitBtn2Click
    end
  end
  object WebBrowser1: TWebBrowser
    Left = 0
    Top = 41
    Width = 605
    Height = 687
    Align = alClient
    TabOrder = 1
    OnCommandStateChange = WebBrowser1CommandStateChange
    ExplicitLeft = 224
    ExplicitTop = 144
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C000000873E0000014700000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
