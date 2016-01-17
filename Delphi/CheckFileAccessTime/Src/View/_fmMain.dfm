object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = #54028#51068' '#47004#45924' '#51217#44540' '#54217#44512' '#49549#46020' '#44396#54616#44592
  ClientHeight = 525
  ClientWidth = 903
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
  object Label1: TLabel
    Left = 97
    Top = 99
    Width = 142
    Height = 14
    Caption = 'MB '#47196' Data '#54028#51068' '#47564#46308#44592
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnMakeDataFile: TButton
    Left = 48
    Top = 40
    Width = 193
    Height = 41
    Caption = #51076#51032#51032' Data '#54028#51068' '#49373#49457#54616#44592
    TabOrder = 0
    OnClick = btnMakeDataFileClick
  end
  object Memo1: TMemo
    Left = 48
    Top = 320
    Width = 193
    Height = 57
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 1
  end
  object Memo3: TMemo
    Left = 304
    Top = 40
    Width = 273
    Height = 337
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 2
  end
  object mmDisplay: TMemo
    Left = 48
    Top = 123
    Width = 193
    Height = 89
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 3
  end
  object btnFileRead: TButton
    Left = 117
    Top = 264
    Width = 124
    Height = 41
    Caption = #48264' '#51069#44256' '#49884#44036' '#44228#49328#54616#44592
    Enabled = False
    TabOrder = 4
    OnClick = btnFileReadClick
  end
  object Button1: TButton
    Left = 48
    Top = 416
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Edit: TEdit
    Left = 48
    Top = 274
    Width = 63
    Height = 21
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 6
    Text = '1000'
  end
  object Memo2: TMemo
    Left = 632
    Top = 40
    Width = 185
    Height = 337
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 7
  end
  object chBox: TCheckBox
    Left = 632
    Top = 17
    Width = 161
    Height = 17
    Caption = #47004#45924#51004#47196' '#51069#45716' '#44284#51221' '#48372#44592' '
    TabOrder = 8
  end
  object edDataSize: TEdit
    Left = 54
    Top = 97
    Width = 35
    Height = 20
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 9
    Text = '50'
  end
end
