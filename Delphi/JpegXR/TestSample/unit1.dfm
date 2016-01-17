object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 407
  ClientWidth = 579
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 327
    Top = 13
    Width = 51
    Height = 13
    Caption = #48152#48373' '#54924#49688
  end
  object Label2: TLabel
    Left = 329
    Top = 83
    Width = 99
    Height = 13
    Caption = 'Jpeg '#54408#51656' (Max100)'
  end
  object Label3: TLabel
    Left = 329
    Top = 121
    Width = 80
    Height = 13
    Caption = 'XR '#54408#51656' (Max 1)'
  end
  object Label4: TLabel
    Left = 8
    Top = 356
    Width = 563
    Height = 13
    AutoSize = False
    Caption = 'File'#50630#51020
  end
  object Button1: TButton
    Left = 351
    Top = 208
    Width = 75
    Height = 25
    Caption = 'IJL'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 351
    Top = 256
    Width = 75
    Height = 25
    Caption = 'XR'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 351
    Top = 304
    Width = 75
    Height = 25
    Caption = 'All'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 305
    Height = 297
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 3
  end
  object SpinEdit1: TSpinEdit
    Left = 327
    Top = 32
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 10
    OnChange = SpinEdit1Change
  end
  object CheckBox1: TCheckBox
    Left = 329
    Top = 60
    Width = 97
    Height = 17
    Caption = #54028#51068#47196' '#51200#51109
    TabOrder = 5
  end
  object SpinEdit2: TSpinEdit
    Left = 329
    Top = 96
    Width = 121
    Height = 22
    MaxValue = 100
    MinValue = 1
    TabOrder = 6
    Value = 100
    OnChange = SpinEdit1Change
  end
  object SpinEdit3: TSpinEdit
    Left = 329
    Top = 140
    Width = 121
    Height = 22
    MaxValue = 255
    MinValue = 1
    TabOrder = 7
    Value = 1
    OnChange = SpinEdit1Change
  end
  object Button4: TButton
    Left = 351
    Top = 168
    Width = 75
    Height = 25
    Caption = 'File'#50676#44592
    TabOrder = 8
    OnClick = Button4Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'BMP|*.bmp'
    Left = 16
    Top = 16
  end
end
