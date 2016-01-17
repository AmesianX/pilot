object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 475
  ClientWidth = 726
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 32
    Top = 24
    Width = 75
    Height = 25
    Caption = #49884#51089
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 32
    Top = 64
    Width = 75
    Height = 25
    Caption = #51333#47308
    TabOrder = 1
    OnClick = Button2Click
  end
  inline frCam: TfrCam
    Left = 150
    Top = 50
    Width = 483
    Height = 239
    Color = clBlack
    ParentBackground = False
    ParentColor = False
    TabOrder = 2
    ExplicitLeft = 150
    ExplicitTop = 50
    ExplicitWidth = 483
    ExplicitHeight = 239
    inherited VideoGrabber: TVideoGrabber
      Width = 483
      Height = 239
    end
  end
  object Button3: TButton
    Left = 32
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Device'#44079#49688
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 32
    Top = 144
    Width = 75
    Height = 25
    Caption = 'DeviceName'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 32
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Bitmap'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 32
    Top = 215
    Width = 75
    Height = 25
    Caption = 'Jpeg'
    TabOrder = 6
    OnClick = Button6Click
  end
end
