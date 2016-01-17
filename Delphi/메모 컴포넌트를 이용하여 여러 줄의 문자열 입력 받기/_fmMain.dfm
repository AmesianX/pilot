object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 293
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 176
    Top = 204
    Width = 65
    Height = 25
    Shape = stRoundRect
  end
  object Button1: TButton
    Left = 324
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 100
    Height = 89
    Alignment = taCenter
    BorderStyle = bsNone
    Ctl3D = False
    ImeName = 'Microsoft IME 2003'
    Lines.Strings = (
      'Memo1')
    ParentCtl3D = False
    TabOrder = 1
    Visible = False
    OnExit = Memo1Exit
    OnKeyDown = Memo1KeyDown
  end
end
