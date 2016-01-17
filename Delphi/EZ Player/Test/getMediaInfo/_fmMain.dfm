object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 524
  ClientWidth = 716
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
    Top = 41
    Width = 716
    Height = 172
    Align = alTop
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 716
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 20
    ExplicitTop = -21
    object btOpen: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btOpenClick
    end
    object btReadPacket: TButton
      Left = 89
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Read Packet'
      TabOrder = 1
      OnClick = btReadPacketClick
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 213
    Width = 716
    Height = 311
    Align = alClient
    TabOrder = 2
    ExplicitLeft = 160
    ExplicitTop = 308
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Image: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All file(s)|*.*'
    Left = 272
    Top = 152
  end
end
