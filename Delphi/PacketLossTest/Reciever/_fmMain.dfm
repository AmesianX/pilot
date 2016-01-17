object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'PacketLossTest - Reciever'
  ClientHeight = 290
  ClientWidth = 554
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
  object lblPing: TLabel
    Left = 32
    Top = 128
    Width = 22
    Height = 13
    Caption = #54609' : '
  end
  object EditIP: TLabeledEdit
    Left = 32
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = 'MyIP'
    ImeName = 'Microsoft Office IME 2007'
    ReadOnly = True
    TabOrder = 0
  end
  object EditPeerIP: TLabeledEdit
    Left = 168
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 32
    EditLabel.Height = 13
    EditLabel.Caption = 'PeerIP'
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 1
  end
  object btnAccept: TButton
    Left = 312
    Top = 22
    Width = 75
    Height = 25
    Caption = #51201#50857
    TabOrder = 2
    OnClick = btnAcceptClick
  end
  object lblErrorPercent: TStaticText
    Left = 32
    Top = 80
    Width = 50
    Height = 17
    Caption = #50724#47448#50984' : '
    TabOrder = 3
  end
  object lblSpeed: TStaticText
    Left = 32
    Top = 103
    Width = 38
    Height = 17
    Caption = #49549#46020' : '
    TabOrder = 4
  end
end
