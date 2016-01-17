object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'PacketLossTest - Sender'
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
  object lblError: TLabel
    Left = 32
    Top = 80
    Width = 24
    Height = 13
    Caption = 'Error'
  end
  object lblPing: TLabel
    Left = 32
    Top = 99
    Width = 20
    Height = 13
    Caption = 'Ping'
  end
  object lblSpeed: TLabel
    Left = 32
    Top = 118
    Width = 30
    Height = 13
    Caption = 'Speed'
  end
  object EditIP: TLabeledEdit
    Left = 24
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = 'MyIP'
    ImeName = 'Microsoft Office IME 2007'
    ReadOnly = True
    TabOrder = 0
  end
end
