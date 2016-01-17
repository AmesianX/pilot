object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'SVN '#47196#44536' '#51221#47532
  ClientHeight = 640
  ClientWidth = 829
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 333
    Width = 829
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 41
    ExplicitWidth = 282
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 829
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 852
    object btRun: TButton
      Left = 12
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 0
      OnClick = btRunClick
    end
    object btSave: TButton
      Left = 93
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 1
      OnClick = btSaveClick
    end
  end
  object moMsg: TMemo
    Left = 0
    Top = 41
    Width = 829
    Height = 292
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitLeft = 36
    ExplicitTop = -68
    ExplicitWidth = 852
    ExplicitHeight = 279
  end
  object moOutput: TMemo
    Left = 0
    Top = 336
    Width = 829
    Height = 304
    Align = alBottom
    ImeName = 'Microsoft IME 2010'
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text file|*.txt'
    Left = 408
    Top = 324
  end
end
