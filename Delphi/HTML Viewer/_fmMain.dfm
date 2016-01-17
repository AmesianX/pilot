object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 569
  ClientWidth = 654
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object htmlvwr1: THtmlViewer
    Left = 0
    Top = 41
    Width = 654
    Height = 528
    TabOrder = 0
    Align = alClient
    BorderStyle = htFocused
    CharSet = DEFAULT_CHARSET
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    HistoryMaxCount = 0
    NoSelect = False
    PrintMarginBottom = 2.000000000000000000
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintScale = 1.000000000000000000
    QuirksMode = qmDetect
    ExplicitLeft = 96
    ExplicitTop = 104
    ExplicitWidth = 150
    ExplicitHeight = 150
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 654
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 200
    ExplicitTop = 32
    ExplicitWidth = 185
    object bt1: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Caption = 'bt1'
      TabOrder = 0
      OnClick = bt1Click
    end
  end
end
