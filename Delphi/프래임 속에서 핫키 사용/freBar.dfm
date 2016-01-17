object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 506
  Height = 103
  PopupMenu = PopupMenu1
  TabOrder = 0
  TabStop = True
  object Button1: TButton
    Left = 12
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object PopupMenu1: TPopupMenu
    Left = 104
    Top = 12
    object test1: TMenuItem
      Caption = 'test'
      ShortCut = 16449
      OnClick = test1Click
    end
  end
end
