object frDeskTopScreen: TfrDeskTopScreen
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object ScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    TabOrder = 0
    object Image: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Left = 148
    Top = 108
  end
end
