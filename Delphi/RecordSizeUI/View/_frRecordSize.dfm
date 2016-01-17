object frRecordSize: TfrRecordSize
  Left = 0
  Top = 0
  Width = 984
  Height = 844
  TabOrder = 0
  object SBoxDefaultSize: TScrollBox
    Left = 220
    Top = 30
    Width = 208
    Height = 140
    HorzScrollBar.ButtonSize = 10
    HorzScrollBar.Tracking = True
    HorzScrollBar.Visible = False
    VertScrollBar.ButtonSize = 10
    VertScrollBar.Size = 10
    VertScrollBar.Style = ssFlat
    VertScrollBar.Tracking = True
    BevelInner = bvNone
    BevelOuter = bvRaised
    BevelKind = bkTile
    BorderStyle = bsNone
    TabOrder = 0
    object PanelDefaultSize: TPanel
      Left = 0
      Top = 2
      Width = 190
      Height = 281
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 0
      object RBtn_DS_1280x720: TRadioButton
        Left = 16
        Top = 240
        Width = 100
        Height = 19
        Caption = '  1280 x 720'
        TabOrder = 0
      end
      object RBtn_DS_480x320: TRadioButton
        Left = 16
        Top = 60
        Width = 100
        Height = 19
        Caption = '    480 x 320'
        TabOrder = 1
      end
      object RBtn_DS_320x240: TRadioButton
        Left = 16
        Top = 24
        Width = 100
        Height = 19
        Caption = '    320 x 240'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object RBtn_DS_640x480: TRadioButton
        Left = 16
        Top = 96
        Width = 100
        Height = 19
        Caption = '    640 x 480'
        TabOrder = 3
      end
      object RBtn_DS_1024x768: TRadioButton
        Left = 16
        Top = 204
        Width = 100
        Height = 19
        Caption = '  1024 x 768'
        TabOrder = 4
      end
      object RBtn_DS_720x480: TRadioButton
        Left = 16
        Top = 132
        Width = 100
        Height = 19
        Caption = '    720 x 480'
        TabOrder = 5
      end
      object RBtn_DS_800x600: TRadioButton
        Left = 16
        Top = 168
        Width = 100
        Height = 19
        Caption = '    800 x 600'
        TabOrder = 6
      end
    end
  end
  object Panel: TPanel
    Left = 11
    Top = 32
    Width = 192
    Height = 140
    BevelKind = bkTile
    TabOrder = 1
    object RBtnDefaultSize: TRadioButton
      Left = 16
      Top = 24
      Width = 121
      Height = 17
      Caption = #44592#48376' '#45433#54868' '#49324#51060#51592
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RBtnDefaultSizeClick
    end
    object RBtnMobileSize: TRadioButton
      Left = 16
      Top = 59
      Width = 137
      Height = 17
      Caption = #55092#45824' '#44592#44592#50857' '#49324#51060#51592
      TabOrder = 1
      OnClick = RBtnMobileSizeClick
    end
    object RBtnRatio: TRadioButton
      Left = 16
      Top = 94
      Width = 113
      Height = 17
      Caption = #44256#51221' '#48708#50984' '#49324#51060#51592
      TabOrder = 2
      OnClick = RBtnRatioClick
    end
  end
  object SBoxMobileSize: TScrollBox
    Left = 220
    Top = 200
    Width = 208
    Height = 140
    HorzScrollBar.ButtonSize = 10
    HorzScrollBar.Tracking = True
    HorzScrollBar.Visible = False
    VertScrollBar.ButtonSize = 10
    VertScrollBar.Size = 10
    VertScrollBar.Style = ssFlat
    VertScrollBar.Tracking = True
    BevelInner = bvNone
    BevelOuter = bvRaised
    BevelKind = bkTile
    BorderStyle = bsNone
    TabOrder = 2
    object PanelMobileSize: TPanel
      Left = 0
      Top = 0
      Width = 190
      Height = 317
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 0
      object RBtn_Atree: TRadioButton
        Left = 16
        Top = 240
        Width = 100
        Height = 19
        Caption = '  '#50640#51060#53944#47532
        TabOrder = 0
        OnClick = RBtn_AtreeClick
      end
      object RBtn_Cyon: TRadioButton
        Left = 16
        Top = 60
        Width = 100
        Height = 19
        Caption = '  '#49912#51060#50616
        TabOrder = 1
        OnClick = RBtn_CyonClick
      end
      object RBtn_Samsung: TRadioButton
        Left = 16
        Top = 24
        Width = 100
        Height = 19
        Caption = '  '#49340#49457
        TabOrder = 2
        OnClick = RBtn_SamsungClick
      end
      object RBtn_Sony: TRadioButton
        Left = 16
        Top = 96
        Width = 100
        Height = 19
        Caption = '  '#49548#45768
        TabOrder = 3
        OnClick = RBtn_SonyClick
      end
      object RBtn_Apple: TRadioButton
        Left = 16
        Top = 204
        Width = 100
        Height = 19
        Caption = '  '#50528#54540
        TabOrder = 4
        OnClick = RBtn_AppleClick
      end
      object RBtn_Iriver: TRadioButton
        Left = 16
        Top = 132
        Width = 100
        Height = 19
        Caption = '  '#50500#51060#47532#48260
        TabOrder = 5
        OnClick = RBtn_IriverClick
      end
      object RBtn_Anycall: TRadioButton
        Left = 16
        Top = 168
        Width = 100
        Height = 19
        Caption = '  '#50528#45768#53084
        TabOrder = 6
        OnClick = RBtn_AnycallClick
      end
      object RBtn_Cowon: TRadioButton
        Left = 16
        Top = 276
        Width = 100
        Height = 19
        Caption = '  '#53076#50896
        TabOrder = 7
        OnClick = RBtn_CowonClick
      end
    end
  end
  object PanelRatio: TPanel
    Left = 220
    Top = 359
    Width = 208
    Height = 146
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 3
    object RBtn_16_9: TRadioButton
      Left = 16
      Top = 60
      Width = 100
      Height = 19
      Caption = '  16 : 9'
      TabOrder = 0
    end
    object RBtn_4_3: TRadioButton
      Left = 16
      Top = 24
      Width = 89
      Height = 19
      Caption = '    4 : 3'
      TabOrder = 1
    end
    object RBtn_16_10: TRadioButton
      Left = 16
      Top = 96
      Width = 100
      Height = 19
      Caption = '  16 : 10'
      TabOrder = 2
    end
  end
  object Panel_MS_Samsung: TPanel
    Left = 492
    Top = 29
    Width = 175
    Height = 140
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 4
    object RBtn_Samsung_480x272: TRadioButton
      Left = 16
      Top = 60
      Width = 100
      Height = 19
      Caption = '    480 x 272'
      TabOrder = 0
    end
  end
  object Panel_MS_Cyon: TPanel
    Left = 684
    Top = 28
    Width = 175
    Height = 140
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 5
    object RBtn_Cyon_320x240: TRadioButton
      Left = 16
      Top = 60
      Width = 100
      Height = 19
      Caption = '    320 x 240'
      TabOrder = 0
    end
  end
  object Panel_MS_Sony: TPanel
    Left = 492
    Top = 211
    Width = 175
    Height = 140
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 6
    object RBtn_Sony_368x208: TRadioButton
      Left = 16
      Top = 45
      Width = 100
      Height = 19
      Caption = '    368 x 208'
      TabOrder = 0
    end
    object RBtn_Sony_320x240: TRadioButton
      Left = 16
      Top = 20
      Width = 89
      Height = 19
      Caption = '    320 x 240'
      TabOrder = 1
    end
    object RBtn_Sony_480x272: TRadioButton
      Left = 16
      Top = 70
      Width = 100
      Height = 19
      Caption = '    480 x 272'
      TabOrder = 2
    end
    object RBtn_Sony_720x480: TRadioButton
      Left = 16
      Top = 95
      Width = 100
      Height = 19
      Caption = '    720 x 480'
      TabOrder = 3
    end
  end
  object Panel_MS_Iriver: TPanel
    Left = 684
    Top = 210
    Width = 175
    Height = 140
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 7
    object RBtn_Iriver_320x240: TRadioButton
      Left = 16
      Top = 39
      Width = 100
      Height = 19
      Caption = '    320 x 240'
      TabOrder = 0
    end
    object RBtn_Iriver_480x272: TRadioButton
      Left = 16
      Top = 75
      Width = 89
      Height = 19
      Caption = '    480 x 272'
      TabOrder = 1
    end
  end
  object Panel_MS_Anycall: TPanel
    Left = 492
    Top = 379
    Width = 175
    Height = 140
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 8
    object RBtn_Anycall_720x400: TRadioButton
      Left = 16
      Top = 75
      Width = 100
      Height = 19
      Caption = '    720 x 400'
      TabOrder = 0
    end
    object RBtn_Anycall_640x360: TRadioButton
      Left = 16
      Top = 39
      Width = 89
      Height = 19
      Caption = '    640 x 360'
      TabOrder = 1
    end
  end
  object Panel_MS_Apple: TPanel
    Left = 684
    Top = 379
    Width = 175
    Height = 140
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 9
    object RBtn_Apple_480x320: TRadioButton
      Left = 16
      Top = 60
      Width = 100
      Height = 19
      Caption = '    480 x 320'
      TabOrder = 0
    end
  end
  object Panel_MS_Atree: TPanel
    Left = 492
    Top = 560
    Width = 175
    Height = 140
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 10
    object RBtn_Atree_480x272: TRadioButton
      Left = 16
      Top = 60
      Width = 100
      Height = 19
      Caption = '    480 x 272'
      TabOrder = 0
    end
  end
  object Panel_MS_Cowon: TPanel
    Left = 684
    Top = 559
    Width = 175
    Height = 140
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 11
    object RBtn_Cowon_480x272: TRadioButton
      Left = 16
      Top = 59
      Width = 100
      Height = 19
      Caption = '    480 x 272'
      TabOrder = 0
    end
    object RBtn_Cowon_320x240: TRadioButton
      Left = 16
      Top = 23
      Width = 89
      Height = 19
      Caption = '    320 x 240'
      TabOrder = 1
    end
    object RBtn_Cowon_624x352: TRadioButton
      Left = 16
      Top = 95
      Width = 100
      Height = 19
      Caption = '    624 x 352'
      TabOrder = 2
    end
  end
end
