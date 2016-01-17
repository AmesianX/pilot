object fmLog: TfmLog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #47196#44536
  ClientHeight = 457
  ClientWidth = 655
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gb_Cache: TGroupBox
    Left = 5
    Top = 31
    Width = 313
    Height = 241
    Caption = #52880#49772
    TabOrder = 0
    object edt_Cache_UsedAmount_Capacity: TLabeledEdit
      Left = 152
      Top = 16
      Width = 145
      Height = 21
      EditLabel.Width = 124
      EditLabel.Height = 13
      EditLabel.Caption = #52880#49772' '#49324#50857#47049' / '#52880#49772' '#50857#47049
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 0
    end
    object edt_CacheRemainTime: TLabeledEdit
      Left = 152
      Top = 43
      Width = 145
      Height = 21
      EditLabel.Width = 78
      EditLabel.Height = 13
      EditLabel.Caption = #52880#49772' '#50976#51648' '#49884#44036
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 1
    end
    object edt_CacheCount: TLabeledEdit
      Left = 152
      Top = 70
      Width = 145
      Height = 21
      EditLabel.Width = 90
      EditLabel.Height = 13
      EditLabel.Caption = #52880#49772' '#45936#51060#53552' '#44060#49688
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 2
    end
    object edt_CacheReusedCount: TLabeledEdit
      Left = 152
      Top = 97
      Width = 145
      Height = 21
      EditLabel.Width = 90
      EditLabel.Height = 13
      EditLabel.Caption = #52880#49772' '#51116#49324#50857' '#54943#49688
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 3
    end
    object edt_CacheRequestCount: TLabeledEdit
      Left = 152
      Top = 151
      Width = 145
      Height = 21
      EditLabel.Width = 90
      EditLabel.Height = 13
      EditLabel.Caption = #52880#49772' '#51116#50836#52397' '#54943#49688
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 4
    end
    object edt_CacheSendCount: TLabeledEdit
      Left = 152
      Top = 178
      Width = 145
      Height = 21
      EditLabel.Width = 78
      EditLabel.Height = 13
      EditLabel.Caption = #52880#49772' '#49569#49888' '#54943#49688
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 5
    end
    object edt_CacheSendAmount: TLabeledEdit
      Left = 152
      Top = 205
      Width = 145
      Height = 21
      EditLabel.Width = 63
      EditLabel.Height = 13
      EditLabel.Caption = #52880#49772' '#49569#49888#47049
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 6
    end
    object edt_CacheReusedAmount: TLabeledEdit
      Left = 152
      Top = 124
      Width = 145
      Height = 21
      EditLabel.Width = 75
      EditLabel.Height = 13
      EditLabel.Caption = #52880#49772' '#51116#49324#50857#47049
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 7
    end
  end
  object gb_Packet: TGroupBox
    Left = 333
    Top = 31
    Width = 313
    Height = 241
    Caption = #54056#53431
    TabOrder = 1
    object edt_PacketSendCount: TLabeledEdit
      Left = 136
      Top = 16
      Width = 145
      Height = 21
      EditLabel.Width = 78
      EditLabel.Height = 13
      EditLabel.Caption = #54056#53431' '#49569#49888' '#54943#49688
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 0
    end
    object edt_PacketSendAmount: TLabeledEdit
      Left = 136
      Top = 43
      Width = 145
      Height = 21
      EditLabel.Width = 63
      EditLabel.Height = 13
      EditLabel.Caption = #54056#53431' '#49569#49888#47049
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 1
    end
    object edt_PacketRecvCount: TLabeledEdit
      Left = 136
      Top = 97
      Width = 145
      Height = 21
      EditLabel.Width = 78
      EditLabel.Height = 13
      EditLabel.Caption = #54056#53431' '#49688#49888' '#54943#49688
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 2
    end
    object edt_PacketRecvAmount: TLabeledEdit
      Left = 136
      Top = 124
      Width = 145
      Height = 21
      EditLabel.Width = 63
      EditLabel.Height = 13
      EditLabel.Caption = #54056#53431' '#49688#49888#47049
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 3
    end
    object edt_PacketBlockRecvCount: TLabeledEdit
      Left = 136
      Top = 151
      Width = 145
      Height = 21
      EditLabel.Width = 105
      EditLabel.Height = 13
      EditLabel.Caption = #49892#51228' '#48660#46973' '#49688#49888' '#54943#49688
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 4
    end
    object edt_PacketBlockRecvAmount: TLabeledEdit
      Left = 136
      Top = 178
      Width = 145
      Height = 21
      EditLabel.Width = 90
      EditLabel.Height = 13
      EditLabel.Caption = #49892#51228' '#48660#46973' '#49688#49888#47049
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 5
    end
  end
  object gb_FrameBuffer: TGroupBox
    Left = 8
    Top = 278
    Width = 313
    Height = 138
    Caption = #54532#47112#51076#48260#54140
    TabOrder = 2
    object edt_FrameBufferCount: TLabeledEdit
      Left = 152
      Top = 15
      Width = 145
      Height = 21
      EditLabel.Width = 126
      EditLabel.Height = 13
      EditLabel.Caption = #54532#47112#51076#48260#54140' '#45936#51060#53552' '#44060#49688
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 0
    end
    object edt_FrameBufferAmount: TLabeledEdit
      Left = 152
      Top = 39
      Width = 145
      Height = 21
      EditLabel.Width = 126
      EditLabel.Height = 13
      EditLabel.Caption = #54532#47112#51076#48260#54140' '#45936#51060#53552' '#53356#44592
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 1
    end
  end
  object gb_EncodeDecode: TGroupBox
    Left = 334
    Top = 278
    Width = 313
    Height = 138
    Caption = #51064#53076#46300'/'#46356#53076#46300
    TabOrder = 3
    object edt_EncodeTime: TLabeledEdit
      Left = 152
      Top = 15
      Width = 145
      Height = 21
      EditLabel.Width = 114
      EditLabel.Height = 13
      EditLabel.Caption = #51064#53076#46300' '#54788#51116' '#49548#50836#49884#44036
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 0
    end
    object edt_EncodeTotalTime: TLabeledEdit
      Left = 152
      Top = 42
      Width = 145
      Height = 21
      EditLabel.Width = 102
      EditLabel.Height = 13
      EditLabel.Caption = #51064#53076#46300' '#52509' '#49548#50836#49884#44036
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 1
    end
    object edt_DecodeTime: TLabeledEdit
      Left = 152
      Top = 69
      Width = 145
      Height = 21
      EditLabel.Width = 114
      EditLabel.Height = 13
      EditLabel.Caption = #46356#53076#46300' '#54788#51116' '#49548#50836#49884#44036
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 2
    end
    object edt_DecodeTotalTime: TLabeledEdit
      Left = 152
      Top = 96
      Width = 145
      Height = 21
      EditLabel.Width = 102
      EditLabel.Height = 13
      EditLabel.Caption = #46356#53076#46300' '#52509' '#49548#50836#49884#44036
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 3
    end
  end
  object btn_Close: TButton
    Left = 567
    Top = 422
    Width = 75
    Height = 25
    Caption = #45803#44592
    TabOrder = 4
    OnClick = btn_CloseClick
  end
  object edt_ElapsedTime: TLabeledEdit
    Left = 157
    Top = 8
    Width = 145
    Height = 21
    EditLabel.Width = 51
    EditLabel.Height = 13
    EditLabel.Caption = #44221#44284' '#49884#44036
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 5
  end
  object btn_Pause: TButton
    Left = 486
    Top = 422
    Width = 75
    Height = 25
    Caption = #51068#49884#51473#51648
    TabOrder = 6
    OnClick = btn_PauseClick
  end
  object Timer: TTimer
    Interval = 200
    OnTimer = TimerTimer
    Left = 464
    Top = 392
  end
end
