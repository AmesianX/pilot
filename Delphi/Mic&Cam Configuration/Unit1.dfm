object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 492
  ClientWidth = 280
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
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 36
    Height = 13
    Caption = #47560#51060#53356
  end
  object Label2: TLabel
    Left = 24
    Top = 168
    Width = 24
    Height = 13
    Caption = #50937#52896
  end
  object Label3: TLabel
    Left = 24
    Top = 128
    Width = 146
    Height = 13
    Caption = #51060#44148' '#47560#51060#53356' '#44221#44256#47928#44396#46972#45716'~'
  end
  object pbMic: TJvXPProgressBar
    Left = 24
    Top = 40
    Width = 233
    Height = 17
    BarColorFrom = clGreen
    BarColorTo = clLime
    Position = 50
  end
  object cbMic: TComboBox
    Left = 80
    Top = 13
    Width = 177
    Height = 21
    Style = csDropDownList
    ImeName = 'Microsoft Office IME 2007'
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbMicChange
  end
  object tbVolume: TTrackBar
    Left = 24
    Top = 72
    Width = 233
    Height = 33
    LineSize = 2
    Max = 100
    Frequency = 10
    Position = 20
    TabOrder = 1
    OnChange = tbVolumeChange
  end
  object cbWebCam: TComboBox
    Left = 80
    Top = 165
    Width = 177
    Height = 21
    Style = csDropDownList
    ImeName = 'Microsoft Office IME 2007'
    ItemHeight = 13
    TabOrder = 2
  end
  object Button1: TButton
    Left = 182
    Top = 418
    Width = 75
    Height = 25
    Caption = #54869#51064
    TabOrder = 3
  end
  object VideoGrabber: TVideoGrabber
    Left = 15
    Top = 200
    Width = 248
    Height = 180
    Caption = 'Cam'#51012' '#50672#44208' '#54644' '#51452#49464#50836
    Color = clBlack
    AudioCompressor = 0
    AutoFilePrefix = 'vg'
    DualDisplay_Left = 400
    DualDisplay_Top = 20
    Cropping_Zoom = 1.000000000000000000
    LicenseString = 'N/A'
    MotionDetector_Grid = 
      '5555555555 5555555555 5555555555 5555555555 5555555555 555555555' +
      '5 5555555555 5555555555 5555555555 5555555555 '
    PlayerSpeedRatio = 1.000000000000000000
    Reencoding_StartTime = -1
    Reencoding_StartFrame = -1
    Reencoding_StopTime = -1
    Reencoding_StopFrame = -1
    TextOverlay_Font.Charset = DEFAULT_CHARSET
    TextOverlay_Font.Color = clAqua
    TextOverlay_Font.Height = -16
    TextOverlay_Font.Name = 'Tahoma'
    TextOverlay_Font.Style = []
    TextOverlay_String = 
      'Note: the date/time formats '#13#10'can be easily modified.'#13#10#13#10'system ' +
      'date/time: %sys_time[dd/mm/yy hh:nn:ss]%'#13#10'DV time code: %time_co' +
      'de%'#13#10'DV date/time: %dv_time[dd/mm/yy hh:nn:ss]%'#13#10'frame number: %' +
      'frame_count%'#13#10'time (full): %time_full%'#13#10'time (sec): %time_sec%'#13#10 +
      'time (ns): %time_100ns%'
    VideoCompression_Quality = 1.000000000000000000
    VideoCompressor = 0
    VideoFromImages_TemporaryFile = 'SetOfBitmaps01.dat'
    VideoProcessing_RotationCustomAngle = 45.500000000000000000
    VideoSource_FileOrURL_StartTime = -1
    VideoSource_FileOrURL_StopTime = -1
  end
  object TimerPreview: TTimer
    Interval = 500
    OnTimer = TimerPreviewTimer
    Left = 240
    Top = 8
  end
  object TimerVolumeGauge: TTimer
    Interval = 20
    OnTimer = TimerVolumeGaugeTimer
    Left = 240
    Top = 48
  end
end
