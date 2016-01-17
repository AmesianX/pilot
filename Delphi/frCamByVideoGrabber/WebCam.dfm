object frCam: TfrCam
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  Color = clBlack
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object VideoGrabber: TVideoGrabber
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    Color = clBlack
    OnResize = VideoGrabberResize
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
    OnFrameBitmap = VideoGrabberFrameBitmap
  end
end
