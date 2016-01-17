object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
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
  object moMsg: TMemo
    Left = 0
    Top = 41
    Width = 554
    Height = 249
    Align = alClient
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      '    /// '#51089#51008' '#54056#53431' '#50668#47084' '#44060#47484' '#54620' '#48264#50640' '#47926#50612#49436' '#48372#45236#44592' '#50948#54632
      '    ftPacketGroup,'
      '    ftPing, ftNetworkQuality, ftTextData,'
      '    ftTimeMarker,  /// '#51221#54644#51652' '#49884#44036' '#45800#50948#47196' '#48736#47476#44172' '#51060#46041#54616#44592' '#50948#54644' '#45796#51020' '#50948#52824#47484' '#51200#51109#54620#45796'.'
      ''
      '    ftEndOfFrame,'
      ''
      '    ftDeskCamInfo,'
      ''
      '    ftAudioInfo,'
      ''
      '    ftVideo800,  /// 800*600 '#54644#49345#46020#51032' '#52896' '#50689#49345' '#54056#53431
      ''
      '    ftAudio20ms,  /// 20ms '#45800#50948#47196' '#45433#51020' '#46108' '#51020#49457' '#54056#53431
      
        '    ftFakeAudio20ms,  /// '#50724#46356#50724' '#45800#50948#47196' '#49905#53356#47484' '#54616#44592' '#46412#47928#50640' '#50724#46356#50724#47484' '#51077#47141#54616#51648' '#50506#51012' '#44221#50864' '#44032#51676' ' +
        #50724#46356#50724' '#54056#53431#51012' '#49324#50857
      ''
      
        '    ftVideo,  /// VPX: WebM'#51012' '#51060#50857#54644#49436' '#50517#52629' '#46108' '#48708#46356#50724' '#50689#49345#51060' '#45812#44200#51256' '#51080#45796'. '#51088#49464#54620' '#54252#47719#51008' V' +
        'ideoZip '#46972#51060#48652#47084#47532#50640' '#51080#45796'.'
      '    ftBitmap,  /// _PixelFormat'#51004#47196' '#44396#49457#46108' Bitmap'#51032' Raw Data'
      '    ftJPeg,  /// ZLib '#45936#51060#53552#45716' 32*32 bitmap '#45936#51060#53552#47484' ZLib'#47196' '#50517#52629#54620' '#44163#51060#45796'.'
      '    ftZLib,'
      '    ftJPegNoHeader,'
      
        '    ftBitmapCache, ftJPegCache, ftZLibCache, ftJPegNoHeaderCache' +
        ','
      ''
      '    ftKeyOnly,'
      '    ftBitmapKey, ftJPegKey, ftZLibKey, ftJPegNoHeaderKey,'
      
        '    ftBitmapCacheKey, ftJPegCacheKey, ftZLibCacheKey, ftJPegNoHe' +
        'aderCacheKey,'
      ''
      
        '    ftBitmapWithKey, ftJPegWithKey, ftZLibWithKey, ftJPegNoHeade' +
        'rWithKey,'
      
        '    ftBitmapCacheWithKey, ftJPegCacheWithKey, ftZLibCacheWithKey' +
        ', ftJPegNoHeaderCacheWithKey,'
      ''
      
        '    ftJPeg90, ftJPeg90NoHeader, ftJPeg90Cache, ftJPeg90NoHeaderC' +
        'ache,'
      
        '    ftJPeg90Key, ftJPeg90NoHeaderKey, ftJPeg90CacheKey, ftJPeg90' +
        'NoHeaderCacheKey,'
      
        '    ftJPeg90WithKey, ftJPeg90NoHeaderWithKey, ftJPeg90CacheWithK' +
        'ey, ftJPeg90NoHeaderCacheWithKey,'
      ''
      
        '    ftClearVideo,  /// '#48708#46356#50724' '#45936#51060#53552#44032' '#48128#47160#51012' '#46412', '#45936#51060#53552#47484' '#49325#51228#54616#44256' '#49688#49888#52769#50640#46020' '#51060#47484' '#50508#47536#45796'.  ' +
        #52488#44592#54868#44032' '#54596#50836#54632'.'
      ''
      '    ftCursorImage,  /// '#49892#51228' '#52964#49436' '#51060#48120#51648' '#51221#48372#44032' '#51080#51020
      '    ftCursor,  /// '#52964#49436#51060#48120#51648#50640' '#45824#54620' Index'#50752' X, Y '#51221#48372#44032' '#51080#51020
      ''
      '    // '#44592#51316#51032' '#54840#54872#49457' '#46412#47928#50640' '#46244#50640' '#52628#44032#54632
      '    ftAudioSub,'
      ''
      
        '    ftJPeg70, ftJPeg70NoHeader, ftJPeg70Cache, ftJPeg70NoHeaderC' +
        'ache,'
      
        '    ftJPeg70Key, ftJPeg70NoHeaderKey, ftJPeg70CacheKey, ftJPeg70' +
        'NoHeaderCacheKey,'
      
        '    ftJPeg70WithKey, ftJPeg70NoHeaderWithKey, ftJPeg70CacheWithK' +
        'ey, ftJPeg70NoHeaderCacheWithKey,'
      ''
      
        '    ftJPeg80, ftJPeg80NoHeader, ftJPeg80Cache, ftJPeg80NoHeaderC' +
        'ache,'
      
        '    ftJPeg80Key, ftJPeg80NoHeaderKey, ftJPeg80CacheKey, ftJPeg80' +
        'NoHeaderCacheKey,'
      
        '    ftJPeg80WithKey, ftJPeg80NoHeaderWithKey, ftJPeg80CacheWithK' +
        'ey, ftJPeg80NoHeaderCacheWithKey,'
      ''
      '    ftPixel // '#54589#49472' '#45800#50948#47196' '#48708#44368#54644#49436' '#47564#46304' '#48660#47197)
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitTop = 40
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btConvert: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btConvert'
      TabOrder = 0
      OnClick = btConvertClick
    end
  end
end
