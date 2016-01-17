object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Invisible Path'
  ClientHeight = 573
  ClientWidth = 792
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 792
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btOpen: TButton
      Left = 12
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btOpen'
      TabOrder = 0
    end
    object btSave: TButton
      Left = 93
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btSave'
      TabOrder = 1
    end
    object btExecute: TButton
      Left = 174
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btExecute'
      TabOrder = 2
      OnClick = btExecuteClick
    end
  end
  object moSrc: TMemo
    Left = 0
    Top = 41
    Width = 792
    Height = 532
    Align = alClient
    Font.Charset = HANGEUL_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Fixedsys'
    Font.Style = []
    ImeName = 'Microsoft IME 2003'
    Lines.Strings = (
      'TCommon'
      '  // TCommon'#51008' '#49548#49828#47196' '#51089#49457#46104#45716' '#44163#51008' '#50500#45768#44256','
      '  // '#54364#51456' '#46972#51060#48652#47084#47532' '#46608#45716' '#44277#50857' '#46972#51060#48652#47084#47532#50640' '#51080#45716' '#51060#48292#53944' '#51221#51032#47484' '#49324#50857#54616#44592' '#50948#54644#49436
      '  // '#48152#46300#49884' '#54596#50836#54620' '#49440#50616' '#48512#48516#51060#45796'.'
      '  TNotifyEvent = procedure (Sender:TObject) of object;'
      '  // '#44033' '#53364#47000#49436' '#49440#50616' '#49324#51060#50640#45716' '#48152#46300#49884' '#54616#45208' '#51060#49345#51032' '#48712' '#46972#51064#51060' '#54596#50836#54616#45796'.'
      ''
      'TVoiceClient'
      '  function Connect(AHost:string; APort:integer):boolean;'
      '  procedure Disconnect;'
      '  procedure Talk;'
      '  procedure Listen;'
      '  // Todo : '#49905#44544#53668' '#54056#53556' '#51201#50857' '#49548#49828' '#49373#49457
      ''
      'TVoiceRecorder'
      '  // '#53364#47000#49828' '#47560#45796' '#50976#45787#51060' '#49373#49457#46108#45796'.'
      '  // '#51060#48292#53944' '#53440#51077' '#49440#50616#51008' '#54644#45817' '#53364#47000#49828#50752' '#44057#51008' '#50976#45787#50640' '#49373#49457#54616#44592' '#50948#54644#49436' '#53364#47000#49828' '#51060#47492' '#45796#51020#50640' '#51089#49457#54620#45796'.'
      
        '  TNewDataEvent = procedure (Sender:TObject; AData:pointer; ASiz' +
        'e:integer) of object;'
      '  procedure Start;'
      '  procedure Stop;'
      '  event OnNewData : TNewDataEvent;'
      ''
      'TVoicePlayer'
      '  procedure DataIn(AData:pointer; ASize:integer);'
      ''
      'TVoiceSocket'
      
        '  TReceivedEvent = procedure (Sender:TObject; AData:pointer; ASi' +
        'ze:integer) of object;'
      '  function Connect(AHost:string; APort:integer):boolean;'
      '  procedure Disconnect;'
      '  procedure Send(AData:pointer; ASize:integer);'
      '  event OnReceived : TReceivedEvent;'
      ''
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
