object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = #50937#51004#47196' Post '#54616#44592
  ClientHeight = 293
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object moData: TMemo
    Left = 0
    Top = 41
    Width = 426
    Height = 252
    Align = alClient
    ImeName = 'Microsoft IME 2003'
    Lines.Strings = (
      'UserID='#47448#51333#53469
      'Password='#50516#54840)
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 124
    ExplicitTop = 140
    ExplicitWidth = 185
    object btPost: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Post'
      TabOrder = 0
      OnClick = btPostClick
    end
  end
  object IdHTTP: TIdHTTP
    MaxLineAction = maException
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HTTPOptions = [hoForceEncodeParams]
    Left = 12
    Top = 84
  end
end
