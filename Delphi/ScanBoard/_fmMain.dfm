object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 739
  ClientWidth = 755
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
    Width = 755
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 6
      Top = 50
      Width = 35
      Height = 13
      Caption = #55064#46020'%'
    end
    object Label2: TLabel
      Left = 170
      Top = 50
      Width = 24
      Height = 13
      Caption = #49353#49345
    end
    object spPenColor: TShape
      Left = 352
      Top = 11
      Width = 65
      Height = 24
    end
    object spColor: TShape
      Left = 352
      Top = 47
      Width = 65
      Height = 24
    end
    object btExecute: TButton
      Left = 85
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btExecute'
      TabOrder = 0
      OnClick = btExecuteClick
    end
    object edBrightness: TEdit
      Left = 47
      Top = 47
      Width = 110
      Height = 21
      ImeName = 'Microsoft IME 2003'
      TabOrder = 1
      Text = '70'
    end
    object edColor: TEdit
      Left = 200
      Top = 47
      Width = 121
      Height = 21
      ImeName = 'Microsoft IME 2003'
      TabOrder = 2
      Text = '30'
    end
    object btOpen: TButton
      Left = 4
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btOpen'
      TabOrder = 3
      OnClick = btOpenClick
    end
    object edPenHSB: TEdit
      Left = 423
      Top = 11
      Width = 121
      Height = 21
      ImeName = 'Microsoft IME 2003'
      TabOrder = 4
    end
    object edHSB: TEdit
      Left = 423
      Top = 47
      Width = 121
      Height = 21
      ImeName = 'Microsoft IME 2003'
      TabOrder = 5
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 81
    Width = 755
    Height = 658
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = #50896#48376
      object sbSrc: TScrollBox
        Left = 0
        Top = 0
        Width = 747
        Height = 630
        Align = alClient
        TabOrder = 0
        object imgSrc: TImage
          Left = 0
          Top = 0
          Width = 33
          Height = 33
          AutoSize = True
          OnClick = imgSrcClick
          OnMouseMove = imgSrcMouseMove
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = #52376#47532' '#51060#54980
      ImageIndex = 1
      object sbDst: TScrollBox
        Left = 0
        Top = 0
        Width = 747
        Height = 630
        Align = alClient
        TabOrder = 0
        object imgDst: TImage
          Left = 0
          Top = 0
          Width = 33
          Height = 33
          AutoSize = True
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Image File|*.bmp; *.jpeg; *.jpg'
    Left = 368
    Top = 368
  end
end
