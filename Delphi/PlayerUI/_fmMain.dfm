object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 482
  ClientWidth = 722
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inline frPlayer1: TfrPlayer
    Left = 0
    Top = 0
    Width = 722
    Height = 482
    Align = alClient
    Color = clSkyBlue
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitWidth = 722
    ExplicitHeight = 891
    inherited ImgScreen: TImage
      Width = 722
      Height = 462
    end
    inherited ControlPanel: TPanel
      Top = 462
      Width = 722
      inherited ImgControlPanelBackground: TImage
        Width = 722
      end
    end
  end
end
