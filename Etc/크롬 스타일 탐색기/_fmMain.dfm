object fmMain: TfmMain
  Left = 0
  Top = 0
  ClientHeight = 562
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object rkAeroTabs1: TrkAeroTabs
    Left = 0
    Top = 0
    Width = 784
    Height = 25
    Align = alTop
    AllowTabDrag = True
    ColorBackground = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ExplicitLeft = -1
    ExplicitTop = 36
    ExplicitWidth = 554
  end
  object rkSmartPath1: TrkSmartPath
    Left = 0
    Top = 25
    Width = 784
    Height = 25
    Align = alTop
    BtnGreyGrad1 = 15921906
    BtnGreyGrad2 = 14935011
    BtnNormGrad1 = 16643818
    BtnNormGrad2 = 16046502
    BtnHotGrad1 = 16643818
    BtnHotGrad2 = 16441260
    BtnPenGray = 9408399
    BtnPenNorm = 11632444
    BtnPenShade1 = 9598820
    BtnPenShade2 = 15388572
    BtnPenArrow = clBlack
    ComputerAsDefault = True
    DirMustExist = True
    EmptyPathIcon = -1
    NewFolderName = 'NewFolder'
    ParentColor = False
    ParentBackground = False
    Path = 'C:\Users\Ryu\Documents\'
    SpecialFolders = [spDesktop, spDocuments]
    TabOrder = 1
    ExplicitLeft = -8
    ExplicitTop = 236
    ExplicitWidth = 554
  end
  inline frShellControl: TfrShellControl
    Left = 0
    Top = 50
    Width = 784
    Height = 512
    Align = alClient
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentColor = False
    ParentFont = False
    TabOrder = 2
    ExplicitLeft = 148
    ExplicitTop = 99
    inherited Splitter1: TSplitter
      Height = 512
    end
    inherited ShellTreeView: TShellTreeView
      Height = 512
    end
    inherited ShellListView: TShellListView
      Width = 496
      Height = 512
      ExplicitLeft = 288
      ExplicitTop = 0
      ExplicitWidth = 348
      ExplicitHeight = 463
    end
  end
end
