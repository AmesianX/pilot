object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 301
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 562
    Height = 301
    ActivePage = tsUnit
    Align = alClient
    TabOrder = 0
    object tsUnit: TTabSheet
      Caption = 'Unit'
      object moUnit: TMemo
        Left = 0
        Top = 0
        Width = 554
        Height = 273
        Align = alClient
        ImeName = 'Microsoft IME 2003'
        Lines.Strings = (
          'unit _@FrameName;'
          ''
          'interface'
          ''
          'uses'
          '  ValueList,'
          
            '  Windows, Messages, SysUtils, Variants, Classes, Graphics, Cont' +
            'rols, Forms,'
          '  Dialogs;'
          ''
          'type'
          '  T@FrameName = class(TFrame)'
          '  private'
          '  public'
          '    constructor Create(AOwner: TComponent); override;'
          '    destructor Destroy; override;'
          '  end;'
          ''
          'implementation'
          ''
          'uses'
          '  Global, View, Option;'
          ''
          '{$R *.dfm}'
          ''
          'constructor T@FrameName.Create(AOwner: TComponent);'
          'begin'
          '  inherited;'
          ''
          '  TView.Obj.Add(Self);'
          'end;'
          ''
          'destructor T@FrameName.Destroy;'
          'begin'
          '  TView.Obj.Remove(Self);'
          ''
          '  inherited;'
          'end;'
          ''
          'end.')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsDFM: TTabSheet
      Caption = 'DFM'
      ImageIndex = 1
      object moDFM: TMemo
        Left = 0
        Top = 0
        Width = 554
        Height = 273
        Align = alClient
        ImeName = 'Microsoft IME 2003'
        Lines.Strings = (
          'object @FrameName: T@FrameName'
          '  Left = 0'
          '  Top = 0'
          '  Width = 320'
          '  Height = 240'
          '  TabOrder = 0'
          'end')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
end
