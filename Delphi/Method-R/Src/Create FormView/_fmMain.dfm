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
          'unit _@FormName;'
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
          '  T@FormName = class(TForm)'
          '    procedure FormCreate(Sender: TObject);'
          '    procedure FormDestroy(Sender: TObject);'
          '  private'
          '  public'
          '  end;'
          ''
          'var'
          '  @FormName: T@FormName;'
          ''
          'implementation'
          ''
          'uses'
          '  Global, View, Option;'
          ''
          '{$R *.dfm}'
          ''
          'procedure T@FormName.FormCreate(Sender: TObject);'
          'begin'
          '  TView.Obj.Add(Self);'
          'end;'
          ''
          'procedure T@FormName.FormDestroy(Sender: TObject);'
          'begin'
          '  TView.Obj.Remove(Self);'
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
          'object @FormName: T@FormName'
          '  Left = 0'
          '  Top = 0'
          '  Caption = '#39'@FormName'#39
          '  ClientHeight = 240'
          '  ClientWidth = 320'
          '  Color = clBtnFace'
          '  Font.Charset = DEFAULT_CHARSET'
          '  Font.Color = clWindowText'
          '  Font.Height = -11'
          '  Font.Name = '#39'Tahoma'#39
          '  Font.Style = []'
          '  OldCreateOrder = False'
          '  OnCreate = FormCreate'
          '  OnDestroy = FormDestroy'
          '  PixelsPerInch = 96'
          '  TextHeight = 13'
          'end')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
end
