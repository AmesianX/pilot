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
    ActivePage = tsApplication
    Align = alClient
    TabOrder = 0
    object tsApplication: TTabSheet
      Caption = 'Application'
      ImageIndex = 5
      object moApplication: TMemo
        Left = 0
        Top = 0
        Width = 554
        Height = 273
        Align = alClient
        ImeName = 'Microsoft IME 2003'
        Lines.Strings = (
          'program @PrjectName;'
          ''
          'uses'
          '  SysUtils,'
          '  Forms,'
          '  Dialogs,'
          '  Global in '#39'Globals\Global.pas'#39','
          '  Option in '#39'Options\Option.pas'#39','
          '  View in '#39'Views\View.pas'#39','
          '  _fmMain in '#39'Views\_fmMain.pas'#39' {fmMain};'
          ''
          '{$R *.res}'
          ''
          'begin'
          'try'
          '  TGlobal.Obj.Initialize;'
          ''
          '  Application.Initialize;'
          '  Application.MainFormOnTaskbar := True;'
          '  Application.ShowMainForm := true;'
          '  Application.CreateForm(TfmMain, fmMain);'
          '  Application.Run;'
          ''
          'except'
          '  on E : Exception do MessageDlg(E.Message, mtError, [mbOk], 0);'
          'end;'
          ''
          'TGlobal.Obj.Finalize;'
          'end.'
          '')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsMainUnit: TTabSheet
      Caption = 'MainUnit'
      ImageIndex = 3
      object moMainUnit: TMemo
        Left = 0
        Top = 0
        Width = 554
        Height = 273
        Align = alClient
        ImeName = 'Microsoft IME 2003'
        Lines.Strings = (
          'unit _fmMain;'
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
          '  TfmMain = class(TForm)'
          '    procedure FormCreate(Sender: TObject);'
          '    procedure FormDestroy(Sender: TObject);'
          '  private'
          '  public'
          '  published'
          '    procedure rp_Terminate(APacket:TValueList);'
          '  end;'
          ''
          'var'
          '  fmMain: TfmMain;'
          ''
          'implementation'
          ''
          'uses'
          '  Global, View, Option;'
          ''
          '{$R *.dfm}'
          ''
          'procedure TfmMain.FormCreate(Sender: TObject);'
          'begin'
          '  TView.Obj.Add(Self);'
          'end;'
          ''
          'procedure TfmMain.FormDestroy(Sender: TObject);'
          'begin'
          '  TGlobal.Obj.Finalize;'
          '  TView.Obj.Remove(Self);'
          'end;'
          ''
          'procedure TfmMain.rp_Terminate(APacket: TValueList);'
          'begin'
          '  MessageDlg(APacket.Values['#39'Msg'#39'], mtInformation, [mbOk], 0);'
          '  Application.Terminate;'
          'end;'
          ''
          'end.')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsMainForm: TTabSheet
      Caption = 'MainForm'
      ImageIndex = 4
      object moMainForm: TMemo
        Left = 0
        Top = 0
        Width = 554
        Height = 273
        Align = alClient
        ImeName = 'Microsoft IME 2003'
        Lines.Strings = (
          'object fmMain: TfmMain'
          '  Left = 0'
          '  Top = 0'
          '  Caption = '#39'fmMain'#39
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
    object tsGlobal: TTabSheet
      Caption = 'Global'
      object moGlobal: TMemo
        Left = 0
        Top = 0
        Width = 554
        Height = 273
        Align = alClient
        ImeName = 'Microsoft IME 2003'
        Lines.Strings = (
          'unit Global;'
          ''
          'interface'
          ''
          'uses'
          '  ValueList,'
          '  Classes, SysUtils;'
          ''
          'type'
          '  TGlobal = class(TComponent)'
          '  strict private'
          '    FInitialized: boolean;'
          '  public'
          '    constructor Create(AOwner:TComponent); override;'
          '    destructor Destroy; override;'
          ''
          '    class function Obj:TGlobal;'
          ''
          '    procedure Initialize;'
          '    procedure Finalize;'
          '  published'
          '    property Initialized : boolean read FInitialized;'
          '  end;'
          ''
          'implementation'
          ''
          'uses'
          '  View, Option;'
          ''
          'var'
          '  MyObj : TGlobal = nil;'
          ''
          '{ TGlobal }'
          ''
          'constructor TGlobal.Create(AOwner:TComponent);'
          'begin'
          '  inherited;'
          ''
          '  FInitialized := false;'
          'end;'
          ''
          'destructor TGlobal.Destroy;'
          'begin'
          '  Finalize;'
          ''
          '  inherited;'
          'end;'
          ''
          'procedure TGlobal.Finalize;'
          'begin'
          '  if not FInitialized then Exit;'
          '  FInitialized := false;'
          ''
          '  // Todo : '
          'end;'
          ''
          'procedure TGlobal.Initialize;'
          'begin'
          '  if FInitialized then Exit;'
          ''
          '  // Todo : '
          ''
          '  FInitialized := true;'
          'end;'
          ''
          'class function TGlobal.Obj: TGlobal;'
          'begin'
          '  if MyObj = nil then MyObj := TGlobal.Create(nil);'
          '  Result := MyObj;'
          'end;'
          ''
          'end.'
          '')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsView: TTabSheet
      Caption = 'View'
      ImageIndex = 1
      object moView: TMemo
        Left = 0
        Top = 0
        Width = 554
        Height = 273
        Align = alClient
        ImeName = 'Microsoft IME 2003'
        Lines.Strings = (
          'unit View;'
          ''
          'interface'
          ''
          'uses'
          '  ObserverList, ValueList,'
          '  Classes, SysUtils;'
          ''
          'type'
          '  TView = class(TObserverList)'
          '  private'
          '  public'
          '    class function Obj:TView;'
          ''
          '    procedure sp_Terminate(Msg:string);'
          '  end;'
          ''
          'implementation'
          ''
          'var'
          '  MyObj : TView = nil;'
          ''
          '{ TView }'
          ''
          'class function TView.Obj: TView;'
          'begin'
          '  if MyObj = nil then MyObj := TView.Create(nil);'
          '  Result := MyObj;'
          'end;'
          ''
          'procedure TView.sp_Terminate(Msg: string);'
          'begin'
          '  Packet.Clear;'
          '  Packet.Values['#39'Code'#39'] := '#39'Terminate'#39';'
          '  Packet.Values['#39'Msg'#39'] := Msg;'
          '  BroadCast;'
          'end;'
          ''
          'end.'
          '')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsOption: TTabSheet
      Caption = 'Option'
      ImageIndex = 2
      object moOption: TMemo
        Left = 0
        Top = 0
        Width = 554
        Height = 273
        Align = alClient
        ImeName = 'Microsoft IME 2003'
        Lines.Strings = (
          'unit Option;'
          ''
          'interface'
          ''
          'uses'
          '  Classes, SysUtils;'
          ''
          'type'
          '  TOption = class(TComponent)'
          '  strict private'
          '  public'
          '    class function Obj:TOption;'
          '  end;'
          ''
          'implementation'
          ''
          'var'
          '  MyObj : TOption = nil;'
          ''
          '{ TOption }'
          ''
          'class function TOption.Obj: TOption;'
          'begin'
          '  if MyObj = nil then MyObj := TOption.Create(nil);'
          '  Result := MyObj;'
          'end;'
          ''
          'end.'
          '')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
end
