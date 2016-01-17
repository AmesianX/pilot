unit _fmAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SwitchButton;

type
  TfmAbout = class(TForm)
    Panel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    LabelURL: TLabel;
    LabelVersion: TLabel;
    btOk: TSwitchButton;
    Label5: TLabel;
    procedure btOkChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelURLMouseEnter(Sender: TObject);
    procedure LabelURLMouseLeave(Sender: TObject);
    procedure LabelURLClick(Sender: TObject);
    procedure LabelURLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelURLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowAboutDlg;

implementation

uses
  Disk, gSysInfo;

var
  fmAbout: TfmAbout;

{$R *.dfm}

procedure ShowAboutDlg;
begin
  fmAbout := TfmAbout.Create(nil);
  try
    fmAbout.ShowModal;
  finally
    FreeAndNil(fmAbout);
  end;
end;

procedure TfmAbout.btOkChanged(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  Panel.DoubleBuffered := True;
  LabelVersion.Caption := GetAppVersion;
end;

procedure TfmAbout.LabelURLClick(Sender: TObject);
begin
  ShellExecuteFile('http://megachannel.co.kr', '', '');
end;

procedure TfmAbout.LabelURLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with LabelURL do
  begin
    Left := 41;
    Top := 180;
  end;
end;

procedure TfmAbout.LabelURLMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with LabelURL do
  begin
    Left := 40;
    Top := 179;
  end;
end;

procedure TfmAbout.LabelURLMouseEnter(Sender: TObject);
begin
  LabelURL.Font.Color := clDkGray;
end;

procedure TfmAbout.LabelURLMouseLeave(Sender: TObject);
begin
  LabelURL.Font.Color := $00666666;
end;

end.
