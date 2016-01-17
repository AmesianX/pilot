unit _frCam;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrCam = class(TFrame)
    ChkCamOn: TCheckBox;
    procedure ChkCamOnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  _fmMain, Global, Option;

{$R *.dfm}

constructor TfrCam.Create(AOwner: TComponent);
begin
  inherited;

  TGlobal.Obj.CamClient.Parent := Self;
end;

procedure TfrCam.ChkCamOnClick(Sender: TObject);
begin
  TGlobal.Obj.CamClient.ActiveSendData := TCheckBox(Sender).Checked;
end;

end.

