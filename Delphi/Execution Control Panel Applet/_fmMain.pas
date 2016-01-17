unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  ShellAPI;

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  ShellExecute (Handle, nil, 'control.exe', 'mmsys.cpl,,1', '', SW_SHOWNORMAL);
//  ShellExecute (Handle, nil, 'mmsys.cpl', '', '', SW_SHOWNORMAL);
//  ShellExecute (Handle, nil, 'Firewall.cpl', nil, nil, SW_SHOWNORMAL);
//  ShellExecute (Handle, nil, 'desk.cpl', nil, nil, SW_SHOWNORMAL);
//  ShellExecute (Handle, nil, 'sysdm.cpl', nil, nil, SW_SHOWNORMAL);
//  ShellExecute (Handle, nil, 'timedate.cpl', nil, nil, SW_SHOWNORMAL);
//  ShellExecute (Handle, nil, 'Appwiz.cpl', nil, nil, SW_SHOWNORMAL);
//  ShellExecute (Handle, nil, 'odbccp32.cpl', nil, nil, SW_SHOWNORMAL);
end;

end.
