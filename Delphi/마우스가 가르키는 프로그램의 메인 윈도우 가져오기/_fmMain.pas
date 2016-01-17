unit _fmMain;

interface

uses
  Disk,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.TimerTimer(Sender: TObject);
var
  MousePoint : TPoint;
  ParentWnd, TargetWnd : THandle;
  Title, FileName : string;
  Len : integer;
begin
  GetCursorPos(MousePoint);
  TargetWnd := WindowFromPoint(MousePoint);

  ParentWnd := GetParent(TargetWnd);
  while ParentWnd <> 0 do begin
    TargetWnd := ParentWnd;
    ParentWnd := GetParent(TargetWnd);
  end;

  FileName := ExtractFileName(GetProcessNameFromWnd(TargetWnd));

  Len := GetWindowTextLength(TargetWnd) + 1;
  SetLength(Title, Len);
  GetWindowText(TargetWnd, PChar(Title), Len);

  Caption := Format('%s, %s', [FileName, Title]);
end;

end.
