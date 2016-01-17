unit _fmMain;

interface

uses
  RyuGraphics,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, RadioDials,
  Vcl.Imaging.pngimage, Vcl.Tabs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TfmMain = class(TForm)
    Background: TShape;
    Mark: TShape;
    RadioDial1: TRadioDial;
    Image: TImage;
    procedure BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RadioDial1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  // 마우스 따라 다니기 테스트
  if (ssLeft in Shift) or (ssPen in Shift) then begin
    Mark.Left := X - Mark.Width  div 2;
    Mark.Top  := Y - Mark.Height div 2;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  RadioDial1.BackGround.Assign( Image.Picture.Bitmap );
end;

procedure TfmMain.RadioDial1Change(Sender: TObject);
begin
  Caption := IntToStr(RadioDial1.Position);
end;

end.
