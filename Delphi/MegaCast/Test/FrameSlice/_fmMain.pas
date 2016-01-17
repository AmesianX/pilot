unit _fmMain;

interface

uses
  MegaCastUtils, FrameSlice, DeskTopCaptrue, 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SyncObjs, ExtCtrls;

type
  TfmMain = class(TForm)
    btStart: TButton;
    Panel1: TPanel;
    ScrollBox: TScrollBox;
    Image: TImage;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FBitmap : TBitmap;
    FFrameSlice : TFrameSlice;
    FCaptrue : TDeskTopCaptrue;
    FCS : TCriticalSection;
    function on_NeedFrame(Sender:TObject; var AData:pointer; var ASize:integer; var AFrameSize:TFrameSize):boolean;
    procedure on_NewBlockUnit(Sender:TObject; ABlockUnit:pointer; AUnitSize:integer);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btStartClick(Sender: TObject);
begin
  FCaptrue.Start;
  FFrameSlice.Start;
  Timer.Enabled := true;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.PixelFormat := _PixelFormat;
  Image.Picture.Bitmap.Width  := 1024;
  Image.Picture.Bitmap.Height := 768;

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := _PixelFormat;
  FBitmap.Width  := 1024;
  FBitmap.Height := 768;

  FCaptrue := TDeskTopCaptrue.Create;
  FCaptrue.SetCaptrueRect(Rect(0, 0, 1024, 768));

  FFrameSlice := TFrameSlice.Create(Self);
  FFrameSlice.OnNeedFrame := on_NeedFrame;
  FFrameSlice.OnNewBlockUnit := on_NewBlockUnit;

  FFrameSlice.SetFrameSize(FrameSize(1024, 768));

  FCS := TCriticalSection.Create;
end;

function TfmMain.on_NeedFrame(Sender: TObject; var AData: pointer;
  var ASize: integer; var AFrameSize: TFrameSize): boolean;
begin
  Result := FCaptrue.GetFrame(AData, ASize, AFrameSize);
end;

procedure TfmMain.on_NewBlockUnit(Sender: TObject; ABlockUnit: pointer;
  AUnitSize: integer);
begin
  FCS.Enter;
  try
    DrawBlockOnBitmap(ABlockUnit, AUnitSize, FBitmap);
  finally
    FCS.Leave;
  end;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  FCS.Enter;
  try
    Move(
      FBitmap.ScanLine[FBitmap.Height-1]^,
      Image.Picture.Bitmap.ScanLine[FBitmap.Height-1]^,
      FBitmap.Width*FBitmap.Height*_PixelSize
    );
    Image.Repaint;
  finally
    FCS.Leave;
  end;
end;

end.
