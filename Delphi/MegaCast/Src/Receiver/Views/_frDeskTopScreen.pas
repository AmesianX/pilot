unit _frDeskTopScreen;

interface

uses
  ValueList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TfrDeskTopScreen = class(TFrame)
    ScrollBox: TScrollBox;
    Image: TImage;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Global, View, Option;

{$R *.dfm}

constructor TfrDeskTopScreen.Create(AOwner: TComponent);
begin
  inherited;

  TView.Obj.Add(Self);
end;

destructor TfrDeskTopScreen.Destroy;
begin
  TView.Obj.Remove(Self);

  inherited;
end;

procedure TfrDeskTopScreen.TimerTimer(Sender: TObject);
begin
  if TGlobal.Obj.MegaCastPlayer.GetBitmap(Image.Picture.Bitmap) then Image.Repaint;
end;

end.
