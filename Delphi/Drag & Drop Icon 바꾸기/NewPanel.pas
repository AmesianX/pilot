unit NewPanel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TNewPanel = class(Vcl.ExtCtrls.TPanel)
  protected
    function GetDragImages: TDragImageList; override;
  private
    FDragImageList : TDragImageList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TNewPanel }

constructor TNewPanel.Create(AOwner: TComponent);
begin
  inherited;

  Caption := 'xxxx';
  DragCursor := crNone;

  FDragImageList := TDragImageList.Create(Self);
end;

destructor TNewPanel.Destroy;
begin

  inherited;
end;

function TNewPanel.GetDragImages: TDragImageList;
var
  XImage: TBitmap;
begin
  Result := FDragImageList;

  XImage := TBitmap.Create;
  try
    Result.GetBitmap(0, XImage);
    Result.Clear;
    Result.Height := 100;
    Result.Width := 100;
    XImage.Height := Result.Height;
    XImage.Width := Result.Width;
    XImage.Canvas.Brush.Color := clRed;
    XImage.Canvas.FillRect(Rect(0, 0, 100, 100));
    Result.Add(XImage, nil);
  finally
    XImage.Free;
  end;
end;

end.
