unit ImageListView;

interface

uses
  SysUtils, Classes, Windows, Controls, Graphics, Messages, StdCtrls;

const
  ImageWidth : Integer = 150;
  ImageHeight : Integer = 150;

type
  TImageListItem = class
  public
    Thumbnail : TBitmap;
    Original : TBitmap;

    constructor Create;
    destructor Destroy; override;
  end;

  TImageListView = class(TWinControl)
  private
    FHorzScrollBar: TScrollBar;
    FItemList : TList;
    FSelectedItem: Integer;

    procedure ScrollBarChange(Sender: TObject);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure Draw(DC : HDC); overload;
    procedure Draw; overload;

    function ImageTotalWidth:Integer;
    procedure SetSelectedItem(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddPicture(Bitmap : TBitmap);
    procedure DeletePicture(Index : Integer); overload;
    procedure DeletePicture;overload;
  published
    property SelectedItem : Integer read FSelectedItem write SetSelectedItem;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ThinkPlant', [TImageListView]);
end;

{ TImageListView }

procedure TImageListView.Draw(DC: HDC);
var
  BackGroundBrush, OldBrush : HBRUSH;
  MemBitmapRect : TRect;
  MemBitmap, OldBitmap : HBitmap;
  MemBitmapWidth : Integer;
  I: Integer;
  TempItem : TImageListItem;
  FMemDC : HDC;

  SidePen, OldPen : HPen;//틀 그리기용
  SideBrush, OldSideBrush : HBrush;
begin
  FMemDC := CreateCompatibleDC(DC);
  try
    MemBitmapRect := GetClientRect;

    if FitemList.Count * ImageWidth < Width  then MemBitmapWidth := Width
    else MemBitmapWidth := ImageTotalWidth;
    if FHorzScrollBar.Visible = true then MemBitmapRect.Bottom := MemBitmapRect.Bottom - FHorzScrollBar.Height;
    


    MemBitmap := CreateCompatibleBitmap(DC, MemBitmapWidth, Height);
    OldBitmap := SelectObject(FMemDC, MemBitmap);

    MemBitmapRect.Right := MemBitmapRect.Left + MemBitmapWidth;

    BackGroundBrush := CreateSolidBrush(clWhite);
    try
      FillRect(FMemDC, MemBitmapRect, BackGroundBrush);

      for I := 0 to FItemList.Count - 1 do begin
        TempItem := TImageListItem(FitemList.Items[i]);
        SetStretchBltMode(FMemDC, HALFTONE);

        StretchBlt(FMemDC,(ImageWidth * i + 1) + i, 1, ImageWidth, ImageHeight,
          TempItem.Thumbnail.Canvas.Handle,
          0,0, TempItem.Thumbnail.Width, TempItem.Thumbnail.Height, SRCCOPY);

        if SelectedItem = i then begin
          SidePen := CreatePen(PS_SOLID, 2, clAqua);
          SideBrush := GetStockObject(HOLLOW_BRUSH);
          try
            OldPen := SelectObject(FMemDC, SidePen);
            OldSideBrush := SelectObject(FMemDC, SideBrush);
            Rectangle(FMemDC, (ImageWidth * i + 1) + i, 0, (ImageWidth * i + 2) + i + ImageWidth, ImageHeight+2);
            SelectObject(FMemDC, OldPen);
            SelectObject(FMemDC, OldSideBrush);
          finally
            DeleteObject(SidePen);
          end;
        end;
      end;

      BitBlt(DC, 0,0, Width, Height, FMemDC, FHorzScrollBar.Position ,0, SRCCOPY);
      FHorzScrollBar.Repaint;
    finally
      DeleteObject(BackGroundBrush);
    end;
  finally
    SelectObject(FMemDC, OldBitmap);
    DeleteObject(MemBitmap);
    ReleaseDC(Handle, FMemDC);
  end;
end;

procedure TImageListView.AddPicture(Bitmap: TBitmap);
var
  Item : TImageListItem;
begin
  Item := TImageListItem.Create;
  Item.Original.Width := Bitmap.Width;
  Item.Original.Height := Bitmap.Height;
  Item.Original.PixelFormat := Bitmap.PixelFormat;
  Item.Original.Canvas.Draw(0,0, Bitmap);

  Item.Thumbnail.Width := ImageWidth;
  Item.Thumbnail.Height := ImageHeight;
  Item.Thumbnail.PixelFormat := pfDevice;

  SetStretchBltMode(Item.Thumbnail.Canvas.Handle, HALFTONE);
  StretchBlt(Item.Thumbnail.Canvas.Handle, 0,0, ImageWidth, ImageHeight,
    Bitmap.Canvas.Handle, 0,0,Bitmap.Width, Bitmap.Height, SRCCOPY);

  FItemList.Add(Item);

  if ImageTotalWidth > Width then begin
    FHorzScrollBar.Visible := true;
    FHorzScrollBar.Max := ImageTotalWidth - Width ;
  end;

  Draw;
end;

constructor TImageListView.Create(AOwner: TComponent);
begin
  inherited;

  FSelectedItem := -1;

  FItemList := TList.Create;
  FHorzScrollBar := TScrollBar.Create(Self);
  FHorzScrollBar.Parent := Self;
  FHorzScrollBar.Align := alBottom;
  FHorzScrollBar.Visible := false;
  FHorzScrollBar.OnChange := ScrollBarChange;
end;

procedure TImageListView.DeletePicture(Index: Integer);
var
  I : TImageListItem;
begin
  try
    I := FItemList.Items[Index];
    FItemList.Delete(Index);
    I.Free;
  finally

  end;

  if ImageTotalWidth > Width then begin
    FHorzScrollBar.Visible := true;
    FHorzScrollBar.Max := ImageTotalWidth - Width;
    SelectedItem := -1;
  end;

  Draw;
end;

procedure TImageListView.DeletePicture;
begin
  if SelectedItem = -1 then exit;
  
  DeletePicture(SelectedItem);
end;

destructor TImageListView.Destroy;
begin
//  if FMemDC <> 0 then begin
//    SelectObject(FMemDC, FOldBitmap);
  //  ReleaseDC(Handle, FMemDC);
  //end;

  FHorzScrollBar.Free;
  FItemList.Free;

  inherited;
end;

procedure TImageListView.Draw;
var
  DC : HDC;
begin
  DC := GetDC(Handle);
  try
    Draw(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

function TImageListView.ImageTotalWidth: Integer;
begin
  Result := ((ImageWidth + 1) * FItemList.Count) + 1;
  //좌우에 1씩 Gap
end;

procedure TImageListView.ScrollBarChange(Sender: TObject);
begin
  Draw;
end;

procedure TImageListView.SetSelectedItem(const Value: Integer);
begin
  FSelectedItem := Value;
  Draw;
end;

procedure TImageListView.WMLButtonDown(var Message: TWMLButtonDown);
var
  Result : Integer;
begin
  Result := (Message.XPos + FHorzScrollBar.Position) div (ImageWidth + 1);
  SelectedItem := Result;
end;

procedure TImageListView.WMPaint(var Message: TWMPaint);
var
  DC : HDC;
  PS : TPaintStruct;
begin
  DC := BeginPaint(Handle, PS);
  try
    Draw(DC);
  finally
    EndPaint(Handle, PS);
  end;
end;

{ TImageListItem }

constructor TImageListItem.Create;
begin
  Original := TBitmap.Create;
  Thumbnail := TBitmap.Create;
end;

destructor TImageListItem.Destroy;
begin
  Original.Free;
  Thumbnail.Free;
end;

end.
