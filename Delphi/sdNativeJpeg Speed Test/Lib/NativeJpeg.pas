//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  Creation Date: 24Apr2007
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
//  Description:
//
//  NativeJpeg is a library that provides full functionality for Jpeg images (*.jpg,
//  *.jpeg). It can be used as a replacement for Delphi's Jpeg unit. Just replace
//  "Jpeg" in the uses clause by "NativeJpeg", and add the source folder with
//  NativeJpeg's files to the search path of your project.
//
//  This unit contains class TsdJpegGraphic, a TGraphic descendant, so it works
//  like any TGraphic descendant, allowing to load jpeg files in TPicture / TImage,
//  and providing preview capability in TOpenPictureDialog and TSavePictureDialog.
//
//  The actual Jpeg encoding/decoding functionality is in files sdJpeg*.pas, most
//  notably sdJpegFormat.pas.
//

unit NativeJpeg;

interface

uses
  Windows, Classes, SysUtils, Graphics, sdJpegFormat, sdJpegTypes;

type

  TsdJpegPerformance = (
    jpBestQuality,
    jpBestSpeed
  );

  // TsdJpegGraphic is a Delphi TGraphic compatible class, which can be used
  // to load Jpeg files. It is recommended not to use this class directly in
  // your code, but the TsdJpegFormat class instead. This class is used to
  // provide preview capability inside TOpenPictureDialog. It provides very
  // fast (and good quality) preview and this class registers itself as a
  // TGraphic compatible class just by including "NativeJpeg" in your project.
  TsdJpegGraphic = class(TGraphic)
  private
    FData: TMemoryStream;
    FJpeg: TsdJpegFormat;
    FScale: TsdJpegScale;
    function GetPerformance: TsdJpegPerformance;
    procedure SetPerformance(const Value: TsdJpegPerformance);
    function GetGrayScale: boolean;
    procedure SetGrayScale(const Value: boolean);
    function GetCompressionQuality: TsdJpegQuality;
    procedure SetCompressionQuality(const Value: TsdJpegQuality);
  protected
    // Assign this TsdJpegGraphic to Dest. The only valid type for Dest is TBitmap.
    // The internal jpeg image will be loaded from the data stream at the correct
    // scale, then assigned to the bitmap in Dest.
    procedure AssignTo(Dest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure LoadJpeg(AScale: TsdJpegScale);
    class function GetVersion: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    // Use Assign to assign a TBitmap or other TsdJpegGraphic to this graphic. If
    // Source is a TBitmap, the TBitmap is assigned to the internal jpeg format class'
    // bitmap (see TsdJpegFormat.Bitmap property). If Source is another TsdJpegGraphic,
    // the data streams are copied and the internal Jpeg format is loaded from the
    // data. It is also possible to assign a TsdJpegGraphic to a TBitmap, like this:
    // <code>
    //   MyBitmap.Assign(MyJpegGraphic)
    // </code>
    // In that case, the protected AssignTo method is called.
    procedure Assign(Source: TPersistent); override;
    // Load a Jpeg graphic from the stream in Stream. Stream can be any stream
    // type, as long as the size of the stream is known in advance. The stream
    // should only contain *one* Jpeg graphic. The data in the stream is copied
    // to a local internal memory stream, called FData, and read from there by the
    // jpeg format.
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    property Performance: TsdJpegPerformance read GetPerformance write SetPerformance;
    property Scale: TsdJpegScale read FScale write FScale;
    property GrayScale: boolean read GetGrayScale write SetGrayScale;
    property CompressionQuality: TsdJpegQuality read GetCompressionQuality write SetCompressionQuality;
    // Version returns the current version of the NativeJpeg library.
    property Version: string read GetVersion;
  end;

implementation

uses
  sdBitmapResize;

type
  TJpegAccess = class(TsdJpegFormat);

{ TsdJpegGraphic }

procedure TsdJpegGraphic.Assign(Source: TPersistent);
begin
  if Source is TsdJpegGraphic then
  begin
    // Copy the data stream
    FData.Clear;
    FData.CopyFrom(TsdJpegGraphic(Source).FData, 0);
    FData.Position := 0;
    // Load the metadata for the image size
    FJpeg.LoadOptions := [loOnlyMetadata];
    FJpeg.LoadFromStream(FData);
  end else if Source is TBitmap then
  begin
    FJpeg.Bitmap := Source as TBitmap;
  end else
    inherited;
end;

procedure TsdJpegGraphic.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmap then
  begin
    LoadJpeg(FScale);
    (Dest as TBitmap).Assign(FJpeg.Bitmap);
  end else
    inherited;
end;

constructor TsdJpegGraphic.Create;
begin
  inherited;
  FJpeg := TsdJpegFormat.Create(nil);
  FJpeg.DCTCodingMethod := dmFast;
  FData := TMemoryStream.Create;
end;

destructor TsdJpegGraphic.Destroy;
begin
  FreeAndNil(FJpeg);
  FreeAndNil(FData);
  inherited;
end;

procedure TsdJpegGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  WReq, HReq, W, H: integer;
  S: TsdJpegScale;
  Dest: TBitmap;
  // Determine if size is acceptable
  function AcceptSize: boolean;
  begin
    TJpegAccess(FJpeg).GetBitmapSize(S, W, H);
    Result := (W >= WReq) and (H >= HReq);
  end;
begin
  // Determine correct scale
  WReq := Rect.Right - Rect.Left;
  HReq := Rect.Bottom - Rect.Top;
  // Check with which scale to load
  S := jsDiv8;
  while (S > jsFull) and not AcceptSize do
    dec(S);
  // Load jpeg with this scale
  LoadJpeg(S);
  if ((W = WReq) and (H = HReq)) or
     (W < WReq) or
     (H < HReq) then
  begin
    // Stretchdraw to canvas
    ACanvas.StretchDraw(Rect, FJpeg.Bitmap)
  end else
  begin
    // Use a fast downsizing algo
    Dest := TBitmap.Create;
    try
      Dest.PixelFormat := pf24bit;
      Dest.Width := WReq;
      Dest.Height := HReq;
      DownscaleBitmap(FJpeg.Bitmap, Dest);
      // Draw to canvas (since it's the right size now)
      ACanvas.Draw(Rect.Left, Rect.Top, Dest)
    finally
      Dest.Free;
    end;
  end;
end;

function TsdJpegGraphic.GetCompressionQuality: TsdJpegQuality;
begin
  Result := FJpeg.SaveOptions.Quality;
end;

function TsdJpegGraphic.GetEmpty: Boolean;
var
  W, H: integer;
begin
  TJpegAccess(FJpeg).GetBitmapSize(jsFull, W, H);
  Result := W * H = 0;
end;

function TsdJpegGraphic.GetGrayScale: boolean;
begin
  Result := FJpeg.BitmapColors = jcGray;
end;

function TsdJpegGraphic.GetHeight: Integer;
begin
  Result := FJpeg.ImageHeight;
end;

function TsdJpegGraphic.GetPerformance: TsdJpegPerformance;
begin
  if FJpeg.DCTCodingMethod = dmFast then
    Result := jpBestSpeed
  else
    Result := jpBestQuality;
end;

class function TsdJpegGraphic.GetVersion: string;
begin
  Result := cJpegVersion;
end;

function TsdJpegGraphic.GetWidth: Integer;
begin
  Result := FJpeg.ImageWidth;
end;

procedure TsdJpegGraphic.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
// not implemented
end;

procedure TsdJpegGraphic.LoadFromStream(Stream: TStream);
begin
  FJpeg.LoadOptions := [loOnlyMetadata];
  FData.LoadFromStream(Stream);
  FData.Position := 0;
  FJpeg.LoadFromStream(FData);
end;

procedure TsdJpegGraphic.LoadJpeg(AScale: TsdJpegScale);
var
  UseTileMode: boolean;
  Bmp: TBitmap;
  x, y, L, T, R, B, XCount, YCount: integer;
const
  cTileSize = 1024;
begin
  // Do we use tiled loading?
  UseTileMode := False;
{    (FJpeg.CodingInfo.EncodingMethod = emBaselineDCT) and
    (FJpeg.BitmapColors = jcRGB);}

  if UseTileMode then
    FJpeg.LoadOptions := [loTileMode]
  else
    FJpeg.LoadOptions := [];
  FJpeg.LoadScale := AScale;
  FData.Position := 0;
  FJpeg.LoadFromStream(FData);

  // Tile Mode when possible; this saves on memory usage for big jpeg files
  if UseTileMode then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.PixelFormat := pf24bit;
      Bmp.Width := FJpeg.Width;
      Bmp.Height := FJpeg.Height;
      // Since we use the Canvas.Draw method, we must lock the canvas
      Bmp.Canvas.Lock;
      try
        // Load tile-wise
        XCount := (FJpeg.Width + cTileSize - 1) div cTileSize;
        YCount := (FJpeg.Height + cTileSize - 1) div cTileSize;
        for y := 0 to YCount - 1 do
        begin
          for x := 0 to XCount - 1 do
          begin
            L := x * cTileSize;
            T := y * cTileSize;
            R := L + cTileSize;
            B := T + cTileSize;
            FJpeg.LoadTileBlock(FData, L, T, R, B);
            Bmp.Canvas.Draw(L, T, FJpeg.Bitmap);
          end;
        end;
      finally
        Bmp.Canvas.Unlock;
      end;
      // Assign back to Jpeg's bitmap
      FJpeg.Bitmap.Assign(Bmp);
      FJpeg.LoadOptions := [];
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TsdJpegGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
// not implemented
end;

procedure TsdJpegGraphic.SaveToStream(Stream: TStream);
begin
  FJpeg.SaveToStream(Stream);
end;

procedure TsdJpegGraphic.SetCompressionQuality(
  const Value: TsdJpegQuality);
begin
  FJpeg.SaveOptions.Quality := Value;
end;

procedure TsdJpegGraphic.SetGrayScale(const Value: boolean);
begin
  if Value then
    FJpeg.BitmapColors := jcGray
  else
    FJpeg.BitmapColors := jcRGB;
end;

procedure TsdJpegGraphic.SetHeight(Value: Integer);
begin
// not implemented
end;

procedure TsdJpegGraphic.SetPerformance(const Value: TsdJpegPerformance);
begin
  case Value of
  jpBestSpeed: FJpeg.DCTCodingMethod := dmFast;
  jpBestQuality: FJpeg.DCTCodingMethod := dmAccurate;
  end;
end;

procedure TsdJpegGraphic.SetWidth(Value: Integer);
begin
// not implemented
end;

initialization

  TPicture.RegisterFileFormat('jpg', 'Jpeg file', TsdJpegGraphic);

finalization

  TPicture.UnregisterGraphicClass(TsdJpegGraphic);

end.
