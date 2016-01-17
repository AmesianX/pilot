//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegFormat;

interface

uses
  {$IFDEF TRIALJPG}
  Dialogs,
  {$ENDIF}
  Windows, Graphics, Classes, Contnrs, SysUtils, sdJpegMarkers, sdJpegBitstream,
  sdJpegConsts, sdJpegTypes, Math, sdMapIterator, sdBitmapConversion,
  sdJpegLossless, sdJpegColors;

const

  // Version number changes with updates. See "versions.txt" for a list of
  // updated features.
  cJpegVersion = '1.15';

type

  TsdJpegDebugEvent = procedure (Sender: TObject; const AMessage: string) of object;
  TsdJpegProvideStripEvent = procedure (Sender: TObject; ALeft, ATop: integer; ABitmap: TBitmap) of object;
  TsdJpegExternalCMEvent = procedure (Sender: TObject; var ABitmap: TBitmap) of object;

  TsdJpegCoder = class;
  TsdJpegSaveOptions = class;

  TsdJpegLoadOption = (
    loOnlyMetadata, // If set, only meta-data is read (exits when SOS is encountered)
    loTileMode      // If set, the loadfromstream only finds the start of each MCU tile
  );
  TsdJpegLoadOptions = set of TsdJpegLoadOption;

  // TsdJpegFormat is a non-visual component that can be used to load and save
  // Jpeg files. It is best to create just one instance of TsdJpegFormat, and
  // use it over and over again to load Jpeg files, because this way memory
  // for the coefficients and bitmaps will be reused, instead of repeatedly
  // allocated/deallocated (which would happen if you each time create a new
  // TsdJpegFormat instance).
  // Use the LoadFromFile or LoadFromStream method to load a Jpeg image, and use
  // the SaveToFile and SaveToStream method to save a Jpeg image. Use the Bitmap
  // property to assign the bitmap to another bitmap, or to work with the actual
  // image. The StoredColors and BitmapColors properties provide information and
  // control over which colour spaces and conversions are used. The Lossless property
  // gives access to a TsdLosslessOperation class with which you can perform
  // lossless operations on the Jpeg. The SaveOptions property gives access to
  // options used when saving the Jpeg.
  TsdJpegFormat = class(TComponent)
  private
    FCoder: TsdJpegCoder;
    FMarkers: TsdJpegMarkerList;
    FInfo: TsdJpegCodingInfo;
    FLoadOptions: TsdJpegLoadOptions;
    FLoadScale: TsdJpegScale;
    FBitmap: TBitmap;
    FICCProfile: TsdJpegICCProfile;
    FStoredColors: TsdJpegColorSpace;
    FBitmapColors: TsdJpegColorSpace;
    FDCTCodingMethod: TsdJpegDCTCodingMethod;
    FLossless: TsdLosslessOperation;
    FSaveOptions: TsdJpegSaveOptions;
    FOnDebugOut: TsdJpegDebugEvent;
    FOnProvideStrip: TsdJpegProvideStripEvent;
    FOnExternalCM: TsdJpegExternalCMEvent;
    function GetExifInfo: TsdEXIFMarker;
    function GetIptcInfo: TsdIPTCMarker;
    function GetJfifInfo: TsdJFIFMarker;
    function GetBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
    function GetAdobeAPP14Info: TsdAdobeApp14Marker;
    function GetICCProfile: TsdJpegICCProfile;
    procedure SetICCProfile(const Value: TsdJpegICCProfile);
    function GetLossless: TsdLosslessOperation;
    function GetComment: string;
    procedure SetComment(const Value: string);
    class function GetVersion: string;
    function GetHeight: integer;
    function GetImageHeight: integer;
    function GetImageWidth: integer;
    function GetWidth: integer;
  protected
    procedure EntropyDecodeSkip(S: TStream);
    procedure InitializeDecode;
    // Returns the tag of last marker read
    function ReadMarkers(S: TStream): byte;
    // Get the required color transform from bitmap to samples, based on detected
    // color space in file and bitmap
    procedure GetColorTransformFromBitmap(var AClass: TsdColorTransformClass);
    // Get the required color transform from samples to bitmap, based on detected
    // color space in file and bitmap
    procedure GetColorTransformToBitmap(var AClass: TsdColorTransformClass;
      var AFormat: TPixelFormat);
    class function GetDivisor(AScale: TsdJpegScale): integer;
    // Get the size of the bitmap that must be created to hold the decoded
    // information
    procedure GetBitmapSize(AScale: TsdJpegScale; var AWidth, AHeight: integer);
    procedure EncodeBitmap;
    procedure Clear;
    procedure DoDebugOut(const AMessage: string);
    function HasSamples: boolean;
    function HasCoefficients: boolean;
    function VerifyBitmapColorSpaceForSave: TsdJpegColorSpace;
    procedure AddMinimalMarkersForColorSpaceDetection(AColors: TsdJpegColorSpace);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Load a Jpeg image from a file with name AFileName. Internally, the file
    // is copied to a TMemoryStream which is then loaded with LoadFromStream.
    procedure LoadFromFile(const AFileName: string);
    // Load a Jpeg image from the stream S. It is best to use a TMemoryStream
    // because the bitreader (which causes most reads to the stream) has
    // a specially optimized version to read from TMemoryStream streams. The
    // stream S will be read from from S.Position, and if everything goes well,
    // the stream will be positioned directly after the last (EOI) marker. An
    // exception will be raised if the stream is corrupt or truncated.
    procedure LoadFromStream(S: TStream);
    // In case of LoadOption [loTileMode] is included, after the LoadFromStream,
    // individual tile blocks can be loaded which will be put in the resulting
    // bitmap. The tile loaded will contain all the MCU blocks that fall within
    // the specified bounds ALeft/ATop/ARight/ABottom. Note that these are var
    // parameters, after calling this procedure they will be updated to the MCU
    // block borders. ALeft/ATop can subsequently be used to draw the resulting
    // TsdJpegFormat.Bitmap to a canvas.
    procedure LoadTileBlock(S: TStream; var ALeft, ATop, ARight, ABottom: integer);
    // Save the Jpeg image to a file with AFileName. If AFileName exists, it
    // will be overwritten.
    procedure SaveToFile(const AFileName: string);
    // Save the Jpeg image to stream S. Set SaveOptions before saving. Assign the
    // bitmap to be saved to the Bitmap property up front.
    procedure SaveToStream(S: TStream);
    // Save a Jpeg image to stream S. No bitmap needs to be provided up front;
    // but the event OnProvideStrip must be implemented to fill one strip of the
    // jpeg file at a time (a strip consists of a row of MCU blocks, usually
    // 8 or 16 pixels high).
    // Only one pass over the data is required, but the resulting jpeg will
    // be saved with standard Huffman tables (as provided in the Jpeg specification)
    procedure SaveToStreamStripByStrip(S: TStream; AWidth, AHeight: integer);
    // All metadata markers will be extracted from the file, and put in AList.
    // AList must be initialized (AList := TsdJpegMarkerList.Create)
    procedure ExtractMetadata(AList: TsdJpegMarkerList);
    // Inject the metadata in AList into the marker list of the file. All existing
    // metadata will be removed first; then the markers in AList will be added
    // below the SOI marker.
    procedure InjectMetadata(AList: TsdJpegMarkerList);
    // Returns true if the Jpeg has a valid ICC profile. Use property ICCProfile
    // to actually get it.
    function HasICCProfile: boolean;
    // Call this function to detect what colorspace is used in the file. This
    // can only be detected *after* the file is loaded. Set LoadOptions to
    // [loOnlyMetadata] to quickly do a test load to use this function.
    function DetectStoredColorSpace: TsdJpegColorSpace;
    // Reference to low-level information of file. CodingInfo.Width/Height provides
    // the size of the coded image.
    property CodingInfo: TsdJpegCodingInfo read FInfo;
    // Reference to the Jpeg coder. TsdJpegCoder is a generic coder, and specific
    // implementations are present for baseline DCT and progressive DCT.
    property Coder: TsdJpegCoder read FCoder;
    // Reference to the list of low-level markers present in the file (valid after
    // loading).
    property Markers: TsdJpegMarkerList read FMarkers;
    // Perform lossless operations on the jpeg coefficients
    property Lossless: TsdLosslessOperation read GetLossless;
  published
    // Pointer to JFIF info marker (if any)
    property JfifInfo: TsdJFIFMarker read GetJfifInfo;
    // Pointer to EXIF info marker (if any)
    property ExifInfo: TsdEXIFMarker read GetExifInfo;
    // Pointer to IPTC info marker (if any)
    property IptcInfo: TsdIPTCMarker read GetIptcInfo;
    // Pointer to Adobe APP14 info marker (if any)
    property AdobeAPP14Info: TsdAdobeApp14Marker read GetAdobeAPP14Info;
    // Read ICCProfile to get a TsdJpegICCProfile object back, in case ICC profile
    // data is available in the markers. The object has a Data pointer and a
    // DataLength property, and it can be used with e.g. LittleCMS. The profile
    // is valid until you load a new file or free the jpeg component.
    property ICCProfile: TsdJpegICCProfile read GetICCProfile write SetICCProfile;
    // Read and write a Jpeg comment (implemented through the COM marker).
    property Comment: string read GetComment write SetComment;
    // Read Bitmap to get a pointer to a TBitmap object back, that has the currently
    // loaded image. The TBitmap object is no longer valid when the jpeg format class is freed.
    // Assign to Bitmap in order to save the bitmap to a jpeg file. Note: when
    // assigning a new bitmap, the metadata is destroyed. In order to preserve
    // metadata, use a construct like this:
    // <code>
    // List := TsdJpegMarkerList.Create;
    // Jpg.ExtractMetadata(List);
    // Jpg.Bitmap := MyBitmap;
    // Jpg.InjectMetadata(List);
    // Jpg.SaveToFile('test.jpg');
    // List.Free;
    // </code>
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property ImageWidth: integer read GetImageWidth;
    property ImageHeight: integer read GetImageHeight;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    // Include loOnlyMetadata if you want to only load metadata and not decode
    // the image. Include loTileMode to load the image first, without decoding
    // it directly, so that LoadTileBlock can be used to load tiles of the image.
    property LoadOptions: TsdJpegLoadOptions read FLoadOptions write FLoadOptions;
    // Set LoadScale to anything other than jsFull to load a downscaled image, which
    // will be faster than the full image. jsDiv2 will download an image that is
    // half the size in X and Y, jsDiv4 will be quarter size, jsDiv8 will be one
    // eight size.
    property LoadScale: TsdJpegScale read FLoadScale write FLoadScale;
    // The colorspace present in the file. If jcAutoDetect (default), the software will
    // try to detect which colorspace the JPEG file contains. This info is present
    // in some markers, or in the frame definition. If set to another value,
    // the software will use this as given, and assume that the data in the file
    // is using the given colorspace.
    property StoredColors: TsdJpegColorSpace read FStoredColors write FStoredColors;
    // The colorspace that will be generated when outputting to a bitmap. If
    // jcAutoDetect, the stored color space present in the file will be used to
    // directly output the data without color conversion. Default value for
    // BitmapColors is jcRGB.
    property BitmapColors: TsdJpegColorSpace read FBitmapColors write FBitmapColors;
    // Method used for forward and inverse DCT transform. dmAccurate will use an
    // accurate integer method (slow), dmFast will use fast but less accurate
    // integer method. When reading a Jpeg, this only impacts the visual quality.
    // When writing Jpeg, the resulting file quality will be impacted by the
    // method chosen.
    property DCTCodingMethod: TsdJpegDctCodingMethod read FDCTCodingMethod write FDCTCodingMethod;
    // Options that influence how Jpeg images are compressed and saved.
    property SaveOptions: TsdJpegSaveOptions read FSaveOptions;
    // Version of the NativeJpg library (readonly)
    property Version: string read GetVersion;
    // Connect to this event to get low-level textual debug information
    property OnDebugOut: TsdJpegDebugEvent read FOnDebugOut write FOnDebugOut;
    // Connect to this event before calling SaveToStreamStripByStrip. The implementation
    // of this event should fill the passed ABitmap parameter with the part of the
    // image at ALeft/ATop position.
    property OnProvideStrip: TsdJpegProvideStripEvent read FOnProvideStrip write FOnProvideStrip;
    // Connect to this event to do external color management (e.g. apply an ICC profile
    // to an image, with an external library (like LittleCMS). If this event is
    // implemented, no internal color transforms are applied.
    property OnExternalCM: TsdJpegExternalCMEvent read FOnExternalCM write FOnExternalCM;
  end;

  // Generic coder class. This is the base class for special coders, like
  // TsdJpegBaselineCoder and TsdJpegProgressiveCoder.
  TsdJpegCoder = class(TPersistent)
  private
    FOnDebugOut: TsdJpegDebugEvent;
  protected
    FInfo: TsdJpegCodingInfo; // reference to jpeg coding info
    FMethod: TsdJpegDCTCodingMethod; // fast or accurate
    FHasCoefs: boolean;
    FHasSamples: boolean;
    FScale: TsdJpegScale;
    FTileMode: boolean;
    procedure DoDebugOut(const AMessage: string);
  public
    constructor Create(AInfo: TsdJpegCodingInfo); virtual;
    procedure Clear; virtual;
    procedure Initialize(AScale: TsdJpegScale); virtual;
    procedure Encode(S: TStream); virtual;
    procedure Decode(S: TStream); virtual;
    procedure DecodeBlock(S: TStream; XStart, YStart, XCount, YCount: integer); virtual;
    procedure Finalize; virtual;
    procedure ForwardDCT; virtual;
    procedure InverseDCT; virtual;
    // Get the values from the image described with map iterator AImage, and put
    // them in the sample maps. Use ATransform to transform the colors.
    procedure SamplesFromImage(AImage: TsdMapIterator; ATransform: TsdColorTransform); virtual; abstract;
    // Build the image that is described with the map iterator AImage, based on the
    // decoded samples. Transform the decoded samples color space to the image color
    // space with ATransform.
    procedure SamplesToImage(AImage: TsdMapIterator; ATransform: TsdColorTransform); virtual; abstract;
    procedure BuildMarkerList(AList: TsdJpegMarkerList); virtual;
    function CreateDHTMarker: TsdDHTMarker; virtual;
    property Method: TsdJpegDCTCodingMethod read FMethod write FMethod;
    property HasCoefs: boolean read FHasCoefs write FHasCoefs;
    property HasSamples: boolean read FHasSamples write FHasSamples;
    property OnDebugOut: TsdJpegDebugEvent read FOnDebugOut write FOnDebugOut;
    property Scale: TsdJpegScale read FScale;
    property TileMode: boolean read FTileMode write FTileMode;
  end;

  TsdJpegSaveOptions = class(TPersistent)
  private
    FOwner: TsdJpegFormat;
    FQuality: TsdJpegQuality;
    FOptimizeHuffmanTables: boolean;
    FCodingMethod: TsdJpegEncodingMethod;
    FUseSubSampling: boolean;
  protected
    procedure AddMarkers(AStored: TsdJpegColorSpace; AWidth, AHeight: integer);
    procedure SetupDefaultHuffmanTables; virtual;
    procedure SetupQuantTables; virtual;
    procedure SetTableMultiplication(ATable: TsdQuantizationTable; MultiplyPercent: integer;
      const ADefaultTable: TsdIntArray64);
  public
    constructor Create(AOwner: TsdJpegFormat);
    property Quality: TsdJpegQuality read FQuality write FQuality;
    property OptimizeHuffmanTables: boolean read FOptimizeHuffmanTables write FOptimizeHuffmanTables;
    property CodingMethod: TsdJpegEncodingMethod read FCodingMethod write FCodingMethod;
    property UseSubSampling: boolean read FUseSubSampling write FUseSubSampling;
  end;

implementation

uses
  sdJpegHuffman, sdJpegBaseline, sdJpegProgressive;

{ TsdJpegFormat }

procedure TsdJpegFormat.AddMinimalMarkersForColorSpaceDetection(AColors: TsdJpegColorSpace);
var
  M: TsdJpegMarker;
begin
  Markers.Insert(0, TsdSOIMarker.Create(FInfo, mkSOI));
  // JFIF marker if these color spaces
  if AColors in [jcGray, jcYCbCr] then
    Markers.Insert(1, TsdJFIFMarker.Create(FInfo, mkAPP0))
  else
    // Adobe APP14 marker if these color spaces
    if AColors in [jcRGB, jcCMYK, jcYCCK] then
    begin
      M := TsdAdobeAPP14Marker.Create(FInfo, mkAPP14);
      case AColors of
      jcRGB, jcCMYK: TsdAdobeAPP14Marker(M).Transform := 0;
      jcYCCK: TsdAdobeAPP14Marker(M).Transform := 2;
      end;
      Markers.Insert(1, M);
    end;
end;

procedure TsdJpegFormat.Clear;
begin
  // Clear any lists/objects we have
  FMarkers.Clear;
  FInfo.Clear;
  if assigned(FCoder) then FCoder.Clear;
  if assigned(FLossless) then FLossless.Clear;
  // We free the bitmap and color profile
  FreeAndNil(FICCProfile);
end;

constructor TsdJpegFormat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Owned objects
  FMarkers := TsdJpegMarkerList.Create(Self);
  FInfo := TsdJpegCodingInfo.Create;
  FSaveOptions := TsdJpegSaveOptions.Create(Self);
  // Defaults
  FStoredColors := jcAutoDetect;
  FBitmapColors := jcRGB;
  FDctCodingMethod := dmAccurate;
end;

destructor TsdJpegFormat.Destroy;
begin
  FreeAndNil(FSaveOptions);
  FreeAndNil(FCoder);
  FreeAndNil(FBitmap);
  FreeAndNil(FICCProfile);
  FreeAndNil(FInfo);
  FreeAndNil(FMarkers);
  FreeAndNil(FLossless);
  inherited;
end;

function TsdJpegFormat.DetectStoredColorSpace: TsdJpegColorSpace;
var
  JFIF: TsdJFIFMarker;
  EXIF: TsdEXIFMarker;
  Adobe: TsdAdobeApp14Marker;
  IDStr: string;
  // local
  function GetComponentIDString: string;
  var
    i: integer;
  begin
    SetLength(Result, FInfo.FrameCount);
    for i := 0 to FInfo.FrameCount - 1 do
      Result[i + 1] := Chr(FInfo.Frames[i].ComponentID);
  end;
begin
  // Defaults: Based on component count
  Result := jcAutoDetect;
  case FInfo.FrameCount of
  1: Result := jcGray;
  2: Result := jcGrayA;
  3: Result := jcYCbCr;
  4: Result := jcYCCK;
  end;

  // Check JFIF marker
  JFIF := GetJFIFInfo;
  if assigned(JFIF) and JFIF.IsValid then
    // We have a JFIF marker: if component count is 1 or 3, above assumptions are correct
    if FInfo.FrameCount in [1, 3] then
      exit;

  // Check Adobe APP14 marker
  Adobe := GetAdobeAPP14Info;
  if assigned(Adobe) and Adobe.IsValid then
  begin
    // We have an Adobe APP14 marker
    case Adobe.Transform of
    0:
      begin
        case FInfo.FrameCount of
        3: Result := jcRGB;
        4: Result := jcCMYK;
        end;
      end;
    1: Result := jcYCbCr;
    2: Result := jcYCCK;
    end;
    exit;
  end;

  // Check for ITU G3FAX format
  EXIF := GetEXIFInfo;
  if assigned(EXIF) and EXIF.IsG3Fax then
  begin
    Result := jcITUCieLAB;
    exit;
  end;

  // No subsampling used?
  if (FInfo.HorzSamplingMax = 1) and (FInfo.HorzSamplingMax = 1) then
  begin
    // No subsampling used -> Change YC method to RGB or CMYK
    case FInfo.FrameCount of
    3: Result := jcRGB;
    4: Result := jcCMYK;
    end;
  end;

  // Use component ID's
  IDStr := GetComponentIDString;
  case FInfo.FrameCount of
  3:
    begin
      // Possible ID strings
      if IDStr = #1#2#3 then
        Result := jcYCbCr;
      if IDStr = 'RGB' then
        Result := jcRGB;
      if IDStr = 'YCc' then
        Result := jcPhotoYCC;
    end;
  4:
    begin
      // Possible ID strings
      if IDStr = #1#2#3#4 then
      begin
        if HasICCProfile then
          // Note: in fact, in cases seen, this represents CMYK instead of RGBA,
          // so to decode: decode to RGBA as usual, then pretend these channels
          // are CMYK, and convert to final colour space (seen in scanners, and
          // always with ICC profile present - which has CMYK profile)
          Result := jcYCbCrK
        else
          Result := jcYCbCrA;
      end;
      if IDStr = 'RGBA' then
        Result := jcRGBA;
      if IDStr = 'YCcA' then
        Result := jcPhotoYCCA;
    end;
  end;
end;

procedure TsdJpegFormat.DoDebugOut(const AMessage: string);
begin
  if assigned(FOnDebugOut) then FOnDebugOut(Self, AMessage);
end;

procedure TsdJpegFormat.EncodeBitmap;
// Here we can assume that Info fields are already set
var
  TransformClass: TsdColorTransformClass;
  Transform: TsdColorTransform;
  BmpIter: TsdMapIterator;
  Stored: TsdJpegColorSpace;
begin
  // If no coder yet, we create the baseline coder (for now)
  if not assigned(FCoder) then
    FCoder := TsdJpegBaselineCoder.Create(FInfo);

  // Verify incoming bitmap format versus bitmap colorspace
  Stored := VerifyBitmapColorSpaceForSave;

  // We create minimal default markers to warrant color space detection
  // later on
  AddMinimalMarkersForColorSpaceDetection(Stored);

  // Ask save options to add DQT, SOFn and SOS marker
  FSaveOptions.AddMarkers(Stored, FBitmap.Width, FBitmap.Height);

  // Color transform
  GetColorTransformFromBitmap(TransformClass);
  if not assigned(TransformClass) then
    raise EInvalidGraphic.Create(sInvalidFormatForSelectedCS);

  Transform := TransformClass.Create;
  BmpIter := TsdMapIterator.Create;
  try
    // Get iterator
    GetBitmapIterator(FBitmap, BmpIter);
    // Initialize coder (this sets map sizes etc)
    FCoder.Initialize(jsFull);
    // Get samples from bitmap data
    FCoder.SamplesFromImage(BmpIter, Transform);
    // Now convert samples to coefficients. This also does the quantization
    FCoder.ForwardDCT;
  finally
    BmpIter.Free;
    Transform.Free;
  end;

  // We also must add an EOI marker
  FMarkers.Add(TsdEOIMarker.Create(FInfo, mkEOI));
end;

procedure TsdJpegFormat.EntropyDecodeSkip(S: TStream);
// In case we want to skip the entropy-encoded stream, but inspect markers
var
  B, ReadBytes, Tag: byte;
  First, Last, P: PByte;
begin
  if S is TMemoryStream then
  begin
    // Fast skip based on memorystream
    First := TMemoryStream(S).Memory;
    Last := First;
    inc(Last, S.Size);
    P := First;
    inc(P, S.Position);
    while cardinal(P) < cardinal(Last) do begin
      // Scan stream for $FF<marker>
      if P^ = $FF then begin
        inc(P);
        if P^ <> 0 then begin
          dec(P, 1);
          S.Position := cardinal(P) - cardinal(First);
          exit;
        end;
      end;
      inc(P);
    end;
  end else
  begin
    // Slow skip for general streams
    repeat
      ReadBytes := S.Read(B, 1);
      if B = $FF then begin
        S.Read(Tag, 1);
        if Tag <> 0 then begin
          S.Seek(-2, soFromCurrent);
          exit;
        end;
      end;
    until ReadBytes = 0;
  end;
end;

procedure TsdJpegFormat.ExtractMetadata(AList: TsdJpegMarkerList);
var
  Idx: integer;
begin
  Idx := 0;
  while Idx < FMarkers.Count do
  begin
    if FMarkers[Idx].Tag in [mkAPP0..mkAPP15, mkCOM] then
      AList.Add(FMarkers.Extract(FMarkers[Idx]))
    else
      inc(Idx);
  end;
end;

function TsdJpegFormat.GetAdobeAPP14Info: TsdAdobeApp14Marker;
begin
  Result := TsdAdobeAPP14Marker(FMarkers.ByTag(mkApp14));
end;

function TsdJpegFormat.GetBitmap: TBitmap;
var
  BmpIter: TsdMapIterator;
  Transform: TsdColorTransform;
  TransformClass: TsdColorTransformClass;
  PixelFormat: TPixelFormat;
  BmpWidth, BmpHeight: integer;
begin
  if not assigned(FBitmap) then
    FBitmap := TBitmap.Create;
  Result := FBitmap;

  // If we do not have coefficients we have not loaded anything yet, so exit
  if not HasCoefficients then
    exit;

  // Do we need to update the bitmap?
  if not HasSamples then
  begin
    GetColorTransformToBitmap(TransformClass, PixelFormat);
    if TransformClass = nil then
      raise EInvalidGraphic.Create(sNoColorTransformation);
    Transform := TransformClass.Create;
    FBitmap.PixelFormat := PixelFormat;
    BmpIter := TsdMapIterator.Create;
    try
      // Inverse-DCT the coefficients, this also does the unquantization
      FCoder.InverseDCT;
      // Find the bitmap size based on decoded info and required scale
      GetBitmapSize(FLoadScale, BmpWidth, BmpHeight);
      // Set bitmap size and pixelformat, and get the iterator to pass as argument
      FBitmap.Width := BmpWidth;
      FBitmap.Height := BmpHeight;
      if PixelFormat = pf8bit then
        SetBitmap8bitGrayscale(FBitmap);
      GetBitmapIterator(FBitmap, BmpIter);
      // Ask the coder to put the samples in the bitmap
      FCoder.SamplesToImage(BmpIter, Transform);
    finally
      Transform.Free;
      BmpIter.Free;
    end;
    FCoder.HasSamples := True;

    // Defer color management to to application, if OnExternalCM is implemented
    if assigned(FOnExternalCM) then
    begin
      FOnExternalCM(Self, FBitmap);
      Result := FBitmap;
    end;

  end;
end;

procedure TsdJpegFormat.GetBitmapSize(AScale: TsdJpegScale; var AWidth, AHeight: integer);
var
  W, H, Divisor: integer;
begin
  if loTileMode in FLoadOptions then
  begin
    W := FInfo.TileWidth;
    H := FInfo.TileHeight;
  end else
  begin
    W := FInfo.Width;
    H := FInfo.Height;
  end;
  Divisor := GetDivisor(AScale);
  AWidth  := (W + Divisor - 1) div Divisor;
  AHeight := (H + Divisor - 1) div Divisor;
end;

procedure TsdJpegFormat.GetColorTransformFromBitmap(var AClass: TsdColorTransformClass);
var
  Stored, Input: TsdJpegColorSpace;
begin
  // At this point we can use DetectStoredColorSpace to find the color space of
  // the file, since all parameters and markes have been set. We can also trust
  // the FBitmapColorspace to match with the bitmap.

  DoDebugOut('Color conversion bitmap->samples:');
  if FStoredColors = jcAutoDetect then
  begin
    Stored := DetectStoredColorSpace;
    DoDebugOut(Format(' Stored colorsp: %s (detected)', [cColorSpaceNames[Stored]]));
  end else
  begin
    Stored := FStoredColors;
    DoDebugOut(Format(' Stored colorsp: %s (selected)', [cColorSpaceNames[Stored]]));
  end;
  Input := FBitmapColors;
  if Input = jcAutoDetect then
  begin
    case FBitmap.PixelFormat of
    pf8bit:  Input := jcGray;
    pf24bit: Input := jcRGB;
    pf32bit: Input := jcRGBA;
    end;
  end;

  // Defaults
  AClass := nil;
  case FInfo.FrameCount of
  1: if FBitmap.PixelFormat = pf8bit  then AClass := TsdNullTransform8bit;
  2: if FBitmap.PixelFormat = pf16bit then AClass := TsdNullTransform16bit;
  3: if FBitmap.PixelFormat = pf24bit then AClass := TsdNullTransform24bit;
  4: if FBitmap.PixelFormat = pf32bit then AClass := TsdNullTransform32bit;
  end;

  // Specific transforms
  case Stored of
  jcGray:
    case Input of
    jcRGB: AClass := TsdTransformBGRToGray;
    jcRGBA: AClass := TsdTransformRGBAToGray;
    end;
  jcRGB:
    case Input of
    jcRGB: AClass := TsdTransformInvertTriplet24bit;
    end;
  jcYCbCr, jcPhotoYCC:
    case Input of
    jcRGB: AClass := TsdTransformBGRToYCbCr;
    jcRGBA: AClass := TsdTransformBGRAToYCbCr;
    end;
  jcYCbCrA:
    case Input of
    jcRGBA: AClass := TsdTransformBGRAToYCbCrA;
    end;
  jcCMYK:
    case Input of
    jcRGB: AClass := TsdTransformRGBToCMYK;
    end;
  jcYCCK, jcPhotoYCCA:
    case Input of
    jcRGB: AClass := TsdTransformRGBToYCCK;
    jcCMYK: AClass := TsdTransformCMYKToYCCK;
    end;
  end;
end;

procedure TsdJpegFormat.GetColorTransformToBitmap(
  var AClass: TsdColorTransformClass; var AFormat: TPixelFormat);
var
  Warning: boolean;
  Stored, Output: TsdJpegColorSpace;
  procedure ClassAndFormat(C: TsdColorTransformClass; F: TPixelFormat);
  begin
    AClass := C;
    AFormat := F;
  end;
begin
  // default class and pixelformat
  case FInfo.FrameCount of
  1: ClassAndFormat(TsdNullTransform8bit, pf8bit);
  2: ClassAndFormat(TsdNullTransform16bit, pf16bit);
  3: ClassAndFormat(TsdNullTransform24bit, pf24bit);
  4: ClassAndFormat(TsdNullTransform32bit, pf32bit);
  else
    ClassAndFormat(nil, pf24bit);
  end;

  // Determine stored and bitmap colorspace
  DoDebugOut('Color conversion samples->bitmap:');
  if FStoredColors = jcAutoDetect then
  begin
    Stored := DetectStoredColorSpace;
    DoDebugOut(Format(' Stored colorsp: %s (detected)', [cColorSpaceNames[Stored]]));
  end else
  begin
    Stored := FStoredColors;
    DoDebugOut(Format(' Stored colorsp: %s (selected)', [cColorSpaceNames[Stored]]));
  end;

  if FBitmapColors = jcAutoDetect then
  begin
    Output := Stored;
    DoDebugOut(Format(' Bitmap colorsp: %s (no change)', [cColorSpaceNames[Output]]));
  end else
  begin
    Output := FBitmapColors;
    DoDebugOut(Format(' Bitmap colorsp: %s (selected)', [cColorSpaceNames[Output]]));
  end;

  // External color management
  if assigned(FOnExternalCM) then
  begin
    // We leave the handling of ICC transform to the external application
    DoDebugOut(' Color management handled by external application');
    exit;
  end;

  // Determine what conversion and pixelformat to use
  Warning := False;
  case Stored of
  jcGray:
    case Output of
    jcRGB: ClassAndFormat(TsdTransformGrayToBGR, pf24bit);
    jcGray:;
    else
      Warning := True;
    end;
  jcGrayA:
    case Output of
    jcRGB:  ClassAndFormat(TsdTransformGrayAToBGR, pf24bit);
    jcRGBA: ClassAndFormat(TsdTransformGrayAToBGRA, pf32bit);
    jcGrayA:;
    else
      Warning := True;
    end;
  jcRGB:
    case Output of
    jcRGBA: ClassAndFormat(TsdTransformRGBToBGRA, pf32bit);
    jcRGB: ClassAndFormat(TsdTransformInvertTriplet24bit, pf24bit);
    else
      Warning := True;
    end;
  jcRGBA:
    case Output of
    jcRGb: ClassAndFormat(TsdTransformRGBAToBGR, pf24bit);
    jcRGBA:;
    else
      Warning := True;
    end;
  jcYCbCr, jcPhotoYCc:
    case Output of
    jcGray: ClassAndFormat(TsdTransformYCbCrToGray, pf8bit);
    jcRGB:  ClassAndFormat(TsdTransformYCbCrToBGR, pf24bit);
    jcRGBA: ClassAndFormat(TsdTransformYCbCrToBGRA, pf32bit);
    else
      Warning := True;
    end;
  jcYCbCrA, jcPhotoYCcA:
    case Output of
    jcRGB: ClassAndFormat(TsdTransformYCbCrAToBGR, pf24bit);
    jcRGBA: ClassAndFormat(TsdTransformYCbCrAToBGRA, pf32bit);
    else
      Warning := True;
    end;
  jcYCbCrK:
    case Output of
    jcRGB: ClassAndFormat(TsdTransformYCbCrKToBGR, pf24bit);
    else
      Warning := True;
    end;
  jcCMYK:
    case output of
    jcRGB: ClassAndFormat(TsdTransformCMYKToBGR_Adobe, pf24bit);
    else
      Warning := True;
    end;
  jcYCCK:
    case output of
    jcRGB: ClassAndFormat(TsdTransformYCCKToBGR, pf24bit);
    else
      Warning := True;
    end;
  jcITUCieLAB:
    case output of
    jcRGB: ClassAndFormat(TsdTransformITUCIELabToBGR, pf24bit);
    else
      Warning := True;
    end;
  else
    Warning := True;
  end;
  if (Output <> Stored) and Warning then
    DoDebugOut(' Warning: no color transform could be found (stored colors are output)');
end;

function TsdJpegFormat.GetComment: string;
var
  M: TsdCOMMarker;
begin
  M := TsdCOMMarker(FMarkers.ByTag(mkCOM));
  if not assigned(M) then
    Result := ''
  else
    Result := M.Comment;
end;

class function TsdJpegFormat.GetDivisor(AScale: TsdJpegScale): integer;
begin
  case AScale of
  jsFull: Result := 1;
  jsDiv2: Result := 2;
  jsDiv4: Result := 4;
  jsDiv8: Result := 8;
  else
    Result := 1;
  end;
end;

function TsdJpegFormat.GetExifInfo: TsdEXIFMarker;
begin
  Result := TsdEXIFMarker(FMarkers.ByTag(mkApp1));
end;

function TsdJpegFormat.GetHeight: integer;
var
  D: integer;
begin
  D := GetDivisor(FLoadScale);
  Result := (GetImageHeight + D - 1) div D;
end;

function TsdJpegFormat.GetICCProfile: TsdJpegICCProfile;
// return the ICC profile from the ICCProfile markers
var
  M: TsdICCProfileMarker;
begin
  if assigned(FICCProfile) then
  begin
    Result := FICCProfile;
    exit;
  end;
  // Do we have an ICC profile?
  Result := nil;
  M := TsdICCProfileMarker(FMarkers.ByTag(mkApp2));
  if not assigned(M) or not M.IsValid then
    exit;

  FICCProfile := TsdJpegICCProfile.Create;
  FICCProfile.ReadFromMarkerList(FMarkers);
  Result := FICCProfile;
end;

function TsdJpegFormat.GetImageHeight: integer;
begin
  if assigned(FInfo) then
    Result := FInfo.Height
  else
    Result := 0;
end;

function TsdJpegFormat.GetImageWidth: integer;
begin
  if assigned(FInfo) then
    Result := FInfo.Width
  else
    Result := 0;
end;

function TsdJpegFormat.GetIptcInfo: TsdIPTCMarker;
begin
  Result := TsdIPTCMarker(FMarkers.ByTag(mkApp13));
end;

function TsdJpegFormat.GetJfifInfo: TsdJFIFMarker;
begin
  Result := TsdJFIFMarker(FMarkers.ByTag(mkApp0));
end;

function TsdJpegFormat.GetLossless: TsdLosslessOperation;
begin
  if not assigned(FLossless) then
    FLossless := TsdLosslessOperation.Create(Self);
  Result := FLossless;
end;

class function TsdJpegFormat.GetVersion: string;
begin
  Result := cJpegVersion;
end;

function TsdJpegFormat.GetWidth: integer;
var
  D: integer;
begin
  D := GetDivisor(FLoadScale);
  Result := (GetImageWidth + D - 1) div D;
end;

function TsdJpegFormat.HasCoefficients: boolean;
begin
  Result := False;
  if assigned(FCoder) then Result := FCoder.HasCoefs;
end;

function TsdJpegFormat.HasICCProfile: boolean;
// Determine if we have a valid ICC profile
var
  M: TsdICCProfileMarker;
begin
  // ICC profile already read?
  if assigned(FICCProfile) then
  begin
    Result := True;
    exit;
  end;
  // Do we have an ICC profile?
  M := TsdICCProfileMarker(FMarkers.ByTag(mkApp2));
  Result := assigned(M) and M.IsValid;
end;

function TsdJpegFormat.HasSamples: boolean;
begin
  Result := False;
  if assigned(FCoder) then Result := FCoder.HasSamples;
end;

procedure TsdJpegFormat.InitializeDecode;
begin
  // Create correct codec
  case FInfo.EncodingMethod of
  emBaselineDCT:
    begin
      if not assigned(FCoder) or (FCoder.ClassType <> TsdJpegBaselineCoder) then
      begin
        FreeAndNil(FCoder);
        FCoder := TsdJpegBaselineCoder.Create(FInfo);
      end;
    end;
  emExtendedDCT:
    begin
      if not assigned(FCoder) or (FCoder.ClassType <> TsdJpegExtendedCoder) then
      begin
        FreeAndNil(FCoder);
        FCoder := TsdJpegExtendedCoder.Create(FInfo);
      end;
    end;
  emProgressiveDCT:
    begin
      if not assigned(FCoder) or (FCoder.ClassType <> TsdJpegProgressiveCoder) then
      begin
        FreeAndNil(FCoder);
        FCoder := TsdJpegProgressiveCoder.Create(FInfo);
      end;
    end;
  else
    FreeAndNil(FCoder);
    exit;
  end;
  FCoder.Clear;
  FCoder.OnDebugOut := FOnDebugOut;
  FCoder.Method := FDCTCodingMethod;
  FCoder.TileMode := loTileMode in FLoadOptions;
  FCoder.Initialize(FLoadScale);
end;

procedure TsdJpegFormat.InjectMetadata(AList: TsdJpegMarkerList);
begin
  FMarkers.RemoveMarkers([mkAPP0..mkAPP15, mkCOM]);
  while AList.Count > 0 do
    FMarkers.Insert(1, AList.Extract(AList[AList.Count - 1]));
end;

procedure TsdJpegFormat.LoadFromFile(const AFileName: string);
var
  F: TFileStream;
  M: TMemoryStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  // We use a temp mem copy for speed
  M := TMemoryStream.Create;
  try
    M.Size := F.Size;
    M.CopyFrom(F, M.Size);
    M.Position := 0;
    LoadFromStream(M);
  finally
    F.Free;
    M.Free;
  end;
end;

procedure TsdJpegFormat.LoadFromStream(S: TStream);
begin
  Clear;
  try
    repeat
      Tag := ReadMarkers(S);
      case Tag of
      mkSOF0..mkSOF2:
        begin
          // Method is defined.. we can initialise the decoder
          if not (loOnlyMetadata in FLoadOptions) then
            InitializeDecode;
        end;
      mkSOS:
        if loOnlyMetadata in FLoadOptions then
        begin
          if FInfo.WaitForDNL then
          begin
            DoDebugOut('loOnlyMetadata option: Skipped encoded data.');
            EntropyDecodeSkip(S);
          end else
          begin
            DoDebugOut('loOnlyMetadata option: Skipped rest of file.');
            exit;
          end;
        end else
          if assigned(FCoder) then
            FCoder.Decode(S);
      mkEOI:
        begin
          if not (loOnlyMetadata in FLoadOptions) then
            FCoder.Finalize;
        end;
      mkRST0..mkRST7:
        begin
          DoDebugOut('WARNING: wrong place for marker.');
          exit;
        end;
      end;
    until Tag = mkEOI;
  except
    on E: Exception do
    begin
      DoDebugOut(Format('Exception during decode: %s', [E.Message]));
      raise;
    end;
  end;
end;

procedure TsdJpegFormat.LoadTileBlock(S: TStream; var ALeft, ATop, ARight, ABottom: integer);
var
  D, McuW, McuH: integer;
  XStart, YStart, XCount, YCount: integer;
begin
  DoDebugOut(Format('Load tile block %d,%d, %d,%d',
    [ALeft, ATop, ARight, ABottom]));

  // Some checks
  if not assigned(FCoder) then
    raise EInvalidGraphic.Create('Call LoadTileBlock only after LoadFromStream');
  if FCoder.ClassType <> TsdJpegBaselineCoder then
    raise EInvalidGraphic.Create('Tiled loading only possible with baseline jpeg');

  // Determine MCU block area
  D := GetDivisor(FLoadScale);
  McuW := FInfo.MCUWidth div D;
  McuH := FInfo.MCUHeight div D;

  ALeft := Max(0, ALeft div McuW);
  ARight := Min(FInfo.HorzMCUCount, (ARight + McuW - 1) div McuW);
  ATop := Max(0, ATop div McuH);
  ABottom := Min(FInfo.VertMcuCount, (ABottom + McuH - 1) div McuH);

  XCount := ARight - ALeft;
  YCount := ABottom - ATop;
  XStart := ALeft;
  YStart := ATop;
  ALeft := ALeft * McuW;
  ATop := ATop * McuH;
  ARight := ARight * McuW;
  ABottom := ABottom * McuH;

  // Anything to load?
  if (XCount <= 0) or (YCount <= 0) then
    exit;

  FInfo.TileWidth := McuW * XCount * D;
  FInfo.TileHeight := McuH * YCount * D;

  FCoder.DecodeBlock(S, XStart, YStart, XCount, YCount);
end;

function TsdJpegFormat.ReadMarkers(S: TStream): byte;
var
  B, Tag, BytesRead: byte;
  Marker: TsdJpegMarker;
  Size: word;
  StreamPos: integer;
begin
  // Read markers from the stream, until a non $FF is encountered
  repeat
    DoDebugOut(Format('Current Pos: %.6d', [S.Position]));
    BytesRead := S.Read(B, 1);
    if BytesRead = 0 then
      raise EInvalidGraphic.Create(sMarkerExpected);
    // Do we have a marker?
    if B = $FF then
    begin
      // Which marker?
      S.Read(Tag, 1);
      while Tag = $FF do
      begin
        Tag := $00;
        DoDebugOut(Format('Error: duplicate $FF encountered at %.6d', [S.Position - 1]));
        S.Read(Tag, 1);
      end;
      case Tag of
      mkAPP0:
        Marker := TsdJFIFMarker.Create(FInfo, Tag);
      mkAPP1:
        Marker := TsdEXIFMarker.Create(FInfo, Tag);
      mkAPP2:
        Marker := TsdICCProfileMarker.Create(FInfo, Tag);
      mkAPP13:
        Marker := TsdIPTCMarker.Create(FInfo, Tag);
      mkAPP14:
        Marker := TsdAdobeApp14Marker.Create(FInfo, Tag);
      mkAPP3..mkAPP12, mkAPP15:
        Marker := TsdAPPnMarker.Create(FInfo, Tag);
      mkDHT:
        Marker := TsdDHTMarker.Create(FInfo, Tag);
      mkDQT:
        Marker := TsdDQTMarker.Create(FInfo, Tag);
      mkDRI:
        Marker := TsdDRIMarker.Create(FInfo, Tag);
      mkSOF0..mkSOF3, mkSOF5..mkSOF7, mkSOF9..mkSOF11, mkSOF13..mkSOF15:
        Marker := TsdSOFnMarker.Create(FInfo, Tag);
      mkSOS:
        Marker := TsdSOSMarker.Create(FInfo, Tag);
      mkSOI:
        Marker := TsdSOIMarker.Create(FInfo, Tag);
      mkEOI:
        Marker := TsdEOIMarker.Create(FInfo, Tag);
      mkRST0..mkRST7:
        Marker := TsdRSTMarker.Create(FInfo, Tag);
      mkCOM:
        Marker := TsdCOMMarker.Create(FInfo, Tag);
      mkDNL:
        Marker := TsdDNLMarker.Create(FInfo, Tag);
      else
        // General marker
        Marker := TsdJpegMarker.Create(FInfo, Tag);
      end;
      // Add marker to our list
      FMarkers.Add(Marker);
      if Tag in [mkAPP0..mkAPP15, mkDHT, mkDQT, mkDRI, mkSOF0..mkSOF15, mkSOS, mkCOM, mkDNL] then
      begin
        // Read length of marker
        Size := TsdJpegMarker.GetWord(S) - 2;
      end else
        Size := 0;
      // Instruct the marker to load
      StreamPos := S.Position;
      Marker.LoadFromStream(S, Size);
      // From here we transfer control back, the SOS marker indicates start of
      // entropy coding, EOI indicates end of image. SOF0 and SOF2 indicate
      // baseline and progressive starts
      if Tag in [mkSOF0..mkSOF2, mkSOS, mkRST0..mkRST7, mkEOI] then break;
      // Find correct stream position
      S.Position := StreamPos + Size;
    end else
    begin
      // B <> $FF is an error, we try to be flexible
      DoDebugOut(Format('Error: marker expected at %.6d', [S.Position - 1]));
      { in case of following the spec strictly.. put this in the code
        raise EInvalidGraphic.Create(sMarkerExpected); }
      repeat
        BytesRead := S.Read(B, 1);
      until (BytesRead = 0) or (B = $FF);
      if BytesRead = 0 then
        raise EInvalidGraphic.Create(sMarkerExpected);
      S.Seek(-1, soFromCurrent);
      DoDebugOut(Format('Resuming at %.6d', [S.Position]));
    end;
  until false;
  Result := Tag;
end;

procedure TsdJpegFormat.SaveToFile(const AFileName: string);
var
  F: TFileStream;
  M: TsdFastMemStream;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  // We use a temp mem copy for speed
  M := TsdFastMemStream.Create;
  try
    SaveToStream(M);
    M.Position := 0;
    F.CopyFrom(M, M.Size);
  finally
    F.Free;
    M.Free;
  end;
end;

procedure TsdJpegFormat.SaveToStream(S: TStream);
const
  cFF: byte = $FF;
var
  i: integer;
  M: TsdJpegMarker;
  PStart, PCurrent: integer;
  Size: word;
  SeenDHT: boolean;
  DHT: TsdDHTMarker;
begin
  // Do we need to convert bitmap info to coefficients?
  if not HasCoefficients then
  begin
    if not assigned(FBitmap) or (FBitmap.Width * FBitmap.Height = 0) then
      raise EInvalidGraphic.Create(sBitmapIsEmptyCannotSave);
    // Encode the bitmap so the coder has coefficients.
    EncodeBitmap;
  end;

  if FCoder.Scale <> jsFull then
    raise EInvalidGraphic.Create(sOperationOnlyFor8x8);

  // We can now repeatedly save up to the last SOS, then ask the codec to encode
  // the scan
  SeenDHT := False;
  i := 0;
  while i < FMarkers.Count do
  begin
    M := Markers[i];
    if M is TsdDHTMarker then
      SeenDHT := True;
    if (M is TsdSOSMarker) and not SeenDHT then
    begin
      // We are at Start of Scan but have not saved a Huffman table, so we must
      // create one and do that now. First we apply scan data by saving marker
      // to nil stream.
      M.SaveToStream(nil);

      // Now create the optimized huffman tables for this scan, by doing a dry
      // run, indicated by nil
      FCoder.Encode(nil);

      // Ask the coder to create the Define Huffman Table (DHT) marker for us, as a
      // result of the dry-run information
      DHT := FCoder.CreateDHTMarker;
      if assigned(DHT) then
      begin
        // If a marker was created, then insert it and continue, so it will be saved
        FMarkers.Insert(i, DHT);
        SeenDHT := True;
        Continue;
      end;
    end;
    S.Write(cFF, 1);
    S.Write(M.Tag, 1);
    PStart := S.Position;
    DoDebugOut(Format('Saving marker %s', [IntToHex(M.Tag, 2)]));
    if not (M.Tag in [mkSOI, mkEOI, mkRST0..mkRST7]) then
    begin
      // Save two spaces for size
      S.Write(Size, 2);
      // Saving a marker will also make the marker update itself in the Info
      // object, so when calling FCoder.Encode later, it will have the current data
      M.SaveToStream(S);
      // Now go back and write the size
      PCurrent := S.Position;
      Size := Swap(PCurrent - PStart);
      S.Position := PStart;
      S.Write(Size, 2);
      S.Position := PCurrent;
    end;
    // Encode and save data
    if M is TsdSOSMarker then
    begin
      FCoder.Encode(S);
      SeenDHT := False;
    end;
    // Next marker
    inc(i);
  end;
end;

procedure TsdJpegFormat.SaveToStreamStripByStrip(S: TStream; AWidth, AHeight: integer);
const
  cFF: byte = $FF;
var
  TransformClass: TsdColorTransformClass;
  Transform: TsdColorTransform;
  BmpIter: TsdMapIterator;
  Stored: TsdJpegColorSpace;
  i, y: integer;
  M: TsdJpegMarker;
  PStart, PCurrent: integer;
  Size: word;
  CB: TsdJpegBaselineCoder;
begin
  if not assigned(FOnProvideStrip) then
    raise EInvalidGraphic.Create(sOnProvideStripMustBeAssigned);

  // Create bitmap with correct pixelformat
  FreeAndNil(FBitmap);
  FBitmap := TBitmap.Create;
  case FBitmapColors of
  jcGray:
    FBitmap.PixelFormat := pf8bit;
  jcGrayA:
    FBitmap.PixelFormat := pf16bit;
  jcRGB, jcYCbCr, jcPhotoYCC:
    FBitmap.PixelFormat := pf24bit;
  jcRGBA, jcYCbCrA, jcCMYK, jcYCCK, jcPhotoYCCA:
    FBitmap.PixelFormat := pf32bit;
  else
    FBitmap.PixelFormat := pf24bit;
  end;

  // We create the baseline coder
  FreeAndNil(FCoder);
  FCoder := TsdJpegBaselineCoder.Create(FInfo);
  CB := TsdJpegBaselineCoder(FCoder);

  // Verify incoming bitmap format versus bitmap colorspace
  Stored := VerifyBitmapColorSpaceForSave;

  // We create minimal default markers to warrant color space detection
  // later on
  AddMinimalMarkersForColorSpaceDetection(Stored);

  // Ask save options to add DQT, SOFn and SOS marker. We use pre-defined
  // Huffman tables because we only do one pass over the image
  FSaveOptions.OptimizeHuffmanTables := False;
  FSaveOptions.AddMarkers(Stored, AWidth, AHeight);

  // We also must add an EOI marker
  FMarkers.Add(TsdEOIMarker.Create(FInfo, mkEOI));

  // Color transform
  GetColorTransformFromBitmap(TransformClass);
  if not assigned(TransformClass) then
    raise EInvalidGraphic.Create(sInvalidFormatForSelectedCS);
  Transform := TransformClass.Create;

  // Initialize coder (this sets map sizes etc)
  FCoder.TileMode := True; // avoid allocating full buffer
  FCoder.Initialize(jsFull); // will calculate MCU height

  // Bitmap strip size
  FBitmap.Width := AWidth;
  FBitmap.Height := FInfo.McuHeight;

  BmpIter := TsdMapIterator.Create;
  try
    // Get iterator
    GetBitmapIterator(FBitmap, BmpIter);

    // Now we can save the image, and interactively ask application for strips
    // We can now repeatedly save up to the last SOS, then ask the codec to encode
    // the scan
    i := 0;
    while i < FMarkers.Count do
    begin

      M := Markers[i];
      S.Write(cFF, 1);
      S.Write(M.Tag, 1);
      PStart := S.Position;
      DoDebugOut(Format('Saving marker %s', [IntToHex(M.Tag, 2)]));
      if not (M.Tag in [mkSOI, mkEOI, mkRST0..mkRST7]) then
      begin
        // Save two spaces for size
        S.Write(Size, 2);
        // Saving a marker will also make the marker update itself in the Info
        // object, so when calling FCoder.Encode later, it will have the current data
        M.SaveToStream(S);
        // Now go back and write the size
        PCurrent := S.Position;
        Size := Swap(PCurrent - PStart);
        S.Position := PStart;
        S.Write(Size, 2);
        S.Position := PCurrent;
      end;

      if M is TsdSOSMarker then
      begin

        // Start encoder in strip-by-strip mode. This will calculate MCU height
        CB.EncodeStripStart(S);

        // Encode strips one by one
        for y := 0 to FInfo.VertMcuCount - 1 do
        begin
          // Call the OnProvideStrip event
          FOnProvideStrip(Self, 0, y * FInfo.McuHeight, FBitmap);
          // Get samples from bitmap data
          FCoder.SamplesFromImage(BmpIter, Transform);
          // Now convert samples to coefficients. This also does the quantization
          FCoder.ForwardDCT;
          // And encode them to the stream
          CB.EncodeStrip(S);
        end;

        // finalise encoder
        CB.EncodeStripClose;

      end;

      // Next marker
      inc(i);
    end;

  finally
    BmpIter.Free;
    Transform.Free;
  end;
end;

procedure TsdJpegFormat.SetBitmap(const Value: TBitmap);
begin
  Clear;
  if assigned(Value) then
  begin
    if not assigned(FBitmap) then
      FBitmap := TBitmap.Create;
    FBitmap.Assign(Value);

  end else
    FreeAndNil(FBitmap);
end;

procedure TsdJpegFormat.SetComment(const Value: string);
var
  M: TsdCOMMarker;
begin
  M := TsdCOMMarker(FMarkers.ByTag(mkCOM));
  if not assigned(M) then
  begin
    // We do not yet have a marker
    if not FMarkers.HasMarker([mkSOI]) then
      raise EInvalidGraphic.Create(sCommentCannotBeSet);
    // Create the marker and insert after SOI or JFIF marker (whichever comes last)
    M := TsdCOMMarker.Create(FInfo, mkCOM);
    FMarkers.InsertAfter([mkSOI, mkAPP0], M);
  end;
  M.Comment := Value;
end;

procedure TsdJpegFormat.SetICCProfile(const Value: TsdJpegICCProfile);
begin
  FreeAndNil(FICCProfile);
  FMarkers.RemoveMarkers([mkApp2]);
  if assigned(Value) then
    Value.WriteToMarkerList(FMarkers);
end;

function TsdJpegFormat.VerifyBitmapColorSpaceForSave: TsdJpegColorSpace;
var
  Error: boolean;
begin
  Error := False;
  Result := FStoredColors;

  case FBitmapColors of
  jcAutoDetect:
    begin
      // Ensure we have some valid pixelformat
      if not (FBitmap.PixelFormat in [pf8bit, pf24bit, pf32bit]) then
        FBitmap.PixelFormat := pf24bit;
      case FBitmap.PixelFormat of
      pf8bit:  Result := jcGray;
      pf24bit: Result := jcYCbCr;
      pf32bit: Result := jcYCCK;
      end;
    end;
  jcGray:
    Error := FBitmap.PixelFormat <> pf8bit;
  jcGrayA:
    Error := FBitmap.PixelFormat <> pf16bit;
  jcRGB, jcYCbCr, jcPhotoYCC:
    Error := FBitmap.PixelFormat <> pf24bit;
  jcRGBA, jcYCbCrA, jcCMYK, jcYCCK, jcPhotoYCCA:
    Error := FBitmap.PixelFormat <> pf32bit;
  end;
  if Error then
    raise EInvalidGraphic.Create(sInvalidFormatForSelectedCS);

  // Select correct color space to store
  if Result = jcAutoDetect then
  begin
    case FBitmapColors of
    jcGray:
      Result := jcGray;
    jcGrayA:
      Result := jcGrayA;
    jcRGB, jcYCbCr:
      Result := jcYCbCr;
    jcRGBA, jcYCbCrA:
      Result := jcYCbCrA;
    else
      Result := FBitmapColors;
    end;
  end;
end;

{ TsdJpegCoder }

procedure TsdJpegCoder.BuildMarkerList(AList: TsdJpegMarkerList);
begin
// default does nothing
end;

procedure TsdJpegCoder.Clear;
begin
  FHasCoefs := False;
  FHasSamples := False;
  FScale := jsFull;
end;

constructor TsdJpegCoder.Create(AInfo: TsdJpegCodingInfo);
begin
  inherited Create;
  FInfo := AInfo;
end;

function TsdJpegCoder.CreateDHTMarker: TsdDHTMarker;
begin
  Result := nil;
end;

procedure TsdJpegCoder.Decode(S: TStream);
begin
// default does nothing
end;

procedure TsdJpegCoder.DecodeBlock(S: TStream; XStart, YStart, XCount, YCount: integer);
begin
// default does nothing
end;

procedure TsdJpegCoder.DoDebugOut(const AMessage: string);
begin
  if assigned(FOnDebugOut) then FOnDebugOut(Self, AMessage);
end;

procedure TsdJpegCoder.Encode(S: TStream);
begin
// default does nothing
end;

procedure TsdJpegCoder.Finalize;
begin
// default does nothing
end;

procedure TsdJpegCoder.ForwardDCT;
begin
// default does nothing
end;

procedure TsdJpegCoder.Initialize(AScale: TsdJpegScale);
begin
  FScale := AScale;
end;

procedure TsdJpegCoder.InverseDCT;
begin
// default does nothing
end;

{ TsdJpegSaveOptions }

procedure TsdJpegSaveOptions.AddMarkers(AStored: TsdJpegColorSpace;
  AWidth, AHeight: integer);
var
  i: integer;
  Info: TsdJpegCodingInfo;
  Frame: TsdFrameComponent;
  SOF: TsdSOFnMarker;
  SOS: TsdSOSMarker;
begin
  // Set the correct FInfo fields
  Info := FOwner.FInfo;
  Info.Width := AWidth;
  Info.Height := AHeight;
  Info.SamplePrecision := 8;
  case AStored of
  jcGray:
    Info.FrameCount := 1;
  jcGrayA:
    Info.FrameCount := 2;
  jcRGB, jcYCbCr, jcPhotoYCC:
    Info.FrameCount := 3;
  jcRGBA, jcYCbCrA, jcCMYK, jcYCCK, jcPhotoYCCA:
    Info.FrameCount := 4;
  else
    raise EInvalidGraphic.Create(sUnsupportedColorSpace);
  end;

  // Subsampling used?
  case AStored of
  jcYCbCr, jcYCbCrA, jcYCCK, jcPhotoYCC, jcPhotoYCCA:
    FUseSubSampling := True
  else
    FUseSubSampling := False;
  end;

  // Set up frame sampling
  for i := 0 to Info.FrameCount - 1 do
  begin
    Frame := Info.Frames[i];
    if (i in [1, 2]) then
    begin
      // Subsampled frame
      Frame.HorzSampling := 1;
      Frame.VertSampling := 1;
      if FUseSubSampling then
        Frame.QTable := 1
      else
        Frame.QTable := 0;
    end else
    begin
      // Full frame
      if FUseSubSampling then
      begin
        Frame.HorzSampling := 2;
        Frame.VertSampling := 2;
      end else
      begin
        Frame.HorzSampling := 1;
        Frame.VertSampling := 1;
      end;
      Frame.QTable := 0;
    end;
    Frame.ComponentID := i + 1;
  end;
  if FUseSubSampling then
  begin
    Info.HorzSamplingMax := 2;
    Info.VertSamplingMax := 2;
  end else
  begin
    Info.HorzSamplingMax := 1;
    Info.VertSamplingMax := 1;
  end;

  // Setup and add quant tables
  SetupQuantTables;

  // Create and add SOFn marker
  SOF := TsdSOFnMarker.Create(Info, mkSOF0);
  FOwner.Markers.Add(SOF);

  // Create and add default Huffman tables if required
  if not OptimizeHuffmanTables then
    SetupDefaultHuffmanTables;

  // Create and add SOS marker
  SOS := TsdSOSMarker.Create(Info, mkSOS);
  FOwner.Markers.Add(SOS);

  SetLength(SOS.FMarkerInfo, Info.FrameCount);
  SOS.FScanCount := Info.FrameCount;
  for i := 0 to Info.FrameCount - 1 do
  begin
    SOS.FMarkerInfo[i].ComponentID := i + 1;
    SOS.FMarkerInfo[i].DCTable := Info.Frames[i].QTable;
    SOS.FMarkerInfo[i].ACTable := Info.Frames[i].QTable;
  end;

end;

constructor TsdJpegSaveOptions.Create(AOwner: TsdJpegFormat);
begin
  inherited Create;
  FOwner := AOwner;
  FQuality := 80;
  FOptimizeHuffmanTables := True;
  FCodingMethod := emBaselineDCT;
  FUseSubSampling := True;
end;

procedure TsdJpegSaveOptions.SetTableMultiplication(ATable: TsdQuantizationTable;
  MultiplyPercent: integer; const ADefaultTable: TsdIntArray64);
var
  i, Q: integer;
begin
  for i := 0 to 63 do
  begin
    Q := (ADefaultTable[cJpegInverseZigZag8x8[i]] * MultiplyPercent + 50) div 100;
    // ensure that quant factor is in valid range
    if Q <= 0 then Q := 1
    else if Q > 255 then Q := 255;
    // set table quant factor i
    ATable.Quant[i] := Q;
  end;
end;

procedure TsdJpegSaveOptions.SetupDefaultHuffmanTables;
var
  M: TsdDHTMarker;
  procedure FillTable(var Info: TsdDHTMarkerInfo; Bits, Values: Pbyte; Tc, Th: byte);
  var
    i, Count: integer;
  begin
    Count := 0;
    Info.Tc := Tc;
    Info.Th := Th;
    for i := 0 to 15 do
    begin
      Info.BitLengths[i] := Bits^;
      inc(Count, Bits^);
      inc(Bits);
    end;
    SetLength(Info.BitValues, Count);
    for i := 0 to Count - 1 do
    begin
      Info.BitValues[i] := Values^;
      inc(Values);
    end;
  end;
begin
  M := TsdDHTMarker.Create(FOwner.FInfo, mkDHT);
  if FUseSubsampling then
    SetLength(M.FMarkerInfo, 4)
  else
    SetLength(M.FMarkerInfo, 2);

  // Luminance tables (always used)
  FillTable(M.FMarkerInfo[0], @cHuffmanBitsDcLum[0], @cHuffmanValDcLum[0], 0, 0);
  FillTable(M.FMarkerInfo[1], @cHuffmanBitsAcLum[0], @cHuffmanValAcLum[0], 1, 0);

  if FUseSubsampling then
  begin
    // Chrominance tables (only when subsampling is used)
    FillTable(M.FMarkerInfo[2], @cHuffmanBitsDcChrom[0], @cHuffmanValDcChrom[0], 0, 1);
    FillTable(M.FMarkerInfo[3], @cHuffmanBitsAcChrom[0], @cHuffmanValAcChrom[0], 1, 1);
  end;
  FOwner.Markers.Add(M);
end;

procedure TsdJpegSaveOptions.SetupQuantTables;
var
  QMul: integer;
  T: TsdQuantizationTable;
  M: TsdDQTMarker;
begin
  if FQuality < 1 then FQuality := 1
  else if FQuality > 100 then FQuality := 100;

  // Calculation of quant multiplication factor
  if FQuality < 50 then
    QMul := 5000 div FQuality
  else
    QMul := 200 - FQuality * 2;

  // Create DQT marker
  M := TsdDQTMarker.Create(FOwner.FInfo, mkDQT);
  if FUseSubSampling then
    SetLength(M.FTableIndices, 2)
  else
    SetLength(M.FTableIndices, 1);

  // Quant table 0
  T := FOwner.FInfo.QuantizationTables[0];
  SetTableMultiplication(T, QMul, cStdLuminanceQuantTbl);
  M.FTableIndices[0] := 0;

  // Quant table 1
  if FUseSubSampling then
  begin
    T := FOwner.FInfo.QuantizationTables[1];
    SetTableMultiplication(T, QMul, cStdChrominanceQuantTbl);
    M.FTableIndices[1] := 1;
  end;

  // Add DQT marker
  FOwner.Markers.Add(M);
end;

initialization

  {$IFDEF TRIALJPG}
  ShowMessage(
    'This is a trial version of NativeJpg. See'#13 +
	'http//www.simdesign.nl/nativejpg.html for more info.');
  {$ENDIF}

end.
