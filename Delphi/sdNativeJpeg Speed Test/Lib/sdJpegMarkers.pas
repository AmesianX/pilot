//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegMarkers;

interface

uses
  Windows, Classes, Contnrs, SysUtils, Graphics, sdJpegTypes, sdJpegConsts;

type

  TsdJpegMarker = class(TPersistent)
  private
    FTag: byte;
    FInfo: TsdJpegCodingInfo;
    FOwner: TObject;
    procedure DoDebugOut(const AMessage: string);
  protected
    function GetByte(S: TStream): byte;
    procedure PutByte(S: TStream; B: byte);
    procedure PutWord(S: TStream; W: word);
  public
    constructor Create(AInfo: TsdJpegCodingInfo; ATag: byte); virtual;
    class function GetWord(S: TStream): word;
    procedure LoadFromStream(S: TStream; Size: integer); virtual;
    procedure SaveToStream(S: TStream); virtual;
    // Any of the mkXXXX constants defined in sdJpegConsts
    property Tag: byte read FTag;
    // Reference to owner TsdJpegFormat, set when adding to the list, and used
    // for DoDebugOut
    property Owner: TObject read FOwner write FOwner;
  end;

  TsdJpegMarkerList = class(TObjectList)
  private
    FOwner: TObject;// Reference to owner TsdJpegFormat
    function GetItems(Index: integer): TsdJpegMarker;
  public
    constructor Create(AOwner: TObject);
    function ByTag(ATag: byte): TsdJpegMarker;
    function HasMarker(ASet: TsdMarkerSet): boolean;
    procedure RemoveMarkers(ASet: TsdMarkerSet);
    procedure InsertAfter(ASet: TsdMarkerSet; AMarker: TsdJpegMarker);
    procedure Add(AItem: TObject);
    property Items[Index: integer]: TsdJpegMarker read GetItems; default;
  end;

  TsdAPPnMarker = class(TsdJpegMarker)
  private
    FData: TMemoryStream;
  protected
    function ApplicationString: string; virtual;
    procedure StoreData(S: TStream; Size: integer);
  public
    constructor Create(AInfo: TsdJpegCodingInfo; ATag: byte); override;
    destructor Destroy; override;
    procedure LoadFromStream(S: TStream; Size: integer); override;
    procedure SaveToStream(S: TStream); override;
  end;

  TsdDHTMarkerInfo = record
    BitLengths: array[0..15] of byte;
    BitValues: array of byte;
    Tc, Th: byte;
  end;
  PsdDHTMarkerInfo = ^TsdDHTMarkerInfo;

  TsdDHTMarker = class(TsdJpegMarker)
  private
  public
    FMarkerInfo: array of TsdDHTMarkerInfo;
    procedure LoadFromStream(S: TStream; Size: integer); override;
    procedure SaveToStream(S: TStream); override;
  end;

  TsdDQTMarker = class(TsdJpegMarker)
  private
  public
    FTableIndices: array of byte;
    procedure LoadFromStream(S: TStream; Size: integer); override;
    procedure SaveToStream(S: TStream); override;
  end;

  TsdDRIMarker = class(TsdJpegMarker)
  public
    procedure LoadFromStream(S: TStream; Size: integer); override;
    procedure SaveToStream(S: TStream); override;
  end;

  TsdSOFnMarker = class(TsdJpegMarker)
  public
    procedure LoadFromStream(S: TStream; Size: integer); override;
    procedure SaveToStream(S: TStream); override;
  end;

  TsdSOSMarkerInfo = record
    ComponentID: byte;
    DCTable: byte;
    ACTable: byte;
  end;

  TsdSOSMarker = class(TsdJpegMarker)
  private
    FSpectralStart,
    FSpectralEnd,
    FApproxHigh,
    FApproxLow: byte;
  protected
    procedure FindScanComponent(AScan: TsdScanComponent; AId: byte);
  public
    FScanCount: byte;
    FMarkerInfo: array of TsdSOSMarkerInfo;
    procedure LoadFromStream(S: TStream; Size: integer); override;
    procedure SaveToStream(S: TStream); override;
  end;

  TsdSOIMarker = class(TsdJpegMarker)
  public
    procedure LoadFromStream(S: TStream; Size: integer); override;
  end;

  TsdEOIMarker = class(TsdJpegMarker)
  public
    procedure LoadFromStream(S: TStream; Size: integer); override;
  end;

  TsdRSTMarker = class(TsdJpegMarker)
  public
    procedure LoadFromStream(S: TStream; Size: integer); override;
  end;

  TsdDNLMarker = class(TsdJpegMarker)
  public
    procedure LoadFromStream(S: TStream; Size: integer); override;
    procedure SaveToStream(S: TStream); override;
  end;

  TsdCOMMarker = class(TsdAppnMarker)
  private
    function GetComment: string;
    procedure SetComment(const Value: string);
  public
    procedure LoadFromStream(S: TStream; Size: integer); override;
    property Comment: string read GetComment write SetComment;
  end;

  TsdJFIFUnits = (
    juNoUnits,
    juXandYareDotsPerInch,
    juXandYareDotsPerCm
  );

  TsdJFIFMarker = class(TsdAPPnMarker)
  private
    FIsValid: boolean;
    FVersion: word;
    FUnits: TsdJFIFUnits;
    FXDensity, FYDensity: word;
    FXThumbnail, FYThumbnail: word;
    function GetUnits: TsdJFIFUnits;
    function GetVersion: word;
    function GetXDensity: word;
    function GetXThumbnail: byte;
    function GetYDensity: word;
    function GetYThumbnail: byte;
  protected
    function GetIsValid: boolean;
    procedure SaveData;
    function ApplicationString: string; override;
  public
    constructor Create(AInfo: TsdJpegCodingInfo; ATag: byte); override;
    property IsValid: boolean read GetIsValid;
    property Version: word read GetVersion;
    property Units: TsdJFIFUnits read GetUnits;
    property XDensity: word read GetXDensity;
    property YDensity: word read GetYDensity;
    property XThumbnail: byte read GetXThumbnail;
    property YThumbnail: byte read GetYThumbnail;
  end;

  TsdICCProfileMarker = class(TsdAppnMarker)
  private
    FIsValid: boolean;
    FCurrentMarker: byte;
    FMarkerCount: byte;
    function GetCurrentMarker: byte;
    function GetMarkerCount: byte;
    function GetData: pointer;
    function GetDataLength: integer;
    procedure SetDataLength(const Value: integer);
    procedure SetCurrentMarker(const Value: byte);
    procedure SetMarkerCount(const Value: byte);
  protected
    function GetIsValid: boolean;
    function ApplicationString: string; override;
  public
    property IsValid: boolean read GetIsValid;
    property CurrentMarker: byte read GetCurrentMarker write SetCurrentMarker;
    property MarkerCount: byte read GetMarkerCount write SetMarkerCount;
    property Data: pointer read GetData;
    property DataLength: integer read GetDataLength write SetDataLength;
  end;

  TsdEXIFMarker = class(TsdAPPnMarker)
  protected
    function ApplicationString: string; override;
  public
    function IsG3FAX: boolean;
  end;

  TsdIPTCMarker = class(TsdAPPnMarker)
  protected
    function ApplicationString: string; override;
  end;

  TsdAdobeApp14Marker = class(TsdAppnMarker)
  private
    FIsValid: boolean;
    FVersion: word;
    FFlags0: word;
    FFlags1: word;
    FTransform: byte;
    function GetTransform: byte;
    procedure SetTransform(const Value: byte);
  protected
    function GetIsValid: boolean;
    procedure SaveData;
    function ApplicationString: string; override;
  public
    constructor Create(AInfo: TsdJpegCodingInfo; ATag: byte); override;
    property IsValid: boolean read GetIsValid;
    property Transform: byte read GetTransform write SetTransform;
  end;

  TsdJpegICCProfile = class(TPersistent)
  private
    FData: array of byte;
    function GetData: pointer;
    function GetDataLength: integer;
  public
    procedure LoadFromStream(S: TStream);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(S: TStream);
    procedure ReadFromMarkerList(AList: TsdJpegMarkerList);
    procedure WriteToMarkerList(AList: TsdJpegMarkerList);
    property Data: pointer read GetData;
    property DataLength: integer read GetDataLength;
  end;

implementation

uses
  sdJpegFormat;

type
  TJpegAccess = class(TsdJpegFormat);

function Min(i1, i2: integer): integer;
begin
  if i1 < i2 then Result := i1
  else Result := i2;
end;

{ TsdJpegMarker }

constructor TsdJpegMarker.Create(AInfo: TsdJpegCodingInfo; ATag: byte);
begin
  inherited Create;
  FInfo := AInfo;
  FTag := ATag;
end;

procedure TsdJpegMarker.DoDebugOut(const AMessage: string);
begin
  if assigned(FOwner) then
    TJpegAccess(FOwner).DoDebugOut(AMessage);
end;

function TsdJpegMarker.GetByte(S: TStream): byte;
begin
  S.Read(Result, 1);
end;

class function TsdJpegMarker.GetWord(S: TStream): word;
var
  W: word;
begin
  S.Read(W, 2);
  Result := Swap(W);
end;

procedure TsdJpegMarker.LoadFromStream(S: TStream; Size: integer);
var
  i: integer;
  B: byte;
  Msg: string;
begin
  DoDebugOut(Format('<unknown marker %s, length:%d>', [IntToHex(FTag, 2), Size]));
  // show first bytes as hex
  for i := 0 to Min(Size, 32) - 1 do
  begin
    S.Read(B, 1);
    Msg := Msg + IntToHex(B, 2);
    if i mod 4 = 3 then
      Msg := Msg + '-';
  end;
  DoDebugOut(Msg + '...');
end;

procedure TsdJpegMarker.PutByte(S: TStream; B: byte);
begin
  S.Write(B, 1);
end;

procedure TsdJpegMarker.PutWord(S: TStream; W: word);
begin
  W := Swap(W);
  S.Write(W, 2);
end;

procedure TsdJpegMarker.SaveToStream(S: TStream);
begin
// default does nothing
  raise Exception.Create(Format('marker %s', [IntToHex(FTag, 2)]));
end;

{ TsdJpegMarkerList }

procedure TsdJpegMarkerList.Add(AItem: TObject);
begin
  inherited Add(AItem);
  if AItem is TsdJpegMarker then
    TsdJpegMarker(AItem).Owner := FOwner;
end;

function TsdJpegMarkerList.ByTag(ATag: byte): TsdJpegMarker;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Tag = ATag then
    begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

constructor TsdJpegMarkerList.Create(AOwner: TObject);
begin
  inherited Create(True);
  FOwner := AOwner;
end;

function TsdJpegMarkerList.GetItems(Index: integer): TsdJpegMarker;
begin
  Result := Get(Index);
end;

function TsdJpegMarkerList.HasMarker(ASet: TsdMarkerSet): boolean;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Tag in ASet then
    begin
      Result := True;
      exit;
    end;
  Result := False;
end;

procedure TsdJpegMarkerList.InsertAfter(ASet: TsdMarkerSet; AMarker: TsdJpegMarker);
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    if Items[i].Tag in ASet then
    begin
      Insert(i + 1, AMarker);
      exit;
    end;
  // If none found, just add the marker
  Add(AMarker);
end;

procedure TsdJpegMarkerList.RemoveMarkers(ASet: TsdMarkerSet);
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    if Items[i].Tag in ASet then
      Delete(i);
end;

{ TsdAPPnMarker }

function TsdAPPnMarker.ApplicationString: string;
begin
  Result := 'Unknown';
end;

constructor TsdAPPnMarker.Create(AInfo: TsdJpegCodingInfo; ATag: byte);
begin
  inherited;
  FData := TMemoryStream.Create;
end;

destructor TsdAPPnMarker.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TsdAPPnMarker.LoadFromStream(S: TStream; Size: integer);
var
  i, ASize: integer;
  App, B: byte;
  Msg: string;
  Chars: array of char;
begin
  StoreData(S, Size);
  App := Tag and $0F;
  DoDebugOut(Format('<APP%d %s marker, length:%d>', [App, ApplicationString, Size]));
  // show first bytes as hex
  ASize := Min(Size, 32);
  SetLength(Chars, ASize);
  for i := 0 to ASize - 1 do
  begin
    FData.Read(B, 1);
    Chars[i] := chr(B);
    Msg := Msg + IntToHex(B, 2);
    if i mod 4 = 3 then
      Msg := Msg + '-';
  end;
  DoDebugOut(Msg + '...');
  DoDebugOut(PChar(@Chars[0]));
end;

procedure TsdAPPnMarker.SaveToStream(S: TStream);
begin
  // Default just saves the stored data
  FData.Position := 0;
  S.CopyFrom(FData, FData.Size);
end;

procedure TsdAPPnMarker.StoreData(S: TStream; Size: integer);
begin
  // We store the data for later use
  FData.Clear;
  FData.CopyFrom(S, Size);
  FData.Position := 0;
end;

{ TsdDHTMarker }

procedure TsdDHTMarker.LoadFromStream(S: TStream; Size: integer);
var
  i, j, Idx, Count, InfoCount: integer;
  Item: PsdDHTMarkerInfo;
  B: byte;
  StreamPos: integer;
  Table: TsdHuffmanTable;
//  Line: string;
begin
  StreamPos := S.Position;
  // Define Huffman Table
  DoDebugOut(Format('<DHT marker, length:%d>', [Size]));
  SetLength(FMarkerInfo, 0);
  InfoCount := 0;
  repeat
    SetLength(FMarkerInfo, InfoCount + 1);
    Item := @FMarkerInfo[InfoCount];
    inc(InfoCount);
    B := GetByte(S);
    Item.Tc := B shr 4;
    Item.Th := B and $0F;
    // Number of elements for each bitsize
    S.Read(Item.BitLengths[0], 16);
    // Count values
    Count := 0;
    for i := 0 to 15 do inc(Count, Item.BitLengths[i]);
    // Set pointer and table info
    case Item.Tc of
    0:
      begin
        Table := FInfo.DCHuffmanTables[Item.Th];
        DoDebugOut(Format('DC Huffman table=%d, length=%d', [Item.Th, Count]));
      end;
    1:
      begin
        Table := FInfo.ACHuffmanTables[Item.Th];
        DoDebugOut(Format('AC Huffman table=%d, length=%d', [Item.Th, Count]));
      end;
    else
      raise EInvalidGraphic.Create(sInvalidTableClass);
    end;
    // Set table length
    Table.Count := Count;
    SetLength(Item.BitValues, Count);
    // Read values
    Idx := 0;
    for i := 0 to 15 do
    begin
      //Line := '';
      //if Item.BitLengths[i] > 0 then
      //  Line := Format('%2dbit:', [i + 1]);
      for j := 0 to Item.BitLengths[i] - 1 do
      begin
        Table[Idx].L := i + 1;
        Item.BitValues[Idx] := GetByte(S);
        Table[Idx].V := Item.BitValues[Idx];
        //if j > 0 then Line := Line + ',';
        //Line := Line + Format('%3d', [Item.BitValues[Idx]]);
        inc(Idx);
      end;
      //if length(Line) > 0 then
      //  DoDebugOut(Line);
    end;
  until (S.Position - StreamPos = Size);
end;

procedure TsdDHTMarker.SaveToStream(S: TStream);
var
  i, Count: integer;
  B: byte;
  Item: PsdDHTMarkerInfo;
  Table: TsdHuffmanTable;
  procedure SetTableValues;
  var
    i, j, Idx: integer;
  begin
    Idx := 0;
    for i := 0 to 15 do
      for j := 0 to Item.BitLengths[i] - 1 do
      begin
        Table[Idx].L := i + 1;
        Table[Idx].V := Item.BitValues[Idx];
        inc(Idx);
      end;
  end;
begin
  for i := 0 to length(FMarkerInfo) - 1 do
  begin
    Item := @FMarkerInfo[i];
    B := Item.Tc shl 4 + Item.Th;
    PutByte(S, B);
    case Item.Tc of
    0: Table := FInfo.DCHuffmanTables[Item.Th];
    1: Table := FInfo.ACHuffmanTables[Item.Th];
    end;
    Count := length(Item.BitValues);
    // Set table length
    Table.Count := Count;
    SetTableValues;
    // Number of elements for each bitsize
    S.Write(Item.BitLengths[0], 16);
    // Write values
    if Count > 0 then
      S.Write(Item.BitValues[0], Count);
  end;
end;

{ TsdDQTMarker }

procedure TsdDQTMarker.LoadFromStream(S: TStream; Size: integer);
var
  i, Count: integer;
  B: byte;
  P, T: byte;
  Table: TsdQuantizationTable;
  StreamPos: integer;
  function TabVal(x: integer): integer;
  begin
    Result := Table.Quant[cJpegForwardZigZag8x8[i * 8 + x]];
  end;
begin
  StreamPos := S.Position;
  // Define Quantization Table
  DoDebugOut(Format('<DQT marker, length:%d>', [Size]));
  // Read quantization table(s)
  SetLength(FTableIndices, 0);
  Count := 0;
  repeat
    B := GetByte(S);
    P := B shr 4;
    T := B and $0F;
    // Store for later use
    SetLength(FTableIndices, Count + 1);
    FTableIndices[Count] := T;
    inc(Count);
    // Initialize table
    Table := FInfo.QuantizationTables[T];
    Table.Precision := TsdQuantizationPrecision(P);
    case P of
    0:
      for i := 0 to 63 do
        Table.Quant[i] := GetByte(S);
    1:
      for i := 0 to 63 do
        Table.Quant[i] := GetWord(S);
    end;//case
    case Table.Precision of
    qp8bit: DoDebugOut(Format('QTable=%d precision=8bit', [T]));
    qp16bit: DoDebugOut(Format('QTable=%d precision=16bit', [T]));
    end;
    for i := 0 to 7 do
      DoDebugOut(Format('%3d %3d %3d %3d %3d %3d %3d %3d',
        [TabVal(0), TabVal(1), TabVal(2), TabVal(3), TabVal(4), TabVal(5), TabVal(6), TabVal(7)]));
  until (S.Position - StreamPos = Size);
end;

procedure TsdDQTMarker.SaveToStream(S: TStream);
var
  i, j: integer;
  B: byte;
  Table: TsdQuantizationTable;
begin
  for i := 0 to length(FTableIndices) - 1 do
  begin
    Table := FInfo.QuantizationTables[FTableIndices[i]];
    case Table.Precision of
    qp8bit:  B := $00;
    qp16bit: B := $10;
    end;
    B := B or FTableIndices[i];
    S.Write(B, 1);
    // Write table
    case Table.Precision of
    qp8bit:
      for j := 0 to 63 do
        PutByte(S, Table.Quant[j]);
    qp16bit:
      for j := 0 to 63 do
        PutWord(S, Table.Quant[j]);
    end;
  end;
end;

{ TsdDRIMarker }

procedure TsdDRIMarker.LoadFromStream(S: TStream; Size: integer);
begin
  // Define Restart Interval
  DoDebugOut(Format('<DRI marker, length:%d>', [Size]));
  // Read restart interval MCU count
  FInfo.RestartInterval := GetWord(S);
  DoDebugOut(Format('Restart interval: %d', [FInfo.RestartInterval]));
end;

procedure TsdDRIMarker.SaveToStream(S: TStream);
begin
  PutWord(S, FInfo.RestartInterval);
end;

{ TsdSOFnMarker }

procedure TsdSOFnMarker.LoadFromStream(S: TStream; Size: integer);
var
  i: integer;
  B, Nf: byte;
  Frame: TsdFrameComponent;
begin
  DoDebugOut(Format('<SOF%s marker, length:%d>', [IntToHex(Tag and $0F, 1), Size]));

  // Determine encoding
  case Tag of
  mkSOF0:
    begin
      FInfo.EncodingMethod := emBaselineDCT;
      DoDebugOut('Coding method: Baseline DCT');
    end;
  mkSOF1:
    begin
      FInfo.EncodingMethod := emExtendedDCT;
      DoDebugOut('Coding method: Extended DCT');
    end;
  mkSOF2:
    begin
      FInfo.EncodingMethod := emProgressiveDCT;
      DoDebugOut('Coding method: Progressive DCT');
    end;
  mkSOF3, mkSOF5..mkSOF7, mkSOF9..mkSOF11, mkSOF13..mkSOF15:
    begin
      // We do not yet support anything fancy
      DoDebugOut('Unsupported coding method');
      raise EInvalidGraphic.CreateFmt(sUnsupportedEncoding, [(Tag and $0F)]);
    end;
  end;//case

  // Read Frame Header
  FInfo.SamplePrecision := GetByte(S);
  FInfo.Height := GetWord(S);
  FInfo.Width := GetWord(S);
  // The weird case of FInfo.Y = 0: we expect a DNL marker somewhere telling
  // us the actual Y dimension. We set WaitForDNL to true.
  if FInfo.Height = 0 then
    FInfo.WaitForDNL := True;
  // Variable Nf: Number of image components in frame
  Nf := GetByte(S);
  FInfo.FrameCount := Nf;
  DoDebugOut(Format('Image %dx%d, %d frames, %dbit samples',
    [FInfo.Width, FInfo.Height, FInfo.FrameCount, FInfo.SamplePrecision]));
  for i := 0 to Nf - 1 do
  begin
    Frame := FInfo.Frames[i];
    Frame.ComponentID := GetByte(S);  // Image component (can be ASCII too!)
    B := GetByte(S);
    if Nf = 1 then
    begin
      // Jpeg spec specifies that with just one frame (no interlace), we need to
      // have a 1x1 MCU, so these ones should be 1, even if they read differently.
      Frame.HorzSampling := 1;
      Frame.VertSampling := 1;
    end else
    begin
      Frame.HorzSampling := B shr 4;     // Horizontal blocksize in MCU
      Frame.VertSampling := B and $0F;   // Vertical blocksize in MCU
    end;
    Frame.QTable := GetByte(S); // Index into quantization table array
    DoDebugOut(Format('Frame %d: %dx%d sampling ID=%s QTable=%d',
      [i, Frame.HorzSampling, Frame.VertSampling, IntToHex(Frame.ComponentID, 2), Frame.QTable]));
  end;
end;

procedure TsdSOFnMarker.SaveToStream(S: TStream);
var
  i: integer;
  B: byte;
  Frame: TsdFrameComponent;
begin
  // Write Frame Header
  PutByte(S, FInfo.SamplePrecision);
  PutWord(S, FInfo.Height);
  PutWord(S, FInfo.Width);
  PutByte(S, FInfo.FrameCount);
  for i := 0 to FInfo.FrameCount - 1 do
  begin
    Frame := FInfo.Frames[i];
    PutByte(S, Frame.ComponentID);
    B := Frame.HorzSampling shl 4 + Frame.VertSampling;
    PutByte(S, B);
    PutByte(S, Frame.QTable);
  end;
end;

{ TsdSOSMarker }

procedure TsdSOSMarker.FindScanComponent(AScan: TsdScanComponent; AId: byte);
var
  i: integer;
begin
  // Let's find the index of the component this one belongs to
  AScan.Component := -1;
  for i := 0 to FInfo.FrameCount - 1 do
    if FInfo.Frames[i].ComponentID = AId then
      AScan.Component := i;
  // Make sure we have a frame for this scan
  if AScan.Component = -1 then
    raise EInvalidGraphic.Create(sInvalidFrameRef);
end;

procedure TsdSOSMarker.LoadFromStream(S: TStream; Size: integer);
var
  i: integer;
  B, Cs: byte;
  Scan: TsdScanComponent;
begin
  // Start of Scan
  DoDebugOut('<SOS marker, coding starts>');
  // Variable Ns, number of image components in scan
  FScanCount := GetByte(S);
  FInfo.ScanCount := FScanCount;
  FInfo.Scans.Clear;
  SetLength(FMarkerInfo, FScanCount);
  if FScanCount = 1 then
    DoDebugOut('Single Channel')
  else
    DoDebugOut(Format('Interleaved (%d channels)', [FScanCount]));
  // read table specifiers
  for i := 0 to FScanCount - 1 do
  begin
    Scan := FInfo.Scans[i];
    Cs := GetByte(S); // Image component reference (can be ASCII too!)
    FMarkerInfo[i].ComponentID := Cs;
    FindScanComponent(Scan, Cs);
    B := GetByte(S);
    FMarkerInfo[i].DCTable := B shr 4;   // DC entropy table selector
    FMarkerInfo[i].ACTable := B and $0F; // AC entropy table selector
    Scan.DCTable := FMarkerInfo[i].DCTable;
    Scan.ACTable := FMarkerInfo[i].ACTable;
    Scan.Predictor^ := 0;       // Predictor (used for diff'ing the DC component)
    DoDebugOut(Format('Channel %d DCTable: %d, ACTable: %d', [Scan.Component, Scan.DCTable, Scan.ACTable]))
  end;
  // read Ss, Se, these are used in progressive scans
  FSpectralStart := GetByte(S);
  FInfo.SpectralStart := FSpectralStart;
  FSpectralEnd := GetByte(S);
  FInfo.SpectralEnd := FSpectralEnd;
  // read Ah, Al, these are used in progressive scans
  B := GetByte(S);
  FApproxHigh := B shr 4;
  FInfo.ApproxHigh := FApproxHigh;
  FApproxLow := B and $0F;
  FInfo.ApproxLow  := FApproxLow;
  // Following is entropy coded data
  if FInfo.EncodingMethod = emProgressiveDCT then
    DoDebugOut(Format('Progressive params: Ss=%d, Se=%d, Ah=%d, Al=%d', [FSpectralStart, FSpectralEnd, FApproxHigh, FApproxLow]));
end;

procedure TsdSOSMarker.SaveToStream(S: TStream);
// Save SOS data, and also apply it back to FInfo for use by the decoder. We can
// call it safely with S = nil, to just apply the scan data
var
  i: integer;
  B: byte;
  Scan: TsdScanComponent;
begin
  if assigned(S) then
    PutByte(S, FScanCount);
  // write table specifiers
  FInfo.ScanCount := FScanCount;
  for i := 0 to FScanCount - 1 do
  begin
    Scan := FInfo.Scans[i];
    if assigned(S) then
      PutByte(S, FMarkerInfo[i].ComponentID);
    FindScanComponent(Scan, FMarkerInfo[i].ComponentID);
    B := FMarkerInfo[i].DCTable shl 4 + FMarkerInfo[i].ACTable;
    if assigned(S) then
      PutByte(S, B);
    Scan.DCTable := FMarkerInfo[i].DCTable;
    Scan.ACTable := FMarkerInfo[i].ACTable;
    Scan.Predictor^ := 0;
  end;
  // Write Ss, Se, Ah and Al
  B := FApproxHigh shl 4 + FApproxLow;
  if assigned(S) then
  begin
    PutByte(S, FSpectralStart);
    PutByte(S, FSpectralEnd);
    PutByte(S, B);
  end;
  FInfo.SpectralStart := FSpectralStart;
  FInfo.SpectralEnd := FSpectralEnd;
  FInfo.ApproxHigh := FApproxHigh;
  FInfo.ApproxLow  := FApproxLow;
end;

{ TsdSOIMarker }

procedure TsdSOIMarker.LoadFromStream(S: TStream; Size: integer);
begin
  // Start of Image
  DoDebugOut('<SOI marker>');
end;

{ TsdEOIMarker }

procedure TsdEOIMarker.LoadFromStream(S: TStream; Size: integer);
begin
  // End of Image
  DoDebugOut('<EOI marker>');
end;

{ TsdRSTMarker }

procedure TsdRSTMarker.LoadFromStream(S: TStream; Size: integer);
begin
  DoDebugOut(Format('<RST%s marker>', [IntToHex(Tag and $0F, 1), Size]));
end;

{ TsdDNLMarker }

procedure TsdDNLMarker.LoadFromStream(S: TStream; Size: integer);
begin
  DoDebugOut('<DNL marker>');
  FInfo.Height := GetWord(S);
  DoDebugOut(Format('Image height: %d', [FInfo.Height]));
end;

procedure TsdDNLMarker.SaveToStream(S: TStream);
begin
  PutWord(S, FInfo.Height);
end;

{ TsdCOMMarker }

function TsdCOMMarker.GetComment: string;
begin
  SetLength(Result, FData.Size);
  if FData.Size = 0 then exit;
  FData.Position := 0;
  FData.Read(Result[1], FData.Size);
end;

procedure TsdCOMMarker.LoadFromStream(S: TStream; Size: integer);
var
  Comment: string;
begin
  DoDebugOut('<COM marker>');
  StoreData(S, Size);
  SetLength(Comment, Size);
  if Size > 0 then
    FData.Read(Comment[1], Size);
  DoDebugOut(Comment);
end;

procedure TsdCOMMarker.SetComment(const Value: string);
var
  Size: integer;
begin
  FData.Clear;
  Size := length(Value);
  if Size >=0 then exit;
  FData.Write(Value[1], Size);
end;

{ TsdJFIFMarker }

function TsdJFIFMarker.ApplicationString: string;
begin
  Result := 'JFIF';
end;

constructor TsdJFIFMarker.Create(AInfo: TsdJpegCodingInfo; ATag: byte);
begin
  inherited;
  // Set sensible defaults
  FVersion := 258;
  FUnits := juXandYAreDotsPerInch;
  FXDensity := 600;
  FYDensity := 600;
  // Save data
  SaveData;
end;

function TsdJFIFMarker.GetIsValid: boolean;
var
  Magic: array[0..4] of char;
begin
  Result := False;
  if FIsValid then
  begin
    Result := True;
    exit;
  end;
  FData.Position := 0;
  FData.Read(Magic, 5);
  FIsValid := (Magic = 'JFIF');
  if not FIsValid then exit;
  Result := True;
  FVersion := GetWord(FData);
  FData.Read(FUnits, 1);
  FXDensity := GetWord(FData);
  FYDensity := GetWord(FData);
  FXThumbnail := GetByte(FData);
  FYThumbnail := GetByte(FData);
end;

function TsdJFIFMarker.GetUnits: TsdJFIFUnits;
begin
  GetIsValid;
  Result := FUnits;
end;

function TsdJFIFMarker.GetVersion: word;
begin
  GetIsValid;
  Result := FVersion;
end;

function TsdJFIFMarker.GetXDensity: word;
begin
  GetIsValid;
  Result := FXDensity;
end;

function TsdJFIFMarker.GetXThumbnail: byte;
begin
  GetIsValid;
  Result := FXThumbnail;
end;

function TsdJFIFMarker.GetYDensity: word;
begin
  GetIsValid;
  Result := FYDensity;
end;

function TsdJFIFMarker.GetYThumbnail: byte;
begin
  GetIsValid;
  Result := FYThumbnail;
end;

procedure TsdJFIFMarker.SaveData;
var
  Magic: array[0..4] of char;
begin
  Magic := 'JFIF'#0;
  FData.Clear;
  FData.Write(Magic, 5);
  PutWord(FData, FVersion);
  FData.Write(FUnits, 1);
  PutWord(FData, FXDensity);
  PutWord(FData, FYDensity);
  PutWord(FData, FXThumbnail);
  PutWord(FData, FYThumbnail);
end;

{ TsdICCProfileMarker }

function TsdICCProfileMarker.ApplicationString: string;
begin
  Result := 'ICCProfile';
end;

function TsdICCProfileMarker.GetCurrentMarker: byte;
begin
  GetIsValid;
  Result := FCurrentMarker;
end;

function TsdICCProfileMarker.GetData: pointer;
var
  PData: PByte;
begin
  GetIsValid;
  if not FIsValid then
    Result := nil
  else
  begin
    PData := FData.Memory;
    inc(PData, 14);
    Result := PData;
  end;
end;

function TsdICCProfileMarker.GetDataLength: integer;
begin
  GetIsValid;
  if not FIsValid then
    Result := 0
  else
    Result := FData.Size - 14;
end;

function TsdICCProfileMarker.GetIsValid: boolean;
var
  Magic: array[0..11] of char;
begin
  Result := False;
  if FIsValid then
  begin
    Result := True;
    exit;
  end;
  FData.Position := 0;
  FData.Read(Magic, 12);
  FIsValid := (Magic = 'ICC_PROFILE');
  if not FIsValid then exit;
  Result := True;
  FCurrentMarker := GetByte(FData);
  FMarkerCount := GetByte(FData);
  // ICC-Profile data follows
end;

function TsdICCProfileMarker.GetMarkerCount: byte;
begin
  GetIsValid;
  Result := FMarkerCount;
end;

procedure TsdICCProfileMarker.SetCurrentMarker(const Value: byte);
begin
  FData.Position := 12;
  PutByte(FData, Value);
  FCurrentMarker := Value;
end;

procedure TsdICCProfileMarker.SetDataLength(const Value: integer);
var
  Magic: string;
begin
  FData.Size := Value + 14;
  FData.Position := 0;
  Magic := 'ICC_PROFILE'#0;
  FData.Write(Magic[1], 12);
end;

procedure TsdICCProfileMarker.SetMarkerCount(const Value: byte);
begin
  FData.Position := 13;
  PutByte(FData, Value);
  FMarkerCount := Value;
end;

{ TsdEXIFMarker }

function TsdEXIFMarker.ApplicationString: string;
begin
  Result := 'EXIF';
end;

function TsdEXIFMarker.IsG3FAX: boolean;
var
  Magic: array[0..4] of char;
begin
  Result := False;
  if FData.Size <> 10 then exit;
  FData.Position := 0;
  FData.Read(Magic, 5);
  Result := Magic = 'G3FAX';
end;

{ TsdIPTCMarker }

function TsdIPTCMarker.ApplicationString: string;
begin
  Result := 'IPTC';
end;

{ TsdAdobeApp14Marker }

function TsdAdobeApp14Marker.ApplicationString: string;
begin
  Result := 'Adobe';
end;

constructor TsdAdobeApp14Marker.Create(AInfo: TsdJpegCodingInfo; ATag: byte);
begin
  inherited;
  // Defaults
  FVersion := 100;
  SaveData;
end;

function TsdAdobeApp14Marker.GetIsValid;
var
  Magic: array[0..4] of char;
begin
  Result := False;
  if FIsValid then
  begin
    Result := True;
    exit;
  end;
  // Check length of Adobe marker
  if FData.Size <> 12 then exit;
  FData.Position := 0;
  FData.Read(Magic, 5);
  FIsValid := (Magic = 'Adobe');
  if not FIsValid then exit;
  Result := True;
  FVersion := GetWord(FData);
  FFlags0 := GetWord(FData);
  FFlags1 := GetWord(FData);
  FTransform := GetByte(FData);
end;

function TsdAdobeApp14Marker.GetTransform: byte;
begin
  GetIsValid;
  Result := FTransform;
end;

procedure TsdAdobeApp14Marker.SaveData;
var
  Magic: array[0..4] of char;
begin
  Magic := 'Adobe';
  FData.Clear;
  FData.Write(Magic, 5);
  PutWord(FData, FVersion);
  PutWord(FData, FFlags0);
  PutWord(FData, FFlags1);
  PutByte(FData, FTransform);
end;

procedure TsdAdobeApp14Marker.SetTransform(const Value: byte);
begin
  GetIsValid;
  FTransform := Value;
  SaveData;
end;

{ TsdJpegICCProfile }

function TsdJpegICCProfile.GetData: pointer;
begin
  if length(FData) > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

function TsdJpegICCProfile.GetDataLength: integer;
begin
  Result := length(FData);
end;

procedure TsdJpegICCProfile.LoadFromFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TsdJpegICCProfile.LoadFromStream(S: TStream);
begin
  SetLength(FData, S.Size);
  S.Position := 0;
  S.Read(FData[0], S.Size);
end;

procedure TsdJpegICCProfile.ReadFromMarkerList(AList: TsdJpegMarkerList);
var
  i, j, DataLen, MarkerCount: integer;
  Markers: array of TsdICCProfileMarker;
  M: TsdICCProfileMarker;
  P: PByte;
begin
  // Determine total length and get list of markers
  DataLen := 0;
  MarkerCount := 0;
  SetLength(Markers, AList.Count);
  for i := 0 to AList.Count - 1 do
    if AList[i].Tag = mkApp2 then
    begin
      M := TsdICCProfileMarker(AList[i]);
      if not M.IsValid then continue;
      inc(DataLen, M.DataLength);
      Markers[MarkerCount] := M;
      inc(MarkerCount);
    end;
  if DataLen <= 0 then exit;

  // Sort markers by index
  for i := 0 to MarkerCount - 2 do
    for j := i + 1 to MarkerCount - 1 do
      if Markers[i].CurrentMarker > Markers[j].CurrentMarker then
      begin
        M := Markers[i];
        Markers[i] := Markers[j];
        Markers[j] := M;
      end;

  // Extract marker data into our data
  SetLength(FData, DataLen);
  P := @FData[0];
  for i := 0 to MarkerCount - 1 do
  begin
    Move(Markers[i].Data^, P^, Markers[i].DataLength);
    inc(P, Markers[i].DataLength);
  end;
end;

procedure TsdJpegICCProfile.SaveToFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TsdJpegICCProfile.SaveToStream(S: TStream);
begin
  if length(FData) > 0 then
    S.Write(FData[0], length(FData))
end;

procedure TsdJpegICCProfile.WriteToMarkerList(AList: TsdJpegMarkerList);
const
  cChunkSize = 60000;
var
  i, Count, Chunk, Left, Base: integer;
  Markers: array of TsdICCProfileMarker;
  P: Pbyte;
begin
  // Create an array of markers with the profile data
  Count := (DataLength + cChunkSize - 1) div cChunkSize;
  Left := DataLength;
  P := Data;
  SetLength(Markers, Count);
  for i := 0 to Count - 1 do
  begin
    Markers[i] := TsdICCProfileMarker.Create(nil, mkApp2);
    Chunk := Min(Left, cChunkSize);
    Markers[i].DataLength := Chunk;
    Move(P^, Markers[i].Data^, Chunk);
    Markers[i].CurrentMarker := i + 1;
    Markers[i].MarkerCount := Count;
    inc(P, Chunk);
    dec(Left, Chunk);
  end;
  // Insert them into the markerlist
  Base := Min(AList.Count, 2);
  for i := Count - 1 downto 0 do
    AList.Insert(Base, Markers[i]);
end;

end.
