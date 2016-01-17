//
//  Author: Nils Haeck M.Sc.
//  Copyright (c) 2007 SimDesign B.V.
//  More information: www.simdesign.nl or n.haeck@simdesign.nl
//
//  This software may ONLY be used or replicated in accordance with
//  the LICENSE found in this source distribution.
//
unit sdJpegHuffman;

interface

uses
  Windows, Classes, SysUtils, sdJpegFormat, sdJpegTypes, sdJpegBitstream, sdJpegConsts,
  Contnrs, SortedLists, sdJpegMarkers;

type

  TsdEntropyCoder = class(TPersistent)
  public
    constructor Create; virtual;
  end;

  TsdEntropyCoderList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdEntropyCoder;
    procedure SetItems(Index: integer; const Value: TsdEntropyCoder);
  public
    property Items[Index: integer]: TsdEntropyCoder read GetItems write SetItems; default;
  end;

  // Generic Huffman coder implementing shared methods
  TsdHuffmanCoder = class(TsdEntropyCoder)
  private
  protected
    FCodes: array of TsdHuffmanCode;
  public
    procedure GenerateCodeTable(ATable: TsdHuffmanTable); virtual;
  end;

  // Generic Huffman decoder
  TsdHuffmanDecoder = class(TsdHuffmanCoder)
  private
  protected
    FLookup: array of TsdHuffLookupTable;
    FLookupCount: word;
  public
    procedure AddToLookupTable(Table, Code, Len, Value: integer);
    procedure GenerateLookupTables(Table: TsdHuffmanTable); virtual;
  end;

  // General 8-bit huffman decoder
  Tsd8bitHuffmanDecoder = class(TsdHuffmanDecoder)
  public
    procedure GenerateLookupTables(Table: TsdHuffmanTable); override;
  end;

  TsdHuffmanNode = class
  private
    FBitCount: integer;
    FCount: integer;
    FCode: PsdHuffmanCode;
    FB0: TsdHuffmanNode;
    FB1: TsdHuffmanNode;
  public
    destructor Destroy; override;
    property BitCount: integer read FBitCount write FBitCount;
    property Count: integer read FCount write FCount;
    property Code: PsdHuffmanCode read FCode write FCode;
    property B0: TsdHuffmanNode read FB0 write FB0;
    property B1: TsdHuffmanNode read FB1 write FB1;
  end;

  TsdHuffmanNodeList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TsdHuffmanNode;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    property Items[Index: integer]: TsdHuffmanNode read GetItems; default;
  end;

  // General 8-bit huffman encoder
  Tsd8bitHuffmanEncoder = class(TsdHuffmanCoder)
  private
    FHistogram: Tsd8bitHuffmanHistogram;
    FNodes: TsdHuffmanNodeList;
    function GetHistogram: Psd8bitHuffmanHistogram;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure GenerateCodeTable(ATable: TsdHuffmanTable); override;
    procedure OptimiseHuffmanFromHistogram(var Item: TsdDHTMarkerInfo);
    property Histogram: Psd8bitHuffmanHistogram read GetHistogram;
  end;

implementation

{ TsdEntropyCoder }

constructor TsdEntropyCoder.Create;
begin
  inherited Create;
end;

{ TsdEntropyCoderList }

function TsdEntropyCoderList.GetItems(Index: integer): TsdEntropyCoder;
begin
  if Index >= Count then Count := Index + 1;
  Result := Get(Index);
end;

procedure TsdEntropyCoderList.SetItems(Index: integer;
  const Value: TsdEntropyCoder);
begin
  if Index >= Count then Count := Index + 1;
  Put(Index, Value);
end;

{ TsdHuffmanCoder }

procedure TsdHuffmanCoder.GenerateCodeTable(ATable: TsdHuffmanTable);
var
  i, k, Idx, Len: integer;
  Code, Size: integer;
  MaxVal: integer;
begin
  // Generate a list of codes for the table (See Fig. C.2)
  Code := 0;
  MaxVal := 0;
  Size := ATable[0].L;
  k := 0;
  Len := ATable.Count;
  while k < Len do
  begin
    while (k < Len) and (ATable[k].L = Size) do
    begin
      ATable[k].Code := Code;
      if ATable[k].V > MaxVal then
        MaxVal := ATable[k].V;
      inc(Code);
      inc(k);
    end;
    Code := Code shl 1;
    inc(Size);
  end;

  SetLength(FCodes, MaxVal + 1); // 0..MaxVal
  for i := 0 to ATable.Count - 1 do
  begin
    Idx := ATable[i].V;
    FCodes[Idx].L := ATable[i].L;
    FCodes[Idx].V := ATable[i].V;
    FCodes[Idx].Code := ATable[i].Code;
  end;

end;

{ TsdHuffmanDecoder }

procedure TsdHuffmanDecoder.AddToLookupTable(Table, Code, Len, Value: integer);
var
  i, Iter, Mask: integer;
  Base: integer;
  Next: integer;
  Lookup: PsdHuffLookupTable;
begin
  Lookup := @FLookup[Table];
  if Len <= 8 then
  begin
    // Fill all the lsb bit entries with the same value
    Iter := 1 shl (8 - Len);
    Base := Code shl (8 - Len);
    for i := 0 to Iter - 1 do
    begin
      Lookup.Len  [Base + i] := Len;
      Lookup.Value[Base + i] := Value;
    end;
  end else
  begin
    // We need to follow a table or instantiate one
    Base := Code shr (Len - 8);
    Next := Lookup.Value[Base];
    if Next = 0 then
    begin
      // No followup table yet, create one
      inc(FLookupCount);
      if (length(FLookup) <= FLookupCount) then
        SetLength(FLookup, length(FLookup) * 2);
      // Next table, set its pointer
      Next := FLookupCount;
      FLookup[Table].Value[Base] := Next;
    end;
    // There is a follow up table, add
    Mask := 1 shl (Len - 8) - 1;
    AddToLookupTable(Next, Code and Mask, Len - 8, Value);
  end;
end;

procedure TsdHuffmanDecoder.GenerateLookupTables(Table: TsdHuffmanTable);
begin
  // Generate the code table first
  GenerateCodeTable(Table);
  // Start with clean 4 lookup tables
  SetLength(FLookup, 0);
  SetLength(FLookup, 4);
  FLookupCount := 0;
  // Default does nothing more
end;

{ Tsd8bitHuffmanDecoder }

procedure Tsd8bitHuffmanDecoder.GenerateLookupTables(Table: TsdHuffmanTable);
var
  i: integer;
begin
  inherited;
  for i := 0 to length(FCodes) - 1 do begin
    if FCodes[i].L > 0 then
      AddToLookupTable(0, FCodes[i].Code, FCodes[i].L, i);
  end;
end;

{ TsdHuffmanNode }

destructor TsdHuffmanNode.Destroy;
begin
  FreeAndNil(FB0);
  FreeAndNil(FB1);
  inherited;
end;

{ TsdHuffmanNodeList }

function TsdHuffmanNodeList.DoCompare(Item1, Item2: TObject): integer;
var
  L1, L2: TsdHuffmanNode;
begin
  // Sort by count, smallest first
  L1 := TsdHuffmanNode(Item1);
  L2 := TsdHuffmanNode(Item2);
  // Compare by bitcount first (smallest bitcount first)
  Result := CompareInteger(L1.BitCount, L2.BitCount);
  if Result = 0 then
    // Compare by frequency count (largest count first)
    Result := -CompareInteger(L1.Count, L2.Count);
end;

function TsdHuffmanNodeList.GetItems(Index: integer): TsdHuffmanNode;
begin
  Result := Get(Index);
end;

{ Tsd8bitHuffmanEncoder }

constructor Tsd8bitHuffmanEncoder.Create;
begin
  inherited;
  // do not own objects
  FNodes := TsdHuffmanNodeList.Create(False);
end;

destructor Tsd8bitHuffmanEncoder.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

procedure Tsd8bitHuffmanEncoder.GenerateCodeTable(ATable: TsdHuffmanTable);
var
  i: integer;
begin
  if ATable.Count = 0 then
  begin
    // Uninitialized table: create just codes for histogramming
    SetLength(FCodes, 256);
    for i := 0 to 255 do
      FCodes[i].V := i;
    exit;
  end;
  inherited;
end;

function Tsd8bitHuffmanEncoder.GetHistogram: Psd8bitHuffmanHistogram;
begin
  Result := @FHistogram;
end;

procedure Tsd8bitHuffmanEncoder.OptimiseHuffmanFromHistogram(var Item: TsdDHTMarkerInfo);
// Create an optimized huffman table from the data gathered in the histogram by
// the dry-run
var
  i: integer;
  N, N0, N1, Top: TsdHuffmanNode;
  // Recursive procedure: add values with their bitcount to the nodelist
  procedure AddBranch(ABranch: TsdHuffmanNode; ABitCount: integer);
  begin
    // Branch B0
    if assigned(ABranch.B0.Code) then
    begin
      ABranch.B0.BitCount := ABitCount;
      FNodes.Add(ABranch.B0);
    end else
      AddBranch(ABranch.B0, ABitCount + 1);
    // Branch B1
    if assigned(ABranch.B1.Code) then
    begin
      ABranch.B1.BitCount := ABitCount;
      FNodes.Add(ABranch.B1);
    end else
      AddBranch(ABranch.B1, ABitCount + 1);
  end;
begin
  // Start by adding nodes in sorted fashion
  FNodes.Clear;
  for i := 0 to length(FCodes) - 1 do
  begin
    if FHistogram[i] = 0 then continue;
    N := TsdHuffmanNode.Create;
    N.Code := @FCodes[i];
    N.Count := FHistogram[i];
    FNodes.Add(N);
  end;

  // Initialize huffman data
  SetLength(Item.BitValues, FNodes.Count);
  for i := 0 to 15 do
    Item.BitLengths[i] := 0;
  if FNodes.Count = 0 then exit;

  // Repeat combining nodes until there's only one
  while FNodes.Count >= 2 do
  begin
    // Two last nodes with smallest frequency count
    N0 := FNodes[FNodes.Count - 1];
    N1 := FNodes[FNodes.Count - 2];
    // Delete two last from list
    FNodes.Delete(FNodes.Count - 1);
    FNodes.Delete(FNodes.Count - 1);
    // New containing node
    N := TsdHuffmanNode.Create;
    N.B0 := N0;
    N.B1 := N1;
    N.Count := N0.Count + N1.Count;
    // Add new one to list (sorted)
    FNodes.Add(N);
  end;

  // Top item
  Top := FNodes[0];
  FNodes.Clear;

  // Start adding them again, now sorted by bitcount
  if assigned(Top.Code) then
  begin
    // If there is only one, we add it directly with bitcount 1
    Top.BitCount := 1;
    FNodes.Add(Top);
  end else
    // Recursive call on the tree
    AddBranch(Top, 1);

  // Since our table is compacted, and the jpeg spec says we must not have codes
  // with all ones, we will increase the bitcount of the last item
  N := FNodes[FNodes.Count - 1];
  N.BitCount := N.BitCount + 1;

  // Check maximum bit count; this should NOT exceed 16 bits for jpeg
  while FNodes[FNodes.Count - 1].BitCount > 16 do
  begin
    // Extract last two with largest bitcounts
    N0 := FNodes[FNodes.Count - 1];
    N1 := FNodes[FNodes.Count - 2];
    FNodes.Delete(FNodes.Count - 1);
    FNodes.Delete(FNodes.Count - 1);
    // Find item with at least 2 bits less
    i := FNodes.Count - 1;
    while FNodes[i].BitCount > N0.BitCount - 2 do
      dec(i);
    N := FNodes[i];
    FNodes.Delete(i);
    // Increment this leaf, decrement one of the other two, and set the other
    // to the same as this one. This preserves bitspace
    N.BitCount := N.BitCount + 1;
    N0.BitCount := N0.BitCount - 1;
    N1.BitCount := N.BitCount;
    // Add these again in a sorted way
    FNodes.Add(N);
    FNodes.Add(N0);
    FNodes.Add(N1);
  end;

  // We should now have a sorted list of codes by bitcount, and we can construct
  // the huffman table
  for i := 0 to FNodes.Count - 1 do
  begin
    N := FNodes[i];
    inc(Item.BitLengths[N.BitCount - 1]);
    Item.BitValues[i] := N.Code.V;
  end;
  FNodes.Clear;
  Top.Free;
end;

end.
