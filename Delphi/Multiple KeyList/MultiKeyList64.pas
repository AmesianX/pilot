unit MultiKeyList64;

interface

uses
  BinaryList,
  Classes, SyncObjs;


const
  MaxCountPerList : Integer = 8192; // 가장 효율적으로 추정

type
  TMultiKeyList64 = class
  private
    FSubList : TList;
    FCS : TCriticalSection;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value : Int64):Boolean;
    procedure Clear;

    property Count : Integer read GetCount;
  end;

implementation

function CompareLogic(Item1, Item2: Pointer): Integer;
var
  pItem1 : ^int64 absolute Item1;
  pItem2 : ^int64 absolute Item2;
begin
  if pItem1^ > pItem2^ then Result := 1
  else if pItem1^ < pItem2^ then Result := -1
  else Result := 0;
end;

{ TMultiKeyList64 }

function TMultiKeyList64.Add(Value: Int64): Boolean;
var
  I: Integer;
  pItem : ^int64;
  Index : Integer;
begin
  if Value = 0 then begin
    Result := false;
    Exit;
  end;

  FCS.Enter;
  try
    if Count >= (FSubList.Count * MaxCountPerList) then begin
      FSubList.Add(TBinaryList.Create);
    end;

    i := 0;
    New(pItem);
    pItem^ := Value;

    while i < FSubList.Count  do begin
      Index := TBinaryList(FSubList.Items[i]).Search(pItem, CompareLogic);
      if Index <> -1 then begin
        Dispose(pItem);
        Result := false;
        Exit;
      end;
      Inc(I);
    end;

    TBinaryList(FSubList.Items[FSubList.Count - 1]).InsertByOrder(pItem, CompareLogic);
    Result := true;
  finally
    FCS.Leave;
  end;
end;

procedure TMultiKeyList64.Clear;
var
  i, j : Integer;
begin
  FCS.Enter;
  try
    for I := 0 to FSubList.Count - 1 do begin
      for j := 0 to TBinaryList(FSubList.Items[i]).Count - 1 do begin
        Dispose(TBinaryList(FSubList.Items[i]).Items[j]);
      end;
      TObject(FSubList.Items[i]).Free;
    end;
    FSublist.Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TMultiKeyList64.Create;
begin
  FCS := TCriticalSection.Create;
  FSubList := TList.Create;
end;

destructor TMultiKeyList64.Destroy;
begin
  Clear;
  FSubList.Free;
  FCS.Free;
  inherited;
end;

function TMultiKeyList64.GetCount: Integer;
var
  I : Integer;
begin
  FCS.Enter;
  try
    Result := 0;
    for I := 0 to FSubList.Count - 1 do begin
      Inc(Result, TBinaryList(FSubList.Items[i]).Count);
    end;
  finally
    FCS.Leave;
  end;
end;

end.
