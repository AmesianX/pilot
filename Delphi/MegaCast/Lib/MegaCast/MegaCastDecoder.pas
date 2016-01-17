unit MegaCastDecoder;

interface

uses
  MegaCastUtils, ThreadRepeater, ZLibUtils, IJLUtils,
  Windows, Classes, SysUtils, SyncObjs, TypInfo, Graphics;

type
  TNeedBlockUnitEvent = function (Sender:TObject; var ABlockUnit:pointer; var AUnitSize:integer):boolean of object;

  TMegaCastDecoder = class (TComponent)
  private
    FBitmap : TBitmap;
    FOldTick, FTickNew : cardinal;
    FCS : TCriticalSection;
    FThreadRepeater : TThreadRepeater;
    procedure on_Execute(Sender:TObject);
    procedure do_ZlibDecompress(ABlockUnit:pointer; AUnitSize:integer);
    procedure do_JPegDecompress(ABlockUnit:pointer; AUnitSize:integer);
  private
    FOnNeedBlockUnit: TNeedBlockUnitEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Stop;

    function GetBitmap(ABitmap:TBitmap):boolean;

    procedure SetFrameSize(AFrameSize:TFrameSize);
  published
    property OnNeedBlockUnit : TNeedBlockUnitEvent read FOnNeedBlockUnit write FOnNeedBlockUnit; 
  end;

implementation

{ TMegaCastDecoder }

constructor TMegaCastDecoder.Create(AOwner: TComponent);
begin
  inherited;

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := _PixelFormat;

  FThreadRepeater := TThreadRepeater.Create(Self);
  FThreadRepeater.Execute(on_Execute);

  FCS := TCriticalSection.Create;

  SetFrameSize(FrameSize(1024, 768));
end;

destructor TMegaCastDecoder.Destroy;
begin
  FThreadRepeater.Stop;

  FreeAndNil(FCS);
  FreeAndNil(FThreadRepeater);
  FreeAndNil(FBitmap);

  inherited;
end;

procedure TMegaCastDecoder.do_JPegDecompress(ABlockUnit: pointer;
  AUnitSize: integer);
var
  X, Y : integer;
  msDataIn, msDataOut : TMemoryStream;
  pHeader : ^TMegaCastBlockUnitHeader;
begin
  pHeader := ABlockUnit;
  X := pHeader^.X;
  Y := pHeader^.Y;

  msDataIn := TMemoryStream.Create;
  msDataOut := TMemoryStream.Create;
  try
    // 헤더만큼 건너 뛴다.
    Inc(pHeader);

    msDataIn.Write(pHeader^, AUnitSize-SizeOf(TMegaCastBlockUnitHeader));

    JpegToBitmapStream(msDataIn, msDataOut);

    FCS.Enter;
    try
      FTickNew := GetTickCount;
      DrawBlockOnBitmap(msDataOut.Memory, msDataOut.Size, X, Y, FBitmap);
    finally
      FCS.Leave;
    end;
  finally
    msDataIn.Free;
    msDataOut.Free;
  end;
end;

procedure TMegaCastDecoder.do_ZlibDecompress(ABlockUnit: pointer;
  AUnitSize: integer);
var
  X, Y : integer;
  pHeader : ^TMegaCastBlockUnitHeader;
  msDataIn, msDataOut : TMemoryStream;
begin
  pHeader := ABlockUnit;
  X := pHeader^.X;
  Y := pHeader^.Y;

  msDataIn := TMemoryStream.Create;
  msDataOut := TMemoryStream.Create;
  try
    // 헤더만큼 건너 뛴다.
    Inc(pHeader);

    msDataIn.Write(pHeader^, AUnitSize-SizeOf(TMegaCastBlockUnitHeader));

    msDataIn.Position := 0;
    ExpandStream(msDataIn, msDataOut);

    FCS.Enter;
    try
      FTickNew := GetTickCount;
      DrawBlockOnBitmap(msDataOut.Memory, msDataOut.Size, X, Y, FBitmap);
    finally
      FCS.Leave;
    end;
  finally
    msDataIn.Free;
    msDataOut.Free;
  end;
end;

function TMegaCastDecoder.GetBitmap(ABitmap: TBitmap): boolean;
begin
  FCS.Enter;
  try
    Result := FOldTick <> FTickNew;
    if not Result then Exit;
    FOldTick := FTickNew;

    ABitmap.PixelFormat := _PixelFormat;
    ABitmap.Width  := FBitmap.Width;
    ABitmap.Height := FBitmap.Height;
    Move(FBitmap.ScanLine[FBitmap.Height-1]^, ABitmap.ScanLine[ABitmap.Height-1]^, FBitmap.Width*FBitmap.Height*_PixelSize);
  finally
    FCS.Leave;
  end;
end;

procedure TMegaCastDecoder.on_Execute(Sender: TObject);
var
  BlockUnit : pointer;
  UnitSize : integer;
  pHeader : ^TMegaCastBlockUnitHeader;
begin
  if not Assigned(FOnNeedBlockUnit) then begin
    Sleep(10);
    Exit;
  end;

  while FOnNeedBlockUnit(Self, BlockUnit, UnitSize) do begin
    try
      pHeader := BlockUnit;
      case pHeader^.BlockType of
        mcdtZBlock : do_ZlibDecompress(BlockUnit, UnitSize);
        mcdtJBlock : do_JPegDecompress(BlockUnit, UnitSize);
        else raise Exception.Create('TMegaCastDecoder.on_Execute: 지원되지 않는 포멧입니다.');
      end;
      {$IFDEF DEBUG}
//        OutputDebugString(PChar(Format(
//          ClassName+'.on_Execute: BlockType=%s, X=%d, Y=%d',
//          [
//            GetEnumName(TypeInfo(TMegaCastBlockType), Integer(pHeader^.BlockType)),
//            pHeader^.X, pHeader^.Y
//          ]
//        )));
      {$ENDIF}
    finally
      if BlockUnit <> nil then FreeMem(BlockUnit);
    end;
  end;

  Sleep(1);
end;

procedure TMegaCastDecoder.SetFrameSize(AFrameSize: TFrameSize);
var
  iWidth, iHeight : integer;
begin
  FCS.Enter;
  try
    iWidth := AFrameSize.Width div _BlockSize;
    if (AFrameSize.Width mod _BlockSize) <> 0 then Inc(iWidth);

    iHeight := AFrameSize.Height div _BlockSize;
    if (AFrameSize.Height mod _BlockSize) <> 0 then Inc(iHeight);

    // _BlockSize에 나누어 떨어지도록 크기를 확장한다.
    FBitmap.Width  := iWidth *_BlockSize;
    FBitmap.Height := iHeight*_BlockSize;
  finally
    FCS.Leave;
  end;
end;

procedure TMegaCastDecoder.Stop;
begin
  FThreadRepeater.Stop;
end;

end.
