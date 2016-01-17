unit MegaCastEncoder;

interface

uses
  MegaCastUtils, ZLibUtils, IJLUtils,
  Windows, Classes, SysUtils, Graphics;

type
  TMegaCastEncoder = class (TComponent)
  private
    FDataZlib : TMemoryStream;
    FDataJPeg : TMemoryStream;
    procedure do_ZlibCompress(ABlockData:pointer; ABlockSize:integer);
    procedure do_JPegCompress(ABlockData:pointer; ABlockSize:integer);
    procedure do_Ouput(AHeader:TMegaCastBlockUnitHeader; AStream:TMemoryStream);
  private
    FOnNewBlockUnit: TNewBlockUnitEvent;
    FDataOut: TMemoryStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute(ABlockUnit:pointer; AUnitSize:integer);
  published
    property DataOut : TMemoryStream read FDataOut;
    property OnNewBlockUnit : TNewBlockUnitEvent read FOnNewBlockUnit write FOnNewBlockUnit;
  end;

implementation

{ TMegaCastEncoder }

constructor TMegaCastEncoder.Create(AOwner: TComponent);
begin
  inherited;

  FDataOut := TMemoryStream.Create;
  FDataZlib := TMemoryStream.Create;
  FDataJPeg := TMemoryStream.Create;
end;

destructor TMegaCastEncoder.Destroy;
begin
  FreeAndNil(FDataOut);
  FreeAndNil(FDataZlib);
  FreeAndNil(FDataJPeg);

  inherited;
end;

procedure TMegaCastEncoder.do_JPegCompress(ABlockData: pointer; ABlockSize: integer);
var
  Bitmap : TBitmap;
begin
  FDataJPeg.Clear;

  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := _PixelFormat;
    Bitmap.Width  := _BlockSize;
    Bitmap.Height := _BlockSize;
    Move(ABlockData^, Bitmap.ScanLine[Bitmap.Height-1]^, ABlockSize);

    BitmapToJpeg(Bitmap, FDataJPeg, 70);

    Assert(FDataJPeg.Size > 0, 'TMegaCastEncoder.do_JPegCompress: JPeg 압축 오류');
  finally
    Bitmap.Free;
  end;
end;

procedure TMegaCastEncoder.do_Ouput(AHeader: TMegaCastBlockUnitHeader; AStream: TMemoryStream);
begin
  {$IFDEF DEBUG}
//    OutputDebugString(PChar(Format('TMegaCastEncoder.do_Ouput: DataType=%d, BlockSize=%d', [Integer(AHeader.BlockType), AStream.Size])));
  {$ENDIF}

  FDataOut.Clear;

  FDataOut.Write(AHeader, SizeOf(AHeader));

  AStream.Position := 0;
  FDataOut.CopyFrom(AStream, AStream.Size);

  if Assigned(FOnNewBlockUnit) then FOnNewBlockUnit(Self, FDataOut.Memory, FDataOut.Size);
end;

procedure TMegaCastEncoder.do_ZlibCompress(ABlockData: pointer; ABlockSize: integer);
var
  msData : TMemoryStream;
begin
  FDataZlib.Clear;
  
  msData := TMemoryStream.Create;
  try
    msData.Write(ABlockData^, ABlockSize);

    msData.Position := 0;
    ShrinkStreamSlow(msData, FDataZlib);
  finally
    msData.Free;
  end;
end;

procedure TMegaCastEncoder.Execute(ABlockUnit: pointer; AUnitSize: integer);
var
  Header : TMegaCastBlockUnitHeader;
  pHeader : ^TMegaCastBlockUnitHeader;
begin
  pHeader := ABlockUnit;

  Assert(AUnitSize = _BlockUnitSize, ClassName+'.Execute: 데이터 크기에 이상이 있습니다.');
  Assert(pHeader^.BlockType = mcdtBitmap, 'TMegaCastEncoder.Execute: 데이터포멧 에러');

  Header := pHeader^;

  // 헤더를 건너뛰고 실제 데이터만 압축한다.
  Inc(pHeader);
  do_ZlibCompress(pHeader, AUnitSize-SizeOf(TMegaCastBlockUnitHeader));
  do_JPegCompress(pHeader, AUnitSize-SizeOf(TMegaCastBlockUnitHeader));

  if FDataZlib.Size > FDataJPeg.Size then begin
    Header.BlockType := mcdtJBlock;
    do_Ouput(Header, FDataJPeg);
  end else begin
    Header.BlockType := mcdtZBlock;
    do_Ouput(Header, FDataZlib);
  end;
end;

end.
