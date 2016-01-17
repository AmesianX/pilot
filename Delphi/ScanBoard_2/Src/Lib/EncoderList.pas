unit EncoderList;

interface

uses
  Classes, Windows, SysUtils;

type
  TEncoderList = class(TComponent)
  private
    FList: TList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function Add(const AData: Pointer; const ASize: Integer): Integer;
  end;

implementation

{ TEncoderList }

function TEncoderList.Add(const AData: Pointer; const ASize: Integer): Integer;
begin
  Result := FList.Add(AData);
end;

constructor TEncoderList.Create(AOwner: TComponent);
begin
  inherited;

  FList := TList.Create;
end;

destructor TEncoderList.Destroy;
begin
  FList.Free;

  inherited;
end;

end.
