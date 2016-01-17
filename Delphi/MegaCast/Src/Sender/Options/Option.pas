unit Option;

interface

uses
  OptionBase,
  Classes, SysUtils;

type
  TOption = class(TOptionBase)
  public
    constructor Create(AOwner: TComponent); override;
    class function Obj:TOption;
  end;

implementation

var
  MyObj : TOption = nil;

{ TOption }

constructor TOption.Create(AOwner: TComponent);
begin
  inherited;

  FIsAdmin := true;
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then MyObj := TOption.Create(nil);
  Result := MyObj;
end;

end.

