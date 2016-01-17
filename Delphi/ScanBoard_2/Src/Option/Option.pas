unit Option;

interface

uses
  Classes;

type
  TOption = class(TComponent)
  private
  public
    class function Obj:TOption;
  end;

implementation

var
  MyObj : TOption = nil;

{ TView }

class function TOption.Obj: TOption;
begin
  if MyObj = nil then MyObj := TOption.Create(nil);
  Result := MyObj;
end;

end.

