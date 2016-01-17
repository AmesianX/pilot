unit View;

interface

uses
  ObserverList, ValueList, ViewBase,
  Classes, SysUtils;

type
  TView = class(TViewBase)
  private
  public
    class function Obj:TView;
  end;

implementation

var
  MyObj : TView = nil;

{ TView }

class function TView.Obj: TView;
begin
  if MyObj = nil then MyObj := TView.Create(nil);
  Result := MyObj;
end;

end.

