unit View;

interface

uses
  ObserverList, ValueList, Classes;

type
  TView = class(TObserverList)
  private
  public
    class function Obj:TView;

    procedure sp_Terminate(Msg:string);
  end;

implementation

uses
  Global;

var
  MyObj : TView = nil;

{ TView }

class function TView.Obj: TView;
begin
  if MyObj = nil then MyObj := TView.Create(nil);
  Result := MyObj;
end;

procedure TView.sp_Terminate(Msg:string);
begin
  TGlobal.Obj.Finalize;

  TView.Obj.Packet.Clear;
  TView.Obj.Packet.Values['Code'] := 'Terminate';
  TView.Obj.Packet.Values['Msg'] := Msg;
  TView.Obj.BroadCast;
end;

end.
