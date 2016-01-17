unit ViewBase;

interface

uses
  ObserverList, ValueList,
  Classes, SysUtils;

type
  TViewBase = class(TObserverList)
  private
  public
    procedure sp_Terminate(Msg:string);
    procedure sp_TextSocketIsReady;
    procedure sp_UserListChange;
  end;

implementation

{ TViewBase }

procedure TViewBase.sp_Terminate(Msg: string);
begin
  Packet.Clear;
  Packet.Values['Code'] := 'Terminate';
  Packet.Values['Msg'] := Msg;
  BroadCast;
end;

procedure TViewBase.sp_TextSocketIsReady;
begin
  Packet.Clear;
  Packet.Values['Code'] := 'TextSocketIsReady';
  Broadcast;
end;

procedure TViewBase.sp_UserListChange;
begin
  Packet.Clear;
  Packet.Values['Code'] := 'UserListChange';
  Broadcast;
end;

end.

