library asEventTest;

uses
  ComServ,
  asEventTest_TLB in 'asEventTest_TLB.pas',
  axEventTestImpl1 in 'axEventTestImpl1.pas' {axEventTest: TActiveForm} {axEventTest: CoClass};

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
