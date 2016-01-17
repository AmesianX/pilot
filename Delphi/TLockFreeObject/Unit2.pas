unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LockFreeObject;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  Ob : TLockFreeObject;
  AData : Pointer;
  I: Integer;
  j: Integer;
  Data2 : Pointer;
  Size : Integer;
begin
  AData := GetMemory(1000);
  ob := TLockFreeObject.Create();
  for I := 0 to 5000 - 1 do begin
    for j := 0 to Random(2) - 1 do begin
      Ob.SetObject(ob, 1000);
    end;

    for j := 0 to Random(100) - 1 do begin
      Ob.GetObject(Data2, Size);
      OutputDebugString(PChar(IntToStr(Size)));
    end;
  end;

  ob.Free;
end;

end.
