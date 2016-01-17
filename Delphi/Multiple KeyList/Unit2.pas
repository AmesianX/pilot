unit Unit2;

interface

uses
  KeyList64, MultiKeyList64,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Btn1List: TButton;
    BtnMultiList: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Btn1ListClick(Sender: TObject);
    procedure BtnMultiListClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    ArrInt64 : Array of Int64;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

const
  NumCount : Integer = 100000;

procedure TForm2.Btn1ListClick(Sender: TObject);
var
  List : TKeyList64;
  Tick : Cardinal;
  i : Integer;
begin
  List := TKeyList64.Create;
  Tick := GetTickCount;
  for I := 0 to NumCount do begin
    List.CheckDuplicate(ArrInt64[i]);
  end;
  OutputDebugString(PChar(Format('%dms One List : %d', [GetTickCount - Tick, List.Count])));
  List.Free;
end;

procedure TForm2.BtnMultiListClick(Sender: TObject);
var
  List : TMultiKeyList64;
  Tick : Cardinal;
  i : Integer;
begin
  List := TMultiKeyList64.Create;
  Tick := GetTickCount;
  for I := 0 to (NumCount div 1)- 1 do begin
    List.Add(ArrInt64[i]);
  end;
  OutputDebugString(PChar(Format('%dms Multi List : %d', [GetTickCount - Tick, List.Count])));
  List.Free;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Btn1ListClick(nil);
  BtnMultiListClick(nil);
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Randomize;
  SetLength(ArrInt64, NumCount);

  for I := 0 to NumCount - 1 do begin
    ArrInt64[i] :=Random(MaxInt);//mod 5;
  end;
end;

end.
