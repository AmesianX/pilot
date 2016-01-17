unit RandNo;

interface

uses
  _dmMain;

type
  TRandNo = class(TARandNo)
  private
  public
    procedure Execute; override;
  end;

implementation

{ TRandNo }

procedure TRandNo.Execute;
var
  Loop : integer;
begin
  SetLength(Numbers, 0);
  SetLength(Numbers, 6);

  for Loop := Low(Numbers) to High(Numbers) do
    Numbers[Loop]:= Round(Random(45)) + 1;

  Self.on_Execute;
end;

end.
