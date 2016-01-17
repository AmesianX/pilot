unit Ray;

interface

uses Vector;

type
  TRay = record                                             // Struktura polop��mky
  P, V: TVector;
  end;

function DistR(RP, RV, V: TVector): double;

implementation

function DistR(RP, RV, V: TVector): double;                 // Vzd�lenost vektoru a polop��mky
var
  tmp: TVector;
  lambda: double;
begin
  lambda := Dot(RV,Subtract(V,RP));
  tmp := Add(RP,Multiply(RV,lambda));
  Result := DistV(V,tmp);
end;

end.
