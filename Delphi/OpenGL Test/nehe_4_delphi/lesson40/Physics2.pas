unit Physics2;

interface

uses Physics;

type
  Spring = class                                                                // T��da pru�iny
    public
    mass1: Mass;                                                                // ��stice na prvn�m konci pru�iny
    mass2: Mass;                                                                // ��stice na druh�m konci pru�iny
    springConstant: Double;                                                     // Konstanta tuhosti pru�iny
    springLength: Double;                                                       // D�lka, p�i kter� nep�sob� ��dn� s�ly
    frictionConstant: Double;                                                   // Konstanta vnit�n�ho t�en�
    constructor Create(mass1: Mass; mass2: Mass; springConstant, springLength, frictionConstant: Double); // Konstruktor
    procedure solve;                                                            // Aplikov�n� sil na ��stice
    end;

  RopeSimulation = class(Simulation)                                            // T��da simulace lana
    springs: array of Spring;                                                   // Pru�iny spojuj�c� ��stice
    gravitation: Vector3D;                                                      // Gravita�n� zrychlen�
    ropeConnectionPos: Vector3D;                                                // Bod v prostoru; pozice prvn� ��stice pro ovl�d�n� lanem
    ropeConnectionVel: Vector3D;                                                // Rychlost a sm�r po�adovan�ho pohybu
    groundRepulsionConstant: Double;                                            // Konstanta reprezentuj�c�, jak moc zem� odr�� ��stice
    groundFrictionConstant: Double;                                             // Konstanta reprezentuj�c� velikost t�en� ��stic se zem� (posouv�me po zemi)
    groundAbsorptionConstant: Double;                                           // Konstanta reprezentuj�c� velikost absorbce t�en� ��stic se zem� (vertik�ln� kolize)
    groundHeight: Double;                                                       // Pozice roviny zem� na ose y
    airFrictionConstant: Double;                                                // Konstanta odporu vzduchu na ��stice
    constructor Create(numOfMasses: integer;                                    // Konstruktor t��dy // Po�et ��stic
                        m,                                                      // Hmotnost ka�d� ��stice
                        springConstant,                                         // Tuhost pru�iny
                        springLength,                                           // D�lka pru�iny v klidov�m stavu
                        springFrictionConstant: Double;                         // Konstanta vnit�n�ho t�en� pru�iny
                        gravitation: Vector3D;                                  // Gravita�n� zrychlen�
                        airFrictionConstant,                                    // Odpor vzduchu
                        groundRepulsionConstant,                                // Odr�en� ��stic zem�
                        groundFrictionConstant,                                 // T�en� ��stic se zem�
                        groundAbsorptionConstant,                               // Absorbce sil zem�
                        groundHeight: Double);                                  // Pozice zem� na ose y
    procedure solve; override;                                                  // Aplikov�n� sil
    procedure simulate(dt: Double); override;                                   // Simulace lana
    procedure setRopeConnectionVel(ropeConnectionVel: Vector3D);                // Nastaven� rychlosti prvn� ��stice
    procedure release; override;                                                // Uvoln�n� prost�edk�
    end;

implementation

{ Spring }

constructor Spring.Create(mass1, mass2: Mass; springConstant, springLength,     // Konstruktor
  frictionConstant: Double);
begin
  Self.springConstant := springConstant;                                        // Nastaven� �lensk�ch prom�nn�ch
  Self.springLength := springLength;
  Self.frictionConstant := frictionConstant;
  Self.mass1 := mass1;
  Self.mass2 := mass2;
end;

procedure Spring.solve;                                                         // Aplikov�n� sil na ��stice
var
  springVector: Vector3D;
  r: Double;
  force: Vector3D;
begin
  springVector := Subtract(mass1.pos,mass2.pos);                                // Vektor mezi ��sticemi
  r := Mag(springVector.x,springVector.y,springVector.z);                       // Vzd�lenost ��stic
  force := Vektor(0,0,0);                                                       // Pomocn� vektor s�ly
  if r <> 0 then                                                                // Proti d�len� nulou
    force := Add(force,Multiply(Divide(springVector,r),(r - springLength) * (- springConstant))); // V�po�et s�ly podle vzorce
  force := Add(force,Multiply(Invert(Subtract(mass1.vel,mass2.vel)),frictionConstant)); // Zmen�en� s�ly o t�en�
  mass1.applyForce(force);                                                      // Aplikov�n� s�ly na ��stici 1
  mass2.applyForce(Invert(force));                                              // Aplikov�n� opa�n� s�ly na ��stici 2
end;

{ RopeSimulation }

constructor RopeSimulation.Create(numOfMasses: integer; m, springConstant,      // Konstruktor t��dy
  springLength, springFrictionConstant: Double; gravitation: Vector3D;
  airFrictionConstant, groundRepulsionConstant, groundFrictionConstant,
  groundAbsorptionConstant, groundHeight: Double);
var
  a: integer;
begin
  inherited Create(numOfMasses,m);                                              // Inicializace p�edka t��dy
  Self.gravitation := gravitation;
  Self.airFrictionConstant := airFrictionConstant;
  Self.groundFrictionConstant := groundFrictionConstant;
  Self.groundRepulsionConstant := groundRepulsionConstant;
  Self.groundAbsorptionConstant := groundAbsorptionConstant;
  Self.groundHeight := groundHeight;
  for a := 0 to numOfMasses - 1 do                                              // Nastaven� po��te�n� pozice ��stic
    begin
    masses[a].pos.x := a * springLength;                                        // Offsety jednotliv�ch ��stic
    masses[a].pos.y := 0;                                                       // Rovnob�n� se zem�
    masses[a].pos.z := 0;                                                       // Rovnob�n� s obrazovkou
    end;
  SetLength(springs,numOfMasses);                                               // Alokace pam�ti pro ukazatele na pru�iny
  for a := 0 to numOfMasses - 2 do                                              // Vytvo�en� jednotliv�ch pru�in
    springs[a] := Spring.Create(masses[a],masses[a+1],springConstant,springLength,springFrictionConstant);  // Dv� ��stice na pru�inu
end;

procedure RopeSimulation.release;
var
  a: integer;
begin
  inherited;
  for a := 0 to numOfMasses - 1 do
    begin
    springs[a].Free;
    springs[a] := nil;
    end;
  SetLength(springs,0);
  springs := nil;
end;

procedure RopeSimulation.setRopeConnectionVel(ropeConnectionVel: Vector3D);     // Nastaven� rychlosti prvn� ��stice
begin
  Self.ropeConnectionVel := ropeConnectionVel;                                  // P�i�azen� rychlost�
end;

procedure RopeSimulation.simulate(dt: Double);                                  // Simulace lana
begin
  inherited;                                                                    // Metoda p�edka
  ropeConnectionPos := Add(ropeConnectionPos,Multiply(ropeConnectionVel,dt));   // Zv�t�en� pozice o rychlost
  if ropeConnectionPos.y < groundHeight then                                    // Dostala se ��stice pod zem?
    begin
    ropeConnectionPos.y := groundHeight;                                        // P�esunut� na �rove� zem�
    ropeConnectionVel.y := 0;                                                   // Nulov�n� rychlosti na ose y
    end;
  masses[0].pos := ropeConnectionPos;                                           // Pozice prvn� ��stice
  masses[0].vel := ropeConnectionVel;                                           // Rychlost prvn� ��stice
end;

procedure RopeSimulation.solve;                                                 // Aplikov�n� sil
var
  a: integer;
  v: Vector3D;
  force: Vector3D;
begin
  inherited;
  for a := 0 to numOfMasses - 2 do                                              // Proch�z� pru�iny
    springs[a].solve;                                                           // Aplikov�n� sil na pru�inu
  for a := 0 to numOfMasses - 1 do                                              // Proch�z� ��stice
    begin
    masses[a].applyForce(Multiply(gravitation,masses[a].m));                    // Gravitace
    masses[a].applyForce(Multiply(Invert(masses[a].vel),airFrictionConstant));  // Odpor vzduchu
    if masses[a].pos.y < groundHeight then                                      // Kolize se zem�
      begin
      v := masses[a].vel;                                                       // Grabov�n� rychlosti
      v.y := 0;                                                                 // Vynech�z� rychlosti na ose y
      masses[a].applyForce(Multiply(Invert(v),groundFrictionConstant));         // T�ec� s�la zem�
      v := masses[a].vel;                                                       // Grabov�n� rychlosti
      v.x := 0;                                                                 // Zanedb�n� rychlosti na os�ch x a z
      v.z := 0;
      if v.y < 0 then                                                           // Pouze p�i kolizi sm�rem k zemi
        masses[a].applyForce(Multiply(Invert(v),groundAbsorptionConstant));     //Absorb�n� s�la
      force := Multiply(Vektor(0,groundRepulsionConstant,0),(groundHeight - masses[a].pos.y));  // S�la odrazu
      masses[a].applyForce(force);                                              // Aplikov�n� s�ly odrazu
      end;
    end;
end;

end.
