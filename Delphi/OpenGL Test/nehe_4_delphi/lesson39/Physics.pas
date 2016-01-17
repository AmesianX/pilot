unit Physics;

interface

type
  Vector3D = record                                             // Vektor
    x, y, z: Double;
    end;

  Mass = class                                                  // Objekt
    public
      m: Double;                                                // Hmotnost
      pos: Vector3D;                                            // Pozice v prostoru
      vel: Vector3D;                                            // Rychlosti a sm�r pohybu
      force: Vector3D;                                          // S�la p�sob�c� na objekt
      constructor Create(m: Double);                            // Konstruktor
      procedure applyForce(force: Vector3D);                    // Aplikace sil
      procedure init;                                           // Nulov�n� sil
      procedure simulate(dt: Double);                           // Krok simulace
    end;

  Simulation = class                                            // Simulace
    public
      numOfMasses: integer;                                     // Po�et objekt� v z�sobn�ku
      masses: array of Mass;                                    // Objekty jsou uchov�v�ny v jednorozm�rn�m poli ukazatel� na objekty
      constructor Create(numOfMasses: integer; m: Double);      // Konstruktor vytvo�� objekty s danou hmotnost�
      procedure Release; virtual;                               // Uvoln� dynamickou pam�
      function getMass(index: integer): Mass;                   // Z�sk�n� objektu s ur�it�m indexem
      procedure init; virtual;                                  // Tato metoda zavol� init() metodu ka�d�ho objektu
      procedure solve; virtual;
      procedure simulate(dt: Double); virtual;                  // V�po�et v z�vislosti na �ase
      procedure operate(dt: Double); virtual;                   // Kompletn� simula�n� metoda
    end;

  ConstantVelocity = class(Simulation)                          // Objekt s konstantn� rychlost�
    public
      constructor Create;
    end;

  MotionUnderGravitation = class(Simulation)                    // Pohyb v gravitaci
    public
      gravitation: Vector3D;                                    // Gravita�n� zrychlen�
      constructor Create(gravitation: Vector3D);
      procedure solve; override;                                // Aplikace gravitace
    end;

  MassConnectedWithSpring = class(Simulation)                   // Objekt spojen� pru�inou s bodem
    public
      springConstant: Double;                                   // ��m vy��� bude tato konstanta, t�m tu��� bude pru�ina
      connectionPos: Vector3D;                                  // Bod ke kter�mu bude objekt p�ipojen
      constructor Create(springConstant: Double);
      procedure solve; override;                                // U�it� s�ly pru�iny
    end;

// Funkce pro pr�ci s vektory
function Vektor(x, y, z: Double): Vector3D;
function Add(v1, v2: Vector3D): Vector3D;
function Subtract(v1, v2: Vector3D): Vector3D;
function Multiply(v1: Vector3D; scale: Double): Vector3D;
function Divide(v1: Vector3D; scale: Double): Vector3D;
function Invert(v1: Vector3D): Vector3D;
function Mag(x, y, z: Double): Double;
function Uni(V1: Vector3D): Vector3D;

  

implementation

function Vektor(x, y, z: Double): Vector3D;                // Vrac� vektor
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function Add(v1, v2: Vector3D): Vector3D;                   // Sou�et
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
  Result.z := v1.z + v2.z;
end;

function Subtract(v1, v2: Vector3D): Vector3D;              // Rozd�l
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
  Result.z := v1.z - v2.z;
end;

function Multiply(v1: Vector3D; scale: Double): Vector3D;   // N�soben�
begin
  Result.x := v1.x * scale;
  Result.y := v1.y * scale;
  Result.z := v1.z * scale;
end;

function Divide(v1: Vector3D; scale: Double): Vector3D;     // Pod�l
begin
  Result.x := v1.x / scale;
  Result.y := v1.y / scale;
  Result.z := v1.z / scale;
end;

function Invert(v1: Vector3D): Vector3D;                    // Inverzn� vektor
begin
  Result.x := -v1.x;
  Result.y := -v1.y;
  Result.z := -v1.z;
end;

function Mag(x, y, z: Double): Double;                      // Vrac� velikost vektoru
begin
  Result := sqrt(sqr(x) + sqr(y) + sqr(z));
end;

function Uni(V1: Vector3D): Vector3D;                       // Vrac� jednotkov� vektor
var
  length: Double;
begin
  length := Mag(V1.x,V1.y,V1.z);
  if length = 0 then
    begin
    Result.x := 0.0;
    Result.y := 0.0;
    Result.z := 0.0;
    end
    else
    begin
    Result.x := V1.x / length;
    Result.y := V1.y / length;
    Result.z := V1.z / length;
    end;
end;

{ Mass }

procedure Mass.applyForce(force: Vector3D);
begin
  Self.force := Add(Self.force,force);                                          // Vn�j�� s�la je p�i�tena
end;

constructor Mass.Create(m: Double);                                             // Konstruktor
begin
  Self.m := m;
end;

procedure Mass.init;
begin
  force.x := 0;
  force.y := 0;
  force.z := 0;
end;

procedure Mass.simulate(dt: Double);
begin
  vel := Add(vel,Multiply(Divide(force,m),dt));                                 // Zm�na rychlosti je p�i�tena k aktu�ln� rychlosti
  pos := Add(pos,Multiply(vel,dt));                                             // Zm�na polohy je p�i�tena k aktu�ln� poloze
end;

{ Simulation }

constructor Simulation.Create(numOfMasses: integer; m: Double);                 // Konstruktor vytvo�� objekty s danou hmotnost�
var
  a: integer;
begin
  Self.numOfMasses := numOfMasses;                                              // Inicializace po�tu
  SetLength(masses,numOfMasses);                                                // Alokace dynamick� pam�ti pro pole ukazatel�
  for a := 0 to numOfMasses - 1 do                                              // Projdeme v�echny ukazatele na objekty
    masses[a] := Mass.Create(m);                                                // Vytvo��me objekt a um�st�me ho na m�sto v poli
end;

function Simulation.getMass(index: integer): Mass;                              // Z�sk�n� objektu s ur�it�m indexem
begin
  if (index < 0) or (index >= numOfMasses) then                                 // Pokud index nen� v rozsahu pole
    Result := nil                                                               // Vr�t� NULL
    else
    Result := masses[index];                                                    // Vr�t� objekt s dan�m indexem
end;

procedure Simulation.init;                                                      // Tato metoda zavol� init() metodu ka�d�ho objektu
var
  a: integer;
begin
  for a := 0 to numOfMasses - 1 do                                              // Proch�z� objekty
    masses[a].init;                                                             // Zavol�n� init() dan�ho objektu
end;

procedure Simulation.operate(dt: Double);                                       // Kompletn� simula�n� metoda
begin
  init;                                                                         // Krok 1: vynulov�n� sil
  solve;                                                                        // Krok 2: aplikace sil
  simulate(dt);                                                                 // Krok 3: vypo��t�n� polohy a rychlosti objekt� v z�vislosti na �ase
end;

procedure Simulation.Release;                                                   // Uvoln� dynamickou pam�
var
  a: integer;
begin
  for a := 0 to numOfMasses - 1 do                                              // Sma�e v�echny vytvo�en� objekty
    begin
    masses[a].Free;                                                             // Uvoln� dynamickou pam� objekt�
    masses[a] := nil;                                                           // Nastav� ukazatele na NULL
    end;
  SetLength(masses,0);                                                          // Uvoln� dynamickou pam� ukazatel� na objekty
  masses := nil;                                                                // Nastav� ukazatel na NULL
end;

procedure Simulation.simulate(dt: Double);                                      // V�po�et v z�vislosti na �ase
var
  a: integer;
begin
  for a := 0 to numOfMasses - 1 do                                              // Projdeme v�echny objekty
    masses[a].simulate(dt);                                                     // V�po�et nov� polohy a rychlosti objektu
end;

procedure Simulation.solve;
begin
  // Bez implementace, proto�e nechceme v z�kladn�m z�sobn�ku ��dn� s�ly
  // Ve vylep�en�ch z�sobn�c�ch, bude tato metoda nahrazena, aby na objekty p�sobila n�jak� s�la
end;

{ ConstantVelocity }

constructor ConstantVelocity.Create;
begin
  inherited Create(1,1.0);                                                      // Konstruktor nejd��ve pou�ije konstruktor nad�azen� t��dy, aby vytvo�il objekt o hmotnosti 1 kg
  masses[0].pos := Vektor(0.0,0.0,0.0);                                         // Nastav�me polohu objektu na po��tek
  masses[0].vel := Vektor(1.0,0.0,0.0);                                         // Nastav�me rychlost objektu na (1.0, 0.0, 0.0) m/s
end;

{ MotionUnderGravitation }

constructor MotionUnderGravitation.Create(gravitation: Vector3D);
begin
  inherited Create(1,1.0);                                                      // Konstruktor nejd��ve pou�ije konstruktor nad�azen� t��dy, aby vytvo�il 1 objekt o hmotnosti 1kg
  Self.gravitation := gravitation;                                              // Nastaven� gravitace
  masses[0].pos := Vektor(-10.0,0.0,0.0);                                       // Nastaven� polohy objektu
  masses[0].vel := Vektor(10.0,15.0,0.0);                                       // Nastaven� rychlosti objektu
end;

procedure MotionUnderGravitation.solve;                                         // Aplikace gravitace na v�echny objekty, na kter� m� p�sobit
var
  a: integer;
begin
  inherited;
  for a := 0 to numOfMasses - 1 do                                              // Pou�ijeme gravitaci na v�echny objekty (zat�m m�me jenom jeden, ale to se m��e do budoucna zm�nit)
    masses[a].applyForce(Multiply(gravitation,masses[a].m));                    // S�la gravitace se spo��t� F = m * g
end;

{ MassConnectedWithSpring }

constructor MassConnectedWithSpring.Create(springConstant: Double);
begin
  inherited Create(1,1.0);                                                      // Konstruktor nejd��ve pou�ije konstruktor nad�azen� t��dy, aby vytvo�il 1 objekt o hmotnosti 1kg
  Self.springConstant := springConstant;                                        // Nastaven� tuhosti pru�iny
  connectionPos := Vektor(0.0,-5.0,0.0);                                        // Nastaven� pozice upev�ovac�ho bodu
  masses[0].pos := Add(connectionPos,Vektor(10.0,0.0,0.0));                     // Nastaven� pozice objektu na 10 metr� napravo od bodu, ke kter�mu je uchycen
  masses[0].vel := Vektor(0.0,0.0,0.0);                                         // Nastaven� rychlosti objektu na nulu
end;

procedure MassConnectedWithSpring.solve;                                        // U�it� s�ly pru�iny
var
  a: integer;
  springVector: Vector3D;
begin
  inherited;
  for a := 0 to numOfMasses - 1 do                                              // Pou�ijeme s�lu na v�echny objekty (zat�m m�me jenom jeden, ale to se m��e do budoucna zm�nit)
    begin
    springVector := Subtract(masses[a].pos,connectionPos);                      // Nalezen� vektoru od pozice objektu k �chytu
    masses[0].applyForce(Multiply(Invert(springVector),springConstant));        // Pou�it� s�ly podle uveden�ho vzorce
    end;
end;

end.
