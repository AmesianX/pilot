program lesson30a;

{   k�d pro Delphi 7}

uses
  Windows, 
  Messages,
  OpenGL,
  sysutils,
  GLaux,
  Vector in 'Vector.pas',
  mmsystem,
  Ray in 'Ray.pas';

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glDeleteTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

type
  Plane = record                                            // Struktura roviny
    _Position: TVector;
    _Normal: TVector;
    end;

  Cylinder = record                                         // Struktura v�lce
    _Position: TVector;
    _Axis: TVector;
    _Radius: Double;
    end;

  Explosion = record                                        // Struktura exploze
    _Position: TVector;
    _Alpha: GLfloat;
    _Scale: GLfloat;
    end;

const
  spec: array [0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);     // Bodov� sv�tlo
  posl: array [0..3] of GLfloat = (0, 400, 0, 1);           // Pozice sv�tla
  amb: array [0..3] of GLfloat = (0.2, 0.2, 0.2, 1.0);      // Okoln� sv�tlo glob�ln�
  amb2: array [0..3] of GLfloat = (0.3, 0.3, 0.3, 1.0);     // Okoln� sv�tlo bodov�ho sv�tla
  ZERO = EPSILON;

var
  h_Rc: HGLRC;		                                          // Trval� Rendering Context
  h_Dc: HDC;                                                // Priv�tn� GDI Device Context
  h_Wnd: HWND;                                              // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	                            // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;                                      // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;                                   // Ponese informaci o tom, zda je program ve fullscreenu
  dir: TVector = (x:0; y:0; z:-10);                         // Sm�r kamery
  pos: TVector = (x:0; y:-50; z:1000);                      // Pozice kamery
  camera_rotation: GLfloat = 0;                             // Rotace sc�ny na ose y
  veloc: TVector = (x:0.5; y:-0.1; z:0.5);                  // Po��te�n� rychlost koul�
  accel: TVector = (x:0; y:-0.05; z:0);                     // Gravita�n� zrychlen� aplikovan� na koule
  ArrayVel: array [0..9] of TVector;                        // Rychlost koul�
  ArrayPos: array [0..9] of TVector;                        // Pozice koul�
  OldPos: array [0..9] of TVector;                          // Star� pozice koul�
  NrOfBalls: integer;                                       // Po�et koul�
  Time: Double = 0.6;                                       // �asov� krok simulace
  hook_toball1: boolean = false;                            // Sledovat kamerou kouli?
  sounds: boolean = true;                                   // Zvukov� efekty on/off
  pl1, pl2, pl3, pl4, pl5: Plane;                           // P�t rovin m�stnosti (bez stropu)
  cyl1, cyl2, cyl3: Cylinder;                               // T�i v�lce
  ExplosionArray: array [0..19] of Explosion;               // Dvacet exploz�
  texture: array [0..3] of GLuint;                          // �ty�i textury
  dlist: GLuint;                                            // Display list v�buchu
  cylinder_obj: GLUquadricObj;                              // Quadratic pro kreslen� koul� a v�lc�

function LoadBMP(FileName: pchar):PTAUX_RGBImageRec;        // Nahraje bitmapu
begin
  if Filename = '' then                                     // Byla p�ed�na cesta k souboru?
    begin
    Result := nil;                                          // Pokud ne, konec
    exit;
    end;
  if not FileExists(Filename) then                          // Existuje soubor?
    begin
    Result := nil;                                          // Pokud ne, konec
    exit;
    end;
  Result := auxDIBImageLoadA(FileName);                     // Na�te bitmapu a vr�t� na ni ukazatel
end;

function LoadGLTextures: Bool;                                                  // Loading bitmapy a konverze na texturu
var TextureImage: array [0..3] of PTAUX_RGBImageRec;                            // Ukl�d� bitmapy
    Status: Bool;                                                               // Indikuje chyby
    i: integer;                                                                 // Cyklus
begin
  Status := false;
  ZeroMemory(@TextureImage,sizeof(TextureImage));                               // Vynuluje pam�
  TextureImage[0] := LoadBMP('Data/Marble.bmp');                                // Nahraje bitmapy
  TextureImage[1] := LoadBMP('Data/Spark.bmp');
  TextureImage[2] := LoadBMP('Data/Boden.bmp');
  TextureImage[3] := LoadBMP('Data/Wand.bmp');
  if Assigned(TextureImage[0]) and Assigned(TextureImage[1])
    and Assigned(TextureImage[2]) and Assigned(TextureImage[3]) then            // V�e je bez probl�m�?
    begin
    Status := true;                                                             // V�e je bez probl�m�
    glGenTextures(4,Texture[0]);                                                // Generuje textury
    for i := 0 to 3 do
      begin
      glBindTexture(GL_TEXTURE_2D,texture[i]);                                  // Typick� vytv��en� textury z bitmapy
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);           // Filtrov�n� p�i zv�t�en�
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);           // Filtrov�n� p�i zmen�en�
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);               // Opakov�n� textury ve sm�ru S
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);               // Opakov�n� textury ve sm�ru T
      glTexImage2D(GL_TEXTURE_2D,0,3,TextureImage[i].sizeX,TextureImage[i].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[i].data);    // Vlastn� vytv��en� textury
      end;
    end;
  Result := Status;                                                             // Ozn�m� p��padn� chyby
end;

procedure InitVars;                                     // Inicializace prom�nn�ch
var
  i: integer;                                           // Cyklus
begin
  pl1._Position := Vektor(0,-300,0);                    // 1. rovina
  pl1._Normal := Vektor(0,1,0);
  pl2._Position := Vektor(300,0,0);                     // 2. rovina
  pl2._Normal := Vektor(-1,0,0);
  pl3._Position := Vektor(-300,0,0);                    // 3. rovina
  pl3._Normal := Vektor(1,0,0);
  pl4._Position := Vektor(0,0,300);                     // 4. rovina
  pl4._Normal := Vektor(0,0,-1);
  pl5._Position := Vektor(0,0,-300);                    // 5. rovina
  pl5._Normal := Vektor(0,0,1);
  cyl1._Position := Vektor(0,0,0);                      // 1. v�lec
	cyl1._Axis := Vektor(0,1,0);
	cyl1._Radius := 60+20;
	cyl2._Position := Vektor(200,-300,0);                 // 2. v�lec
	cyl2._Axis := Vektor(0,0,1);
	cyl2._Radius := 60+20;
	cyl3._Position := Vektor(-200,0,0);                   // 3. v�lec
	cyl3._Axis := Vektor(0,1,1);
  cyl3._Axis := Uni(cyl3._Axis);
	cyl3._Radius := 30+20; 
  cylinder_obj := gluNewQuadric;                        // Nov� kvadratik objekt
  gluQuadricTexture(cylinder_obj,GL_TRUE);
	NrOfBalls := 10;                                      // Po�et m���
	ArrayVel[0] := veloc;                                 // 1. m��
	ArrayPos[0] := Vektor(199,180,10);
	ExplosionArray[0]._Alpha := 0;
	ExplosionArray[0]._Scale := 1;
	ArrayVel[1] := veloc;                                 // 2. m��
	ArrayPos[1] := Vektor(0,150,100);
	ExplosionArray[1]._Alpha := 0;
	ExplosionArray[1]._Scale := 1;
	ArrayVel[2] := veloc;                                 // 3. m��
	ArrayPos[2] := Vektor(-100,180,-100);
	ExplosionArray[2]._Alpha := 0;
	ExplosionArray[2]._Scale := 1;
  for i := 3 to 9 do                                    // 4. - 10. m��
    begin
    ArrayVel[i] := veloc;
	  ArrayPos[i] := Vektor(-500+i*75,300,-500+i*50);
		ExplosionArray[i]._Alpha := 0;
	  ExplosionArray[i]._Scale := 1;
    end;
	for i := 10 to 19 do
    begin
    ExplosionArray[i]._Alpha := 0;
	  ExplosionArray[i]._Scale := 1;
    end;
end;

function TestIntersionPlane(plane: Plane; position, direction: TVector; var lamda: Double; var pNormal: TVector): boolean;
var
  DotProduct, l2: double;                                                       // Skal�rn� sou�in vektor�, Ur�uje kolizn� bod
begin
  DotProduct := Dot(direction,plane._Normal);                                   // Skal�rn� sou�in vektor�
  if ((DotProduct < ZERO) and (DotProduct > -ZERO)) then                        // Je polop��mka rovnob�n� s rovinou?
    begin
		Result := false;                                                            // Bez pr�se��ku
    exit;
    end;
  l2 := Dot(plane._Normal,Subtract(plane._Position,position)) / DotProduct;     // Dosazen� do vzorce
  if l2 < -ZERO then                                                            // Sm��uje polop��mka od roviny?
    begin
		Result := false;                                                            // Bez pr�se��ku
    exit;
    end;
  pNormal := plane._Normal;                                                     // Norm�la roviny
	lamda := l2;                                                                  // Kolizn� bod
  Result := true;                                                               // Pr�se��k existuje
end;

function TestIntersionCylinder(cylinder: Cylinder; position, direction: TVector; var lamda: Double; var pNormal, newposition: TVector): boolean;
var
	RC: TVector;
	d,t,s: Double;
	n,O,HB: TVector;
	ln,vstup,vystup: Double;
begin
  RC := Subtract(position,cylinder._Position);
  n := Cross(direction,cylinder._Axis);
  ln := Mag(n.x,n.y,n.z);
	if (ln < ZERO) and (ln > -ZERO) then
    begin
    Result := false;
    exit;
    end;
	n := Uni(n);
	d := Abs(Dot(RC,n));
  if d <= cylinder._Radius then
    begin
    O := Cross(RC,cylinder._Axis);
		t := - (Dot(O,n) / ln);
		O := Cross(n,cylinder._Axis);
		O := Uni(O);
		s := Abs(sqrt(cylinder._Radius*cylinder._Radius - d*d) / Dot(direction,O));
		vstup := t - s;
		vystup := t + s;
		if vstup < -ZERO then
			if vystup < -ZERO then
        begin
        Result := false;
        exit;
        end
			  else lamda := vystup
		  else
      if vystup < -ZERO then
        lamda := vstup
		    else
		    if vstup < vystup then
          lamda := vstup
		      else lamda := vystup;
    newposition := Add(position,Multiply(direction,lamda));
		HB := Subtract(newposition,cylinder._Position);
		pNormal := Subtract(HB,Multiply(cylinder._Axis,Dot(HB,cylinder._Axis)));
		pNormal := Uni(pNormal);
		Result := true;
    exit;
	  end;
  Result := false;
end;

function FindBallCol(var point: TVector; var TimePoint: Double; Time2: Double; var BallNr1, BallNr2: Integer): boolean;
var
  RelativeV: TVector;                                                           // Relativn� rychlost mezi koulemi
  rays: TRay;                                                                   // Polop��mka
  MyTime, AddTime, Timedummy: double;                                           // Hled�n� p�esn� pozice n�razu
  posi: TVector;                                                                // Pozice na polop��mce
  i, j: integer;                                                                // Cykly
begin
  AddTime := Time2 / 150.0;                                                     // Rozkouskuje �asov� �sek na 150 ��st�
  Timedummy := 10000;                                                           // �as n�razu
	// Test v�ech koul� proti v�em ostatn�m po 150 kroc�ch
	for i := 0 to NrOfBalls - 2 do                                                // V�echny koule
	  for j := i + 1 to NrOfBalls - 1 do                                          // V�echny zb�vaj�c� koule
      begin
		  RelativeV := Subtract(ArrayVel[i],ArrayVel[j]);                           // Relativn� rychlost mezi koulemi
			rays.P := OldPos[i];                                                      // Polop��mka
      rays.V := Uni(RelativeV);
			MyTime := 0.0;                                                            // Inicializace p�ed vstupem do cyklu
			if DistR(rays.P,rays.V,OldPos[j]) > 40 then                               // Je vzd�lenost v�t�� ne� 2 polom�ry?
        begin
        continue;                                                               // Dal��
        end;
			while MyTime < Time2 do                                                   // P�esn� bod n�razu
        begin
			  MyTime := MyTime + AddTime;                                             // Zv�t�� �as
			  posi := Add(OldPos[i],Multiply(RelativeV,MyTime));                      //P�esun na dal�� bod (pohyb na polop��mce)
			  if DistV(posi,OldPos[j]) <= 40 then                                     // N�raz
          begin
          point := posi;                                                        // Bod n�razu
          if Timedummy > (MyTime - AddTime) then Timedummy := MyTime - AddTime; // Bli��� n�raz, ne� kter� jsme u� na�li (v �ase)? P�i�adit �as n�razu
          BallNr1 := i;                                                         // Ozna�en� koul�, kter� narazily
          BallNr2 := j;
          break;                                                                // Ukon�� vnit�n� cyklus
          end;
			  end;
      end;
	if Timedummy <> 10000 then                                                    // Na�li jsme kolizi?
    begin
    TimePoint := Timedummy;                                                     // �as n�razu
	  Result := true;                                                             // �sp�ch
    exit;
    end;
	Result := false;                                                              // Bez kolize
end;

procedure Idle;                                                                 // Simula�n� logika - kolize
var
  rt, rt2, rt4, lamda: double;                                                  // Deklarace prom�nn�ch
  norm, uveloc: TVector;
  normal, point, ltime: TVector;
  RestTime, BallTime: double;
  Pos2: TVector;
  BallNr, dummy, BallColNr1, BallColNr2: integer;
  Nc: TVector;
  i, j: integer;
  pb1,pb2,xaxis,U1x,U1y,U2x,U2y,V1x,V1y,V2x,V2y: TVector;
  a,b: double;
begin
  BallNr := 0;
  dummy := 0;
  if not hook_toball1 then                                                      // Pokud kamera nesleduje kouli
    begin
    camera_rotation := camera_rotation + 0.1;                                   // Pooto�en� sc�ny
    if camera_rotation > 360 then camera_rotation := 0;                         // O�et�en� p�ete�en�
    end;
  RestTime := Time;
  lamda := 1000;
  // V�po�et rychlost� v�ech koul� pro n�sleduj�c� �asov� �sek (Eulerovy rovnice)
  for j := 0 to NrOfBalls - 1 do
    ArrayVel[j] := Add(ArrayVel[j],Multiply(accel,RestTime));
  while RestTime > ZERO do                                                      // Dokud neskon�il �asov� �sek
    begin
    lamda := 10000;                                                             // Inicializace na velmi vysokou hodnotu
    // Kolize v�ech koul� s rovinami a v�lci
    for i := 0 to NrOfBalls - 1 do                                              // V�echny koule
      begin
      OldPos[i] := ArrayPos[i];                                                 // V�po�et nov� pozice a vzd�lenosti
      uveloc := Uni(ArrayVel[i]);
      ArrayPos[i] := Add(ArrayPos[i],Multiply(ArrayVel[i],RestTime));
      rt2 := DistV(OldPos[i],ArrayPos[i]);
      // Kolize koule s rovinou
      if TestIntersionPlane(pl1,OldPos[i],uveloc,rt,norm) then
        begin
        rt4 := rt * RestTime / rt2;                                             // �as n�razu
        if rt4 <= lamda then                                                    // Pokud je men�� ne� n�kter� z d��ve nalezen�ch nahradit ho
          if rt4 <= (RestTime + ZERO) then
            if not ((rt <= ZERO) and (Dot(uveloc,norm) > ZERO)) then
              begin
              normal := norm;
              point := Add(OldPos[i],Multiply(uveloc,rt));
              lamda := rt4;
              BallNr := i;
              end;
        end;
      if TestIntersionPlane(pl2,OldPos[i],uveloc,rt,norm) then
        begin
        rt4 := rt * RestTime / rt2;                                             // �as n�razu
        if rt4 <= lamda then                                                    // Pokud je men�� ne� n�kter� z d��ve nalezen�ch nahradit ho
          if rt4 <= (RestTime + ZERO) then
            if not ((rt <= ZERO) and (Dot(uveloc,norm) > ZERO)) then
              begin
              normal := norm;
              point := Add(OldPos[i],Multiply(uveloc,rt));
              lamda := rt4;
              BallNr := i;
              dummy := 1;
              end;
        end;
      if TestIntersionPlane(pl3,OldPos[i],uveloc,rt,norm) then
        begin
        rt4 := rt * RestTime / rt2;                                             // �as n�razu
        if rt4 <= lamda then                                                    // Pokud je men�� ne� n�kter� z d��ve nalezen�ch nahradit ho
          if rt4 <= (RestTime + ZERO) then
            if not ((rt <= ZERO) and (Dot(uveloc,norm) > ZERO)) then
              begin
              normal := norm;
              point := Add(OldPos[i],Multiply(uveloc,rt));
              lamda := rt4;
              BallNr := i;
              end;
        end;
      if TestIntersionPlane(pl4,OldPos[i],uveloc,rt,norm) then
        begin
        rt4 := rt * RestTime / rt2;                                             // �as n�razu
        if rt4 <= lamda then                                                    // Pokud je men�� ne� n�kter� z d��ve nalezen�ch nahradit ho
          if rt4 <= (RestTime + ZERO) then
            if not ((rt <= ZERO) and (Dot(uveloc,norm) > ZERO)) then
              begin
              normal := norm;
              point := Add(OldPos[i],Multiply(uveloc,rt));
              lamda := rt4;
              BallNr := i;
              end;
        end;
      if TestIntersionPlane(pl5,OldPos[i],uveloc,rt,norm) then
        begin
        rt4 := rt * RestTime / rt2;                                             // �as n�razu
        if rt4 <= lamda then                                                    // Pokud je men�� ne� n�kter� z d��ve nalezen�ch nahradit ho
          if rt4 <= (RestTime + ZERO) then
            if not ((rt <= ZERO) and (Dot(uveloc,norm) > ZERO)) then
              begin
              normal := norm;
              point := Add(OldPos[i],Multiply(uveloc,rt));
              lamda := rt4;
              BallNr := i;
              end;
        end;
      // Kolize koule s v�lcem
      if TestIntersionCylinder(cyl1,OldPos[i],uveloc,rt,norm,Nc) then
        begin
        rt4 := rt * RestTime / rt2;                                             // �as n�razu
        if rt4 <= lamda then                                                    // Pokud je men�� ne� n�kter� z d��ve nalezen�ch nahradit ho
          if rt4 <= (RestTime + ZERO) then
            if not ((rt <= ZERO) and (Dot(uveloc,norm) > ZERO)) then
              begin
              normal := norm;
              point := Nc;
              lamda := rt4;
              BallNr := i;
              end;
        end;
      if TestIntersionCylinder(cyl2,OldPos[i],uveloc,rt,norm,Nc) then
        begin
        rt4 := rt * RestTime / rt2;                                             // �as n�razu
        if rt4 <= lamda then                                                    // Pokud je men�� ne� n�kter� z d��ve nalezen�ch nahradit ho
          if rt4 <= (RestTime + ZERO) then
            if not ((rt <= ZERO) and (Dot(uveloc,norm) > ZERO)) then
              begin
              normal := norm;
              point := Nc;
              lamda := rt4;
              BallNr := i;
              end;
        end;
      if TestIntersionCylinder(cyl3,OldPos[i],uveloc,rt,norm,Nc) then
        begin
        rt4 := rt * RestTime / rt2;                                             // �as n�razu
        if rt4 <= lamda then                                                    // Pokud je men�� ne� n�kter� z d��ve nalezen�ch nahradit ho
          if rt4 <= (RestTime + ZERO) then
            if not ((rt <= ZERO) and (Dot(uveloc,norm) > ZERO)) then
              begin
              normal := norm;
              point := Nc;
              lamda := rt4;
              BallNr := i;
              end;
        end;
      end;
    // Kolize mezi koulemi
    if FindBallCol(Pos2,BallTime,RestTime,BallColNr1,BallColNr2) then
      begin
      if sounds then PlaySound('Data/Explode.wav',0,SND_FILENAME or SND_ASYNC); // Jsou zapnut� zvuky?
      if (lamda = 10000) or (lamda > BallTime) then
        begin
        RestTime := RestTime - BallTime;
        pb1 := Add(OldPos[BallColNr1],Multiply(ArrayVel[BallColNr1],BallTime)); // Nalezen� pozice koule 1
        pb2 := Add(OldPos[BallColNr2],Multiply(ArrayVel[BallColNr2],BallTime)); // Nalezen� pozice koule 2
        xaxis := Uni(Subtract(pb2,pb1));                                        // Nalezen� X_Axis
        a := Dot(xaxis,ArrayVel[BallColNr1]);                                   // Nalezen� projekce
        U1x := Multiply(xaxis,a);                                               // Nalezen� pr�m�t� vektor�
        U1y := Subtract(ArrayVel[BallColNr1],U1x);
        xaxis := Uni(Subtract(pb1,pb2));                                        // Nalezen� X_Axis
        b := Dot(xaxis,ArrayVel[BallColNr2]);                                   // Nalezen� projekce
        U2x := Multiply(xaxis,b);                                               // Nalezen� pr�m�t� vektor�
        U2y := Subtract(ArrayVel[BallColNr2],U2x);
        V1x := U2x;                 // Nalezen� nov�ch rychlost�
        V2x := U1x;
        V1y := U1y;
        V2y := U2y;
        for j := 0 to NrOfBalls - 1 do                                          // Aktualizace pozic v�ech koul�
          ArrayPos[j] := Add(OldPos[j],Multiply(ArrayVel[j],BallTime));
        ArrayVel[BallColNr1] := Add(V1x,V1y);                                   // Nastaven� pr�v� vypo��tan�ch vektor� koul�m, kter� do sebe narazily
        ArrayVel[BallColNr2] := Add(V2x,V2y);
        // Aktualizace pole exploz�
        for j := 0 to 19 do
          if ExplosionArray[j]._Alpha <= 0 then                                 // Hled� voln� m�sto
            begin
            ExplosionArray[j]._Alpha := 1;                                      // Nepr�hledn�
            ExplosionArray[j]._Position := ArrayPos[BallColNr1];                // Pozice
            ExplosionArray[j]._Scale := 1;                                      // M���tko
            break;
            end;
        continue;
        end;
      end;
    // Konec test� koliz�
    // Pokud se pro�el cel� �asov� �sek a byly vypo�teny reakce koul�, kter� narazily
    if lamda <> 10000 then
      begin
      RestTime := RestTime - lamda;                                             // Ode�ten� �asu kolize od �asov�ho �seku
      for j := 0 to NrOfBalls - 1 do
        ArrayPos[j] := Add(OldPos[j],Multiply(ArrayVel[j],lamda));
      rt2 := Mag(ArrayVel[BallNr].x,ArrayVel[BallNr].y,ArrayVel[BallNr].z);
      ArrayVel[BallNr] := Uni(ArrayVel[BallNr]);
      ArrayVel[BallNr] := Uni(Add(Multiply(normal,2*Dot(normal,Invert(ArrayVel[BallNr]))),ArrayVel[BallNr]))  ;
      ArrayVel[BallNr] := Multiply(ArrayVel[BallNr],rt2);
      // Aktualizace pole exploz�
      for j := 0 to 19 do
        if ExplosionArray[j]._Alpha <= 0 then                                   // Hled� voln� m�sto
          begin
          ExplosionArray[j]._Alpha := 1;                                        // Nepr�hledn�
          ExplosionArray[j]._Position := point;                                 // Pozice
          ExplosionArray[j]._Scale := 1;                                        // M���tko
          break;                                                                // Ukon�it prohled�v�n�
          end;
      end
      else
      RestTime := 0;                                                            // Ukon�en� hlavn�ho cyklu a vlastn� i funkce
    end;
end;

procedure ProcessKeys;                                                          // Obsluha stisku kl�ves
begin
  if keys[VK_UP] then pos := Add(pos,Vektor(0,0,-10));
  if keys[VK_DOWN] then pos := Add(pos,Vektor(0,0,10));
  if keys[VK_LEFT] then camera_rotation := camera_rotation + 10;
  if keys[VK_RIGHT] then camera_rotation := camera_rotation - 10;
  if keys[VK_ADD] then
    begin
    Time := Time + 0.1;
    keys[VK_ADD] := false;
    end;
  if keys[VK_SUBTRACT] then
    begin
    Time := Time - 0.1;
    keys[VK_SUBTRACT] := false;
    end;
  if keys[VK_F3] then
    begin
    sounds := not sounds;
    keys[VK_F3] := false;
    end;
  if keys[VK_F2] then
    begin
    hook_toball1 := not hook_toball1;
    camera_rotation := 0;
    keys[VK_F2] := false;
    end;
end;

procedure ReSizeGLScene(Width: GLsizei; Height: GLsizei); // Zm�na velikosti a inicializace OpenGL okna
begin
  if (Height=0) then		                                  // Zabezpe�en� proti d�len� nulou
     Height:=1;                                           // Nastav� v��ku na jedna
  glViewport(0, 0, Width, Height);                        // Resetuje aktu�ln� nastaven�
  glMatrixMode(GL_PROJECTION);                            // Zvol� projek�n� matici
  glLoadIdentity();                                       // Reset matice
  gluPerspective(50.0,Width/Height,10.0,1700.0);          // V�po�et perspektivy
  glMatrixMode(GL_MODELVIEW);                             // Zvol� matici Modelview
  glLoadIdentity;                                         // Reset matice
end;


function InitGL:bool;	                              // V�echno nastaven� OpenGL
var
  df: GLfloat;
begin
  df := 100.0;
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glEnable(GL_DEPTH_TEST);			                    // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LEQUAL);				                    // Typ hloubkov�ho testov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST); // Nejlep�� perspektivn� korekce
  glClearColor(0.0, 0.0, 0.0, 0.0);	  	            // �ern� pozad�
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glEnable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);
  glMaterialfv(GL_FRONT,GL_SPECULAR,@spec);
	glMaterialfv(GL_FRONT,GL_SHININESS,@df);
  glEnable(GL_LIGHTING);
	glLightfv(GL_LIGHT0,GL_POSITION,@posl);
	glLightfv(GL_LIGHT0,GL_AMBIENT,@amb2);
	glEnable(GL_LIGHT0);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT,@amb);
	glEnable(GL_COLOR_MATERIAL);
	glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glEnable(GL_TEXTURE_2D);                          // Zapne mapov�n� textur
  if not LoadGLTextures then                        // Nahraje texturu
    begin
    Result := false;
    exit;
    end;
  dlist := glGenLists(1);
  glNewList(dlist,GL_COMPILE);
    glBegin(GL_QUADS);
	    glRotatef(-45,0,1,0);
	    glNormal3f(0,0,1);
	    glTexCoord2f(0.0, 0.0); glVertex3f(-50,-40,0);
	    glTexCoord2f(0.0, 1.0); glVertex3f(50,-40,0);
	    glTexCoord2f(1.0, 1.0); glVertex3f(50,40,0);
	    glTexCoord2f(1.0, 0.0); glVertex3f(-50,40,0);
      glNormal3f(0,0,-1);
	    glTexCoord2f(0.0, 0.0); glVertex3f(-50,40,0);
	    glTexCoord2f(0.0, 1.0); glVertex3f(50,40,0);
	    glTexCoord2f(1.0, 1.0); glVertex3f(50,-40,0);
	    glTexCoord2f(1.0, 0.0); glVertex3f(-50,-40,0);
	    glNormal3f(1,0,0);
	    glTexCoord2f(0.0, 0.0); glVertex3f(0,-40,50);
	    glTexCoord2f(0.0, 1.0); glVertex3f(0,-40,-50);
	    glTexCoord2f(1.0, 1.0); glVertex3f(0,40,-50);
	    glTexCoord2f(1.0, 0.0); glVertex3f(0,40,50);
      glNormal3f(-1,0,0);
	    glTexCoord2f(0.0, 0.0); glVertex3f(0,40,50);
	    glTexCoord2f(0.0, 1.0); glVertex3f(0,40,-50);
	    glTexCoord2f(1.0, 1.0); glVertex3f(0,-40,-50);
	    glTexCoord2f(1.0, 0.0); glVertex3f(0,-40,50);
	  glEnd();
  glEndList();
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;


function DrawGLScene():bool;                                                    // Vykreslov�n�
var
  i: integer;
  unit_followvector: TVector;
begin
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();	                                                            // Reset matice
  if hook_toball1 then                                                          // Sledov�n� koule?
	  begin
    unit_followvector := ArrayVel[0];
		unit_followvector := Uni(unit_followvector);
 		gluLookAt(ArrayPos[0].X+250,ArrayPos[0].Y+250,ArrayPos[0].Z,ArrayPos[0].X+ArrayVel[0].X,ArrayPos[0].Y+ArrayVel[0].Y,ArrayPos[0].Z+ArrayVel[0].Z,0.0,1.0,0.0);
    end
	  else
	  gluLookAt(pos.X,pos.Y,pos.Z,pos.X+dir.X,pos.Y+dir.Y,pos.Z+dir.Z,0.0,1.0,0.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                          // Sma�e obrazovku a hloubkov� buffer
  glRotatef(camera_rotation,0.0,1.0,0.0);                                       // Rotace
  for i := 0 to NrOfBalls - 1 do                                                // Nastaven� barev pro koule
    begin
    case i of
      1: glColor3f(1.0,1.0,1.0);
      2: glColor3f(1.0,1.0,0.0);
      3: glColor3f(0.0,1.0,1.0);
      4: glColor3f(0.0,1.0,0.0);
      5: glColor3f(0.0,0.0,1.0);
      6: glColor3f(0.65,0.2,0.3);
      7: glColor3f(1.0,0.0,1.0);
      8: glColor3f(0.0,0.7,0.4);
      else
      glColor3f(1.0,0.0,0.0);
      end;
    glPushMatrix;
    glTranslated(ArrayPos[i].X,ArrayPos[i].Y,ArrayPos[i].Z);                    // Vykreslen� koule
    gluSphere(cylinder_obj,20,20,20);
    glPopMatrix;
    end;
  glEnable(GL_TEXTURE_2D);                                                      // Zapnut� textur
	// Otexturovan� st�ny
	glBindTexture(GL_TEXTURE_2D, texture[3]);
	glColor3f(1, 1, 1);
	glBegin(GL_QUADS);
	glTexCoord2f(1.0, 0.0); glVertex3f(320,320,320);
	glTexCoord2f(1.0, 1.0); glVertex3f(320,-320,320);
  glTexCoord2f(0.0, 1.0); glVertex3f(-320,-320,320);
	glTexCoord2f(0.0, 0.0); glVertex3f(-320,320,320);
	glTexCoord2f(1.0, 0.0); glVertex3f(-320,320,-320);
	glTexCoord2f(1.0, 1.0); glVertex3f(-320,-320,-320);
  glTexCoord2f(0.0, 1.0); glVertex3f(320,-320,-320);
	glTexCoord2f(0.0, 0.0); glVertex3f(320,320,-320);
	glTexCoord2f(1.0, 0.0); glVertex3f(320,320,-320);
	glTexCoord2f(1.0, 1.0); glVertex3f(320,-320,-320);
  glTexCoord2f(0.0, 1.0); glVertex3f(320,-320,320);
	glTexCoord2f(0.0, 0.0); glVertex3f(320,320,320);
	glTexCoord2f(1.0, 0.0); glVertex3f(-320,320,320);
	glTexCoord2f(1.0, 1.0); glVertex3f(-320,-320,320);
  glTexCoord2f(0.0, 1.0); glVertex3f(-320,-320,-320);
	glTexCoord2f(0.0, 0.0); glVertex3f(-320,320,-320);
	glEnd;
	// Otexturovan� podlaha
	glBindTexture(GL_TEXTURE_2D, texture[2]);
  glBegin(GL_QUADS);
	glTexCoord2f(1.0, 0.0); glVertex3f(-320,-320,320);
	glTexCoord2f(1.0, 1.0); glVertex3f(320,-320,320);
  glTexCoord2f(0.0, 1.0); glVertex3f(320,-320,-320);
	glTexCoord2f(0.0, 0.0); glVertex3f(-320,-320,-320);
	glEnd;
  // V�lce
	glBindTexture(GL_TEXTURE_2D, texture[0]);
	glColor3f(0.5,0.5,0.5);
  glPushMatrix;
	glRotatef(90, 1,0,0);
	glTranslatef(0,0,-500);
	gluCylinder(cylinder_obj, 60, 60, 1000, 20, 2);
	glPopMatrix;
  glPushMatrix;
  glTranslatef(200,-300,-500);
	gluCylinder(cylinder_obj, 60, 60, 1000, 20, 2);
	glPopMatrix;
	glPushMatrix;
  glTranslatef(-200,0,0);
	glRotatef(135, 1,0,0);
	glTranslatef(0,0,-500);
	gluCylinder(cylinder_obj, 30, 30, 1000, 20, 2);
	glPopMatrix;
  glEnable(GL_BLEND);                                                           // Blending
  glDepthMask(GL_FALSE);                                                        // Vypne z�pis do depth bufferu
  glBindTexture(GL_TEXTURE_2D, texture[1]);                                     // Textura exploze
  for i := 0 to 19 do                                                           // Proch�z� v�buchy
    if ExplosionArray[i]._Alpha >= 0 then                                       // Je exploze vid�t?
      begin
      glPushMatrix;                                                             // Z�loha matice
      ExplosionArray[i]._Alpha := ExplosionArray[i]._Alpha - 0.01;              // Aktualizace alfa hodnoty
      ExplosionArray[i]._Scale := ExplosionArray[i]._Scale + 0.03;              // Aktualizace m���tka
      glColor4f(1,1,0,ExplosionArray[i]._Alpha);                                // �lut� barva s pr�hlednost�
      glScalef(ExplosionArray[i]._Scale,ExplosionArray[i]._Scale,ExplosionArray[i]._Scale); // Zm�na m���tka
      glTranslatef(ExplosionArray[i]._Position.X/ExplosionArray[i]._Scale,ExplosionArray[i]._Position.Y/ExplosionArray[i]._Scale,ExplosionArray[i]._Position.Z/ExplosionArray[i]._Scale); // P�esun na pozici kolizn�ho bodu, m���tko je offsetem
      glCallList(dlist);                                                        // Zavol� display list
      glPopMatrix;                                                              // Obnova p�vodn� matice
      end; 
  glDepthMask(GL_TRUE);                                                         // Obnova p�vodn�ch parametr� OpenGL
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  Result := true;                                                               // Vykreslen� prob�hlo v po��dku
end;


function WndProc(hWnd: HWND;                            // Handle okna
                 message: UINT;                         // Zpr�va pro okno
                 wParam: WPARAM;                        // Dopl�kov� informace
                 lParam: LPARAM):                       // Dopl�kov� informace
                                  LRESULT; stdcall;
begin
  if message=WM_SYSCOMMAND then                         // Syst�mov� p��kaz
    begin
      case wParam of                                    // Typ syst�mov�ho p��kazu
        SC_SCREENSAVE,SC_MONITORPOWER:                  // Pokus o zapnut� �et�i�e obrazovky, Pokus o p�echod do �sporn�ho re�imu?
          begin
            result:=0;                                  // Zabr�n� oboj�mu
            exit;
          end;
      end;
    end;
  case message of                                       // V�tven� podle p��choz� zpr�vy
    WM_ACTIVATE:                                        // Zm�na aktivity okna
      begin
        if (Hiword(wParam)=0) then                      // Zkontroluje zda nen� minimalizovan�
          active:=true                                  // Program je aktivn�
        else
          active:=false;                                // Program nen� aktivn�
        Result:=0;                                      // N�vrat do hlavn�ho cyklu programu
      end;
    WM_CLOSE:                                           // Povel k ukon�en� programu
      Begin
        PostQuitMessage(0);                             // Po�le zpr�vu o ukon�en�
        result:=0                                       // N�vrat do hlavn�ho cyklu programu
      end;
    WM_KEYDOWN:                                         // Stisk kl�vesy
      begin
        keys[wParam] := TRUE;                           // Ozn�m� to programu
        result:=0;                                      // N�vrat do hlavn�ho cyklu programu
      end;
    WM_KEYUP:                                           // Uvoln�n� kl�vesy
      begin
    	keys[wParam] := FALSE;                            // Ozn�m� to programu
        result:=0;                                      // N�vrat do hlavn�ho cyklu programu
      end;
    WM_SIZe:                                            // Zm�na velikosti okna
      begin
    	ReSizeGLScene(LOWORD(lParam),HIWORD(lParam));     // LoWord=���ka, HiWord=V��ka
        result:=0;                                      // N�vrat do hlavn�ho cyklu programu
      end
    else
      // P�ed�n� ostatn�ch zpr�v syst�mu
      begin
      	Result := DefWindowProc(hWnd, message, wParam, lParam);
      end;
    end;
end;


procedure KillGLWindow;                                 // Zav�r�n� okna
begin
  if FullScreen then                                    // Jsme ve fullscreenu?
    begin
      ChangeDisplaySettings(devmode(nil^),0);           // P�epnut� do syst�mu
      showcursor(true);                                 // Zobraz� kurzor my�i
    end;
  if h_rc<> 0 then                                      // M�me rendering kontext?
    begin
      if (not wglMakeCurrent(h_Dc,0)) then              // Jsme schopni odd�lit kontexty?
        MessageBox(0,'Release of DC and RC failed.',' Shutdown Error',MB_OK or MB_ICONERROR);
      if (not wglDeleteContext(h_Rc)) then              // Jsme schopni smazat RC?
        begin
          MessageBox(0,'Release of Rendering Context failed.',' Shutdown Error',MB_OK or MB_ICONERROR);
          h_Rc:=0;                                      // Nastav� hRC na 0
        end;
    end;
  if (h_Dc=1) and (releaseDC(h_Wnd,h_Dc)<>0) then       // Jsme schopni uvolnit DC
    begin
      MessageBox(0,'Release of Device Context failed.',' Shutdown Error',MB_OK or MB_ICONERROR);
      h_Dc:=0;                                          // Nastav� hDC na 0
    end;
  if (h_Wnd<>0) and (not destroywindow(h_Wnd))then      // Jsme schopni odstranit okno?
    begin
      MessageBox(0,'Could not release hWnd.',' Shutdown Error',MB_OK or MB_ICONERROR);
      h_Wnd:=0;                                         // Nastav� hWnd na 0
    end;
  if (not UnregisterClass('OpenGL',hInstance)) then     // Jsme schopni odregistrovat t��du okna?
    begin
      MessageBox(0,'Could Not Unregister Class.','SHUTDOWN ERROR',MB_OK or MB_ICONINFORMATION);
    end;
end;


function CreateGlWindow(title:Pchar; width,height,bits:integer;FullScreenflag:bool):boolean stdcall;
var
  Pixelformat: GLuint;            // Ukl�d� form�t pixel�
  wc:TWndclass;                   // Struktura Windows Class
  dwExStyle:dword;                // Roz���en� styl okna
  dwStyle:dword;                  // Styl okna
  pfd: pixelformatdescriptor;     // Nastaven� form�tu pixel�
  dmScreenSettings: Devmode;      // M�d za��zen�
  h_Instance:hinst;               // Instance okna
  WindowRect: TRect;              // Obd�ln�k okna
begin
  WindowRect.Left := 0;                               // Nastav� lev� okraj na nulu
  WindowRect.Top := 0;                                // Nastav� horn� okraj na nulu
  WindowRect.Right := width;                          // Nastav� prav� okraj na zadanou hodnotu
  WindowRect.Bottom := height;                        // Nastav� spodn� okraj na zadanou hodnotu
  h_instance:=GetModuleHandle(nil);                   // Z�sk� instanci okna
  FullScreen:=FullScreenflag;                         // Nastav� prom�nnou fullscreen na spr�vnou hodnotu
  with wc do
    begin
      style:=CS_HREDRAW or CS_VREDRAW or CS_OWNDC;    // P�ekreslen� p�i zm�n� velikosti a vlastn� DC
      lpfnWndProc:=@WndProc;                          // Definuje proceduru okna
      cbClsExtra:=0;                                  // ��dn� extra data
      cbWndExtra:=0;                                  // ��dn� extra data
      hInstance:=h_Instance;                          // Instance
      hIcon:=LoadIcon(0,IDI_WINLOGO);                 // Standardn� ikona
      hCursor:=LoadCursor(0,IDC_ARROW);               // Standardn� kurzor my�i
      hbrBackground:=0;                               // Pozad� nen� nutn�
      lpszMenuName:=nil;                              // Nechceme menu
      lpszClassName:='OpenGl';                        // Jm�no t��dy okna
    end;
  if  RegisterClass(wc)=0 then                        // Registruje t��du okna
    begin
      MessageBox(0,'Failed To Register The Window Class.','Error',MB_OK or MB_ICONERROR);
      Result:=false;                                  // P�i chyb� vr�t� false
      exit;
    end;
  if FullScreen then                                  // Budeme ve fullscreenu?
    begin
      ZeroMemory( @dmScreenSettings, sizeof(dmScreenSettings) );  // Vynulov�n� pam�ti
      with dmScreensettings do
        begin
          dmSize := sizeof(dmScreenSettings);         // Velikost struktury Devmode
          dmPelsWidth  := width;	                    // ���ka okna
	        dmPelsHeight := height;                     // V��ka okna
          dmBitsPerPel := bits;                       // Barevn� hloubka
          dmFields     := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
        end;
      // Pokus� se pou��t pr�v� definovan� nastaven�
      if (ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN))<>DISP_CHANGE_SUCCESSFUL THEN
        Begin
          // Nejde-li fullscreen, m��e u�ivatel spustit program v okn� nebo ho opustit
          if MessageBox(0,'This FullScreen Mode Is Not Supported. Use Windowed Mode Instead?'
                                             ,'NeHe GL',MB_YESNO or MB_ICONEXCLAMATION)= IDYES then
                FullScreen:=false                     // B�h v okn�
          else
            begin
              // Zobraz� u�ivateli zpr�vu, �e program bude ukon�en
              MessageBox(0,'Program Will Now Close.','Error',MB_OK or MB_ICONERROR);
              Result:=false;                          // Vr�t� FALSE
              exit;
            end;
          end;
    end;
  if FullScreen then                                  // Jsme st�le ve fullscreenu?
    begin
      dwExStyle:=WS_EX_APPWINDOW;                     // Roz���en� styl okna
      dwStyle:=WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN; // Styl okna
      Showcursor(false);                              // Skryje kurzor
    end
  else
    begin
      dwExStyle:=WS_EX_APPWINDOW or WS_EX_WINDOWEDGE;   // Roz���en� styl okna
      dwStyle:=WS_OVERLAPPEDWINDOW or WS_CLIPSIBLINGS or WS_CLIPCHILDREN; // Styl okna
    end;
  AdjustWindowRectEx(WindowRect,dwStyle,false,dwExStyle); // P�izp�soben� velikosti okna
  // Vytvo�en� okna
  H_wnd:=CreateWindowEx(dwExStyle,                    // Roz���en� styl
                               'OpenGl',              // Jm�no t��dy
                               Title,                 // Titulek
                               dwStyle,               // Definovan� styl
                               0,0,                   // Pozice
                               WindowRect.Right-WindowRect.Left,  // V�po�et ���ky
                               WindowRect.Bottom-WindowRect.Top,  // V�po�et v��ky
                               0,                     // ��dn� rodi�ovsk� okno
                               0,                     // Bez menu
                               hinstance,             // Instance
                               nil);                  // Nep�edat nic do WM_CREATE
  if h_Wnd=0 then                                     // Pokud se okno nepoda�ilo vytvo�it
    begin
      KillGlWindow();                                 // Zru�� okno
      MessageBox(0,'Window creation error.','Error',MB_OK or MB_ICONEXCLAMATION);
      Result:=false;                                  // Vr�t� chybu
      exit;
    end;
  with pfd do                                         // Ozn�m�me Windows jak chceme v�e nastavit
    begin
      nSize:= SizeOf( PIXELFORMATDESCRIPTOR );        // Velikost struktury
      nVersion:= 1;                                   // ��slo verze
      dwFlags:= PFD_DRAW_TO_WINDOW                    // Podpora okna
        or PFD_SUPPORT_OPENGL                         // Podpora OpenGL
        or PFD_DOUBLEBUFFER;                          // Podpora Double Bufferingu
      iPixelType:= PFD_TYPE_RGBA;                     // RGBA Format
      cColorBits:= bits;                              // Zvol� barevnou hloubku
      cRedBits:= 0;                                   // Bity barev ignorov�ny
      cRedShift:= 0;
      cGreenBits:= 0;
      cBlueBits:= 0;
      cBlueShift:= 0;
      cAlphaBits:= 0;                                 // ��dn� alpha buffer
      cAlphaShift:= 0;                                // Ignorov�n Shift bit
      cAccumBits:= 0;                                 // ��dn� akumula�n� buffer
      cAccumRedBits:= 0;                              // Akumula�n� bity ignorov�ny
      cAccumGreenBits:= 0;
      cAccumBlueBits:= 0;
      cAccumAlphaBits:= 0;
      cDepthBits:= 16;                                // 16-bitov� hloubkov� buffer (Z-Buffer)
      cStencilBits:= 0;                               // ��dn� Stencil Buffer
      cAuxBuffers:= 0;                                // ��dn� Auxiliary Buffer
      iLayerType:= PFD_MAIN_PLANE;                    // Hlavn� vykreslovac� vrstva
      bReserved:= 0;                                  // Rezervov�no
      dwLayerMask:= 0;                                // Maska vrstvy ignorov�na
      dwVisibleMask:= 0;
      dwDamageMask:= 0;
    end;
  h_Dc := GetDC(h_Wnd);                               // Zkus� p�ipojit kontext za��zen�
  if h_Dc=0 then                                      // Poda�ilo se p�ipojit kontext za��zen�?
    begin
      KillGLWindow();                                 // Zav�e okno
      MessageBox(0,'Cant''t create a GL device context.','Error',MB_OK or MB_ICONEXCLAMATION);
      Result:=false;                                  // Ukon�� program
      exit;
    end;
  PixelFormat := ChoosePixelFormat(h_Dc, @pfd);       // Zkus� naj�t Pixel Format
  if (PixelFormat=0) then                             // Poda�ilo se naj�t Pixel Format?
    begin
      KillGLWindow();                                 // Zav�e okno
      MessageBox(0,'Cant''t Find A Suitable PixelFormat.','Error',MB_OK or MB_ICONEXCLAMATION);
      Result:=false;                                  // Ukon�� program
      exit;
    end;
  if (not SetPixelFormat(h_Dc,PixelFormat,@pfd)) then  // Poda�ilo se nastavit Pixel Format?
    begin
      KillGLWindow();                                 // Zav�e okno
      MessageBox(0,'Cant''t set PixelFormat.','Error',MB_OK or MB_ICONEXCLAMATION);
      Result:=false;                                  // Ukon�� program
      exit;
    end;
  h_Rc := wglCreateContext(h_Dc);                     // Poda�ilo se vytvo�it Rendering Context?
  if (h_Rc=0) then
    begin
      KillGLWindow();                                 // Zav�e okno
      MessageBox(0,'Cant''t create a GL rendering context.','Error',MB_OK or MB_ICONEXCLAMATION);
      Result:=false;                                  // Ukon�� program
      exit;
    end;
  if (not wglMakeCurrent(h_Dc, h_Rc)) then            // Poda�ilo se aktivovat Rendering Context?
    begin
      KillGLWindow();                                 // Zav�e okno
      MessageBox(0,'Cant''t activate the GL rendering context.','Error',MB_OK or MB_ICONEXCLAMATION);
      Result:=false;                                  // Ukon�� program
      exit;
    end;
  ShowWindow(h_Wnd,SW_SHOW);                          // Zobrazen� okna
  SetForegroundWindow(h_Wnd);                         // Do pop�ed�
  SetFOcus(h_Wnd);                                    // Zam��� fokus
  ReSizeGLScene(width,height);                        // Nastaven� perspektivy OpenGL sc�ny
  if (not InitGl()) then                              // Inicializace okna
    begin
      KillGLWindow();                                 // Zav�e okno
      MessageBox(0,'initialization failed.','Error',MB_OK or MB_ICONEXCLAMATION);
      Result:=false;                                  // Ukon�� program
      exit;
    end;
  Result:=true;                                       // V�e prob�hlo v po��dku
end;


function WinMain(hInstance: HINST;                    // Instance
		 hPrevInstance: HINST;                            // P�edchoz� instance
		 lpCmdLine: PChar;                                // Parametry p��kazov� ��dky
		 nCmdShow: integer):                              // Stav zobrazen� okna
                        integer; stdcall;
var
  msg: TMsg;                                          // Struktura zpr�v syst�mu
  done: Bool;                                         // Prom�nn� pro ukon�en� programu

begin
  done:=false;
  // Dotaz na u�ivatele pro fullscreen/okno
  if MessageBox(0,'Would You Like To Run In FullScreen Mode?','Start FullScreen',
                             MB_YESNO or MB_ICONQUESTION)=IDNO then
    FullScreen:=false                                 // B�h v okn�
  else
    FullScreen:=true;                                 // Fullscreen
  InitVars;                                           // Inicializace prom�nn�ch
  if not CreateGLWindow('NeHe''s OpenGL Framework',640,480,16,FullScreen) then // Vytvo�en� OpenGL okna
    begin
      Result := 0;                                    // Konec programu p�i chyb�
      exit;
    end;
  while not done do                                   // Hlavn� cyklus programu
    begin
      if (PeekMessage(msg, 0, 0, 0, PM_REMOVE)) then  // P�i�la zpr�va?
        begin
          if msg.message=WM_QUIT then                 // Obdr�eli jsme zpr�vu pro ukon�en�?
            done:=true                                // Konec programu
          else
            begin
	          TranslateMessage(msg);                    // P�elo�� zpr�vu
	          DispatchMessage(msg);                     // Ode�le zpr�vu
	        end;
        end
      else      // Pokud nedo�la ��dn� zpr�va
        begin
          // Je program aktivn�, ale nelze kreslit? Byl stisknut ESC?
          if (active and not(DrawGLScene()) or keys[VK_ESCAPE]) then
            done:=true                                // Ukon��me program
          else                                        // P�ekreslen� sc�ny
            begin
            Idle;                                     // Vlastn� logika simulace
            SwapBuffers(h_Dc);                        // Prohozen� buffer� (Double Buffering)
            end;
          if keys[VK_F1] then                         // Byla stisknuta kl�vesa F1?
            begin
            Keys[VK_F1] := false;                     // Ozna� ji jako nestisknutou
            KillGLWindow();                           // Zru�� okno
            FullScreen := not FullScreen;             // Negace fullscreen
            // Znovuvytvo�en� okna
            if not CreateGLWindow('NeHe''s OpenGL Framework',640,480,16,fullscreen) then
              Result := 0;                            // Konec programu pokud nebylo vytvo�eno
            end;
          ProcessKeys;                                // Obsluha stisku kl�ves
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  glDeleteTextures(4,texture[0]);                     // Sma�e textury
  glDeleteLists(dlist,1);                             // Zru�� displaylist
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

