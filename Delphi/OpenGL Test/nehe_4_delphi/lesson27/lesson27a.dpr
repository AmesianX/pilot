program lesson27a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages, sysutils,
  OpenGL;

type
  sPoint = record                             // Sou�adnice bodu nebo vektoru
    x, y, z: GLfloat;
    end;

  sPlaneEq = record                           // Rovnice roviny
    a, b, c, d: GLfloat;                      // Ve tvaru ax + by + cz + d = 0
    end;

  sPlane = record                             // Popisuje jeden face objektu
    p: array [0..2] of GLuint;                // Indexy 3 vertex� v objektu, kter� vytv��ej� tento face
    normals: array [0..2] of sPoint;          // Norm�lov� vektory ka�d�ho vertexu
    neigh: array [0..2] of GLuint;            // Indexy sousedn�ch fac�
    PlaneEq: sPlaneEq;                        // Rovnice roviny facu
    visible: boolean;                         // Je face viditeln� (p�ivr�cen� ke sv�tlu)?
    end;

  GLObject = record                           // Struktura objektu
    nPoints: GLuint;                          // Po�et vertex�
    points: array [0..99] of sPoint;          // Pole vertex�
    nPlanes: GLuint;                          // Po�et fac�
    planes: array [0..199] of sPlane;         // Pole fac�
    end;

  GLvector4f = array [0..3] of GLfloat;       // Nov� datov� typ
  GLmatrix16f = array [0..15] of GLfloat;     // Nov� datov� typ

var
  h_Rc: HGLRC;		                                          // Trval� Rendering Context
  h_Dc: HDC;                                                // Priv�tn� GDI Device Context
  h_Wnd: HWND;                                              // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	                            // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;                                      // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;                                   // Ponese informaci o tom, zda je program ve fullscreenu
  obj: GLObject;                                            // Objekt, kter� vrh� st�n
  ObjPos: array [0..2] of GLfloat = (-2.0,-2.0,-5.0);       // Pozice objektu
  xrot: GLfloat = 0;                                        // X rotace
  xspeed: GLfloat = 0;                                      // X rychlost rotace objektu
  yrot: GLfloat = 0;                                        // Y rotace
  yspeed: GLfloat = 0;                                      // Y rychlost rotace objektu
  LightPos: array [0..3] of GLfloat = (0.0,5.0,-4.0,1.0);   // Pozice sv�tla
  LightAmb: array [0..3] of GLfloat = (0.2,0.2,0.2,1.0);    // Ambient sv�tlo
  LightDif: array [0..3] of GLfloat = (0.6,0.6,0.6,1.0);    // Diffuse sv�tlo
  LightSpc: array [0..3] of GLfloat = (-0.2,-0.2,-0.2,1.0); // Specular sv�tlo
  MatAmb: array [0..3] of GLfloat = (0.4,0.4,0.4,1.0);      // Materi�l - Ambient hodnoty (prost�ed�, atmosf�ra)
  MatDif: array [0..3] of GLfloat = (0.2,0.6,0.9,1.0);      // Materi�l - Diffuse hodnoty (rozptylov�n� sv�tla)
  MatSpc: array [0..3] of GLfloat = (0.0,0.0,0.0,1.0);      // Materi�l - Specular hodnoty (zrcadlivost)
  MatShn: array [0..0] of GLfloat = (0.0);                  // Materi�l - Shininess hodnoty (lesk)
  q: GLUquadricObj;                                         // Quadratic pro kreslen� koule
  SpherePos: array [0..2] of GLfloat = (-4.0,-5.0,-6.0);    // Pozice koule

function FindNumber(var retezec: string):string;            // N�hrada funkce fscanf z C++ - vrac� �et�zec obsahuj�c� extrahovan� ��slo
var i: integer;                                             // Index v �et�zci
begin
  for i := 1 to Length(retezec) do
    if retezec[i] <> ' ' then break;                        // Najde prvn� "ne-mezeru" - ��slici nebo znem�nko "-"
  Delete(retezec,1,i-1);                                    // Sma�e nepot�ebnou ��st �et�zce
  i := Pos(' ',retezec)-1;                                  // Index konce ��slice p�ed dal�� mezerou
  if i = -1 then i := Length(retezec);                      // Pokud dal�� mezera neexistuje, vr�t�me konec �et�zce
  Result := Copy(retezec,1,i);                              // Vr�t� ��st �et�zce do dal�� mezery nebo konce �et�zce
  Delete(retezec,1,i);                                      // Sma�e p�e�tenou ��st �et�zce
end;

procedure ReadStr(var f: textfile; var s: string);          // Na�te jeden pou�iteln� ��dek ze souboru
begin
  readln(f,s);                                              // Na�ti ��dek
  while ((copy(s, 1, 1) = '/') or (length(s) = 0)) do       // Pokud nen� pou�iteln� na�ti dal��
    readln(f,s);
end;

function ReadObject(st: string; var o: GLObject): boolean;  // Nahraje objekt
var
  f: TextFile;                                              // Handle souboru
  i: integer;                                               // ��d�c� prom�nn� cykl�
  temp: string;                                             // Pomocn� prom�nn� pro na��t�n� hodnot ze souboru
begin
  AssignFile(f,st);
  {$I-}
  Reset(f);                                                 // Otev�e soubor pro �ten�
  {$I+}
  if IOResult <> 0 then                                     // Poda�ilo se ho otev��t?
    begin
    Result := false;                                        // Pokud ne - konec funkce
    exit;
    end;
  ReadStr(f,temp);                                          // Na�ten� po�tu vertex�
  o.nPoints := StrToInt(FindNumber(temp));
  for i := 1 to o.nPoints do                                // Na��t� vertexy
    begin
    ReadStr(f,temp);
    o.points[i].x := StrToFloat(FindNumber(temp));          // Jednotliv� x, y, z slo�ky
    o.points[i].y := StrToFloat(FindNumber(temp));
    o.points[i].z := StrToFloat(FindNumber(temp));
    end;
  ReadStr(f,temp);
  o.nPlanes := StrToInt(FindNumber(temp));                  // Na�ten� po�tu fac�
  for i := 0 to o.nPlanes - 1 do                            // Na��t� facy
    begin
    ReadStr(f,temp);
    o.planes[i].p[0] := StrToInt(FindNumber(temp));             // Na�ten� index� vertex�
    o.planes[i].p[1] := StrToInt(FindNumber(temp));
    o.planes[i].p[2] := StrToInt(FindNumber(temp));
    o.planes[i].normals[0].x := StrToFloat(FindNumber(temp));   // Norm�lov� vektory prvn�ho vertexu
    o.planes[i].normals[0].y := StrToFloat(FindNumber(temp));
    o.planes[i].normals[0].z := StrToFloat(FindNumber(temp));
    o.planes[i].normals[1].x := StrToFloat(FindNumber(temp));   // Norm�lov� vektory druh�ho vertexu
    o.planes[i].normals[1].y := StrToFloat(FindNumber(temp));
    o.planes[i].normals[1].z := StrToFloat(FindNumber(temp));
    o.planes[i].normals[2].x := StrToFloat(FindNumber(temp));   // Norm�lov� vektory t�et�ho vertexu
    o.planes[i].normals[2].y := StrToFloat(FindNumber(temp));
    o.planes[i].normals[2].z := StrToFloat(FindNumber(temp));
    end;
  CloseFile(f);                                                 // Zav�e soubor
  Result := true;                                               // V�e v po��dku
end;

procedure SetConnectivity(var o :GLObject);                     // Nastaven� soused� jednotliv�ch fac�
var
  p1i, p2i, p1j, p2j: GLuint;                                   // Pomocn� prom�nn�
  q1i, q2i, q1j, q2j: GLuint;                                   // Pomocn� prom�nn�
  i, j, ki, kj: GLuint;                                         // ��d�c� prom�nn� cykl�
begin
  for i := 0 to o.nPlanes - 2 do                                // Ka�d� face objektu (A)
    for j := i + 1 to o.nPlanes - 1 do                          // Ka�d� face objektu (B)
      for ki := 0 to 2 do                                       // Ka�d� okraj facu (A)
        if o.planes[i].neigh[ki] <> 0 then                      // Okraj je�t� nem� souseda?
          for kj := 0 to 2 do                                   // Ka�d� okraj facu (B)
            begin
            p1i := ki;                                          // V�po�ty pro zji�t�n� sousedstv�
            p1j := kj;
            p2i := (ki+1) mod 3;
            p2j := (kj+1) mod 3;
            p1i := o.planes[i].p[p1i];
            p2i := o.planes[i].p[p2i];
            p1j := o.planes[j].p[p1j];
            p2j := o.planes[j].p[p2j];
            q1i := ((p1i+p2i) - abs(p1i-p2i)) div 2;
            q2i := ((p1i+p2i) + abs(p1i-p2i)) div 2;
            q1j := ((p1j+p2j) - abs(p1j-p2j)) div 2;
            q2j := ((p1j+p2j) + abs(p1j-p2j)) div 2;
            if (q1i = q2i) and (q1j = q2j) then                 // Jsou soused�?
              begin
              o.planes[i].neigh[ki] := j+1;
              o.planes[j].neigh[kj] := i+1;
              end;
            end;
end;

procedure DrawGLObject(o: GLObject);                            // Vykreslen� objektu
var
  i, j: GLuint;                                                 // ��d�c� prom�nn� cykl�
begin
  glBegin(GL_TRIANGLES);                                        // Kreslen� troj�heln�k�
    for i := 0 to o.nPlanes-1 do                                // Projde v�echny facy
      for j := 0 to 2 do                                        // Troj�heln�k m� t�i rohy
        begin                                                   // Norm�lov� vektor a um�st�n� bodu
        glNormal3f(o.planes[i].normals[j].x,o.planes[i].normals[j].y,o.planes[i].normals[j].z);
        glVertex3f(o.points[o.planes[i].p[j]].x,o.points[o.planes[i].p[j]].y,o.points[o.planes[i].p[j]].z);
        end;
  glEnd;
end;

procedure CalcPlane(o: GLObject; var plane: sPlane);      // Rovnice roviny ze t�� bod�
var
  v: array [0..3] of sPoint;                              // Pomocn� hodnoty
  i: integer;                                             // ��d�c� prom�nn� cykl�
begin
  for i := 0 to 2 do                                      // Pro zkr�cen� z�pisu
    begin
    v[i+1].x := o.points[plane.p[i]].x;                   // Ulo�� hodnoty do pomocn�ch prom�nn�ch
    v[i+1].y := o.points[plane.p[i]].y;
    v[i+1].z := o.points[plane.p[i]].z;
    end;
  plane.PlaneEq.a := v[1].y*(v[2].z-v[3].z) + v[2].y*(v[3].z-v[1].z) + v[3].y*(v[1].z-v[2].z);
  plane.PlaneEq.b := v[1].z*(v[2].x-v[3].x) + v[2].z*(v[3].x-v[1].x) + v[3].z*(v[1].x-v[2].x);
  plane.PlaneEq.c := v[1].x*(v[2].y-v[3].y) + v[2].x*(v[3].y-v[1].y) + v[3].x*(v[1].y-v[2].y);
  plane.PlaneEq.d := -( v[1].x*(v[2].y*v[3].z - v[3].y*v[2].z) + v[2].x*(v[3].y*v[1].z - v[1].y*v[3].z) + v[3].x*(v[1].y*v[2].z - v[2].y*v[1].z) );
end;

function InitGLObject: boolean;                           // Inicializuje objekty
var i: GLuint;                                            // Cyklus
begin
  if not ReadObject('Data/Object2.txt',obj) then          // Nahraje objekt
    begin
    Result := false;                                      // P�i chyb� konec
    exit;
    end;
  SetConnectivity(obj);                                   // Pospojuje facy (najde sousedy)
  for i := 0 to obj.nPlanes - 1 do                        // Proch�z� facy
    CalcPlane(obj,obj.planes[i]);                         // Spo��t� rovnici roviny facu
  Result := true;                                         // V�e v po��dku
end;

procedure CastShadow(var o: GLObject; lp: GLvector4f);                          // Vr�en� st�nu
var
  i, j, k, jj: GLuint;                                                          // Pomocn�
  p1, p2: GLuint;                                                               // Dva body okraje vertexu, kter� vrhaj� st�n
  v1, v2: sPoint;                                                               // Vektor mezi sv�tlem a p�edchoz�mi body
  side: GLfloat;                                                                // Pomocn� prom�nn�
begin
  for i := 0 to o.nPlanes - 1 do                                                // Projde v�echny facy objektu
    begin                                                                 
    side := o.planes[i].PlaneEq.a*lp[0] + o.planes[i].PlaneEq.b*lp[1]           // Rozhodne jestli je face p�ivr�cen� nebo odvr�cen� od sv�tla
      + o.planes[i].PlaneEq.c*lp[2] + o.planes[i].PlaneEq.d*lp[3];
    if side > 0 then                                                            // Je p�ivr�cen�?
      o.planes[i].visible := true
      else                                                                      // Nen�
      o.planes[i].visible := false;
    end;
  glDisable(GL_LIGHTING);                                                       // Vypne sv�tla
  glDepthMask(GL_FALSE);                                                        // Vypne z�pis do depth bufferu
  glDepthFunc(GL_LEQUAL);                                                       // Funkce depth bufferu
  glEnable(GL_STENCIL_TEST);                                                    // Zapne stencilov� testy
  glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);                             // Nekreslit na obrazovky
  glStencilFunc(GL_ALWAYS,1,$FFFFFFFF);                                         // Funkce stencilu
  glFrontFace(GL_CCW);                                                          // �eln� st�na proti sm�ru hodinov�ch ru�i�ek
  glStencilOp(GL_KEEP,GL_KEEP,GL_INCR);                                         // Zvy�ov�n� hodnoty stencilu
  for i := 0 to o.nPlanes - 1 do                                                // Ka�d� face objektu
    if o.planes[i].visible then                                                 // Je p�ivr�cen� ke sv�tlu
      for j := 0 to 2 do                                                        // Ka�d� okraj facu
        begin
        k := o.planes[i].neigh[j];                                              // Index souseda (pomocn�)
        if (k = 0) or (not o.planes[k-1].visible) then                          // Pokud nem� souseda, kter� je p�ivr�cen� ke sv�tlu
          begin
          p1 := o.planes[i].p[j];                                               // Prvn� bod okraje
          jj := (j+1) mod 3;                                                    // Pro z�sk�n� druh�ho okraje
          p2 := o.planes[i].p[jj];                                              // Druh� bod okraje
          v1.x := (o.points[p1].x - lp[0]) * 100;                               // D�lka vektoru
          v1.y := (o.points[p1].y - lp[1]) * 100;
          v1.z := (o.points[p1].z - lp[2]) * 100;
          v2.x := (o.points[p2].x - lp[0]) * 100;
          v2.y := (o.points[p2].y - lp[1]) * 100;
          v2.z := (o.points[p2].z - lp[2]) * 100;
          glBegin(GL_TRIANGLE_STRIP);                                           // Nakresl� okrajov� polygon st�nu
            glVertex3f(o.points[p1].x, o.points[p1].y, o.points[p1].z);
            glVertex3f(o.points[p1].x + v1.x, o.points[p1].y + v1.y, o.points[p1].z + v1.z);
            glVertex3f(o.points[p2].x, o.points[p2].y, o.points[p2].z);
            glVertex3f(o.points[p2].x + v2.x, o.points[p2].y + v2.y, o.points[p2].z + v2.z);
          glEnd();
          end;
        end;
  glFrontFace(GL_CW);                                                           // �eln� st�na po sm�ru hodinov�ch ru�i�ek
  glStencilOp(GL_KEEP,GL_KEEP,GL_DECR);                                         // Sni�ov�n� hodnoty stencilu
  for i := 0 to o.nPlanes - 1 do                                                // Ka�d� face objektu
    if o.planes[i].visible then                                                 // Je p�ivr�cen� ke sv�tlu
      for j := 0 to 2 do                                                        // Ka�d� okraj facu
        begin
        k := o.planes[i].neigh[j];                                              // Index souseda (pomocn�)
        if (k = 0) or (not o.planes[k-1].visible) then                          // Pokud nem� souseda, kter� je p�ivr�cen� ke sv�tlu
          begin
          p1 := o.planes[i].p[j];                                               // Prvn� bod okraje
          jj := (j+1) mod 3;                                                    // Pro z�sk�n� druh�ho okraje
          p2 := o.planes[i].p[jj];                                              // Druh� bod okraje
          v1.x := (o.points[p1].x - lp[0]) * 100;                               // D�lka vektoru
          v1.y := (o.points[p1].y - lp[1]) * 100;
          v1.z := (o.points[p1].z - lp[2]) * 100;
          v2.x := (o.points[p2].x - lp[0]) * 100;
          v2.y := (o.points[p2].y - lp[1]) * 100;
          v2.z := (o.points[p2].z - lp[2]) * 100;
          glBegin(GL_TRIANGLE_STRIP);                                           // Nakresl� okrajov� polygon st�nu
            glVertex3f(o.points[p1].x, o.points[p1].y, o.points[p1].z);
            glVertex3f(o.points[p1].x + v1.x, o.points[p1].y + v1.y, o.points[p1].z + v1.z);
            glVertex3f(o.points[p2].x, o.points[p2].y, o.points[p2].z);
            glVertex3f(o.points[p2].x + v2.x, o.points[p2].y + v2.y, o.points[p2].z + v2.z);
          glEnd();
          end;
        end;
  glFrontFace(GL_CCW);                                                          // �eln� st�na proti sm�ru hodinov�ch ru�i�ek
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);                                 // Vykreslovat na obrazovku
  glColor4f(0.0,0.0,0.0,0.4);                                                   // �ern�, 40% pr�hledn�
  glEnable(GL_BLEND);                                                           // Zapne blending
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);                             // Typ blendingu
  glStencilFunc(GL_NOTEQUAL,0,$FFFFFFFF);                                       // Nastaven� stencilu
  glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);                                         // Nem�nit hodnotu stencilu
  glPushMatrix();                                                               // Ulo�� matici
  glLoadIdentity();                                                             // Reset matice
  glBegin(GL_TRIANGLE_STRIP);                                                   // �ern� obd�ln�k
    glVertex3f(-0.1, 0.1,-0.10);
    glVertex3f(-0.1,-0.1,-0.10);
    glVertex3f( 0.1, 0.1,-0.10);
    glVertex3f( 0.1,-0.1,-0.10);
  glEnd();
  glPopMatrix();                                                                // Obnov� matici
  glDisable(GL_BLEND);                                                          // Obnov� zm�n�n� parametry OpenGL
  glDepthFunc(GL_LEQUAL);
  glDepthMask(GL_TRUE);
  glEnable(GL_LIGHTING);
  glDisable(GL_STENCIL_TEST);
  glShadeModel(GL_SMOOTH);
end;

procedure ReSizeGLScene(Width: GLsizei; Height: GLsizei); // Zm�na velikosti a inicializace OpenGL okna
begin
  if (Height=0) then		                                  // Zabezpe�en� proti d�len� nulou
     Height:=1;                                           // Nastav� v��ku na jedna
  glViewport(0, 0, Width, Height);                        // Resetuje aktu�ln� nastaven�
  glMatrixMode(GL_PROJECTION);                            // Zvol� projek�n� matici
  glLoadIdentity();                                       // Reset matice
  gluPerspective(45.0,Width/Height,0.1,100.0);            // V�po�et perspektivy
  glMatrixMode(GL_MODELVIEW);                             // Zvol� matici Modelview
  glLoadIdentity;                                         // Reset matice
end;


function InitGL:bool;	                              // V�echno nastaven� OpenGL
begin
  if not InitGLObject then                          // Nahraje objekt
    begin
    Result := false;
    exit;
    end;
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glClearColor(0.0, 0.0, 0.0, 0.5);	  	            // �ern� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glClearStencil(0);                                // Nastaven� stencil bufferu
  glEnable(GL_DEPTH_TEST);			                    // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LEQUAL);				                    // Typ hloubkov�ho testov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST); // Nejlep�� perspektivn� korekce
  glLightfv(GL_LIGHT1,GL_POSITION,@LightPos);       // Pozice sv�tla
  glLightfv(GL_LIGHT1,GL_AMBIENT,@LightAmb);        // Ambient sv�tlo
  glLightfv(GL_LIGHT1,GL_DIFFUSE,@LightDif);        // Diffuse sv�tlo
  glLightfv(GL_LIGHT1,GL_SPECULAR,@LightSpc);       // Specular sv�tlo
  glEnable(GL_LIGHT1);                              // Zapne sv�tlo 1
  glEnable(GL_LIGHTING);                            // Zapne sv�tla
  glMaterialfv(GL_FRONT,GL_AMBIENT,@MatAmb);        // Prost�ed�, atmosf�ra
  glMaterialfv(GL_FRONT,GL_DIFFUSE,@MatDif);        // Rozptylov�n� sv�tla
  glMaterialfv(GL_FRONT,GL_SPECULAR,@MatSpc);       // Zrcadlivost
  glMaterialfv(GL_FRONT,GL_SHININESS,@MatShn);      // Lesk
  glCullFace(GL_BACK);                              // O�ez�v�n� zadn�ch stran
  glEnable(GL_CULL_FACE);                           // Zapne o�ez�v�n�
  q := gluNewQuadric;                               // Nov� quadratic
  gluQuadricNormals(q,GL_SMOOTH);                   // Generov�n� norm�lov�ch vektor� pro sv�tlo
  gluQuadricTexture(q,GL_FALSE);                    // Nepot�ebujeme texturovac� koordin�ty
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;

procedure DrawGLRoom;                               // Vykresl� m�stnost (krychli)
begin
  glBegin(GL_QUADS);                                // Za��tek kreslen� obd�ln�k�
    // Podlaha
    glNormal3f(0.0, 1.0, 0.0);                      // Norm�la sm��uje nahoru
    glVertex3f(-10.0,-10.0,-20.0);                  // Lev� zadn�
    glVertex3f(-10.0,-10.0, 20.0);                  // Lev� p�edn�
    glVertex3f( 10.0,-10.0, 20.0);                  // Prav� p�edn�
    glVertex3f( 10.0,-10.0,-20.0);                  // Prav� zadn�
    // Strop
    glNormal3f(0.0,-1.0, 0.0);                      // Norm�la sm��uje dol�
    glVertex3f(-10.0, 10.0, 20.0);                  // Lev� p�edn�
    glVertex3f(-10.0, 10.0,-20.0);                  // Lev� zadn�
    glVertex3f( 10.0, 10.0,-20.0);                  // Prav� zadn�
    glVertex3f( 10.0, 10.0, 20.0);                  // Prav� p�edn�
    // �eln� st�na
    glNormal3f(0.0, 0.0, 1.0);                      // Norm�la sm��uje do hloubky
    glVertex3f(-10.0, 10.0,-20.0);                  // Lev� horn�
    glVertex3f(-10.0,-10.0,-20.0);                  // Lev� doln�
    glVertex3f( 10.0,-10.0,-20.0);                  // Prav� doln�
    glVertex3f( 10.0, 10.0,-20.0);                  // Prav� horn�
    // Zadn� st�na
    glNormal3f(0.0, 0.0,-1.0);                      // Norm�la sm��uje k obrazovce
    glVertex3f( 10.0, 10.0, 20.0);                  // Prav� horn�
    glVertex3f( 10.0,-10.0, 20.0);                  // Prav� spodn�
    glVertex3f(-10.0,-10.0, 20.0);                  // Lev� spodn�
    glVertex3f(-10.0, 10.0, 20.0);                  // Lev� zadn�
    // Lev� st�na
    glNormal3f(1.0, 0.0, 0.0);                      // Norm�la sm��uje doprava
    glVertex3f(-10.0, 10.0, 20.0);                  // P�edn� horn�
    glVertex3f(-10.0,-10.0, 20.0);                  // P�edn� doln�
    glVertex3f(-10.0,-10.0,-20.0);                  // Zadn� doln�
    glVertex3f(-10.0, 10.0,-20.0);                  // Zadn� horn�
    // Prav� st�na
    glNormal3f(-1.0, 0.0, 0.0);                     // Norm�la sm��uje doleva
    glVertex3f( 10.0, 10.0,-20.0);                  // Zadn� horn�
    glVertex3f( 10.0,-10.0,-20.0);                  // Zadn� doln�
    glVertex3f( 10.0,-10.0, 20.0);                  // P�edn� doln�
    glVertex3f( 10.0, 10.0, 20.0);                  // P�edn� horn�
  glEnd();                                          // Konec kreslen�
end;

procedure VMatMult(M: GLmatrix16f; var v: GLvector4f);
var res: GLvector4f;                                            // Ukl�d� v�sledky
begin
  res[0] := M[ 0]*v[0] + M[ 4]*v[1] + M[ 8]*v[2] + M[12]*v[3];
  res[1] := M[ 1]*v[0] + M[ 5]*v[1] + M[ 9]*v[2] + M[13]*v[3];
  res[2] := M[ 2]*v[0] + M[ 6]*v[1] + M[10]*v[2] + M[14]*v[3];
  res[3] := M[ 3]*v[0] + M[ 7]*v[1] + M[11]*v[2] + M[15]*v[3];
  v[0] := res[0];                                               // V�sledek ulo�� zp�t do v
  v[1] := res[1];
  v[2] := res[2];
  v[3] := res[3];                                               // Homogenn� sou�adnice
end;

function DrawGLScene():bool;                            // Vykreslov�n�
var
  Minv: GLmatrix16f;                                    // OpenGL matice
  wlp, lp: GLvector4f;                                  // Relativn� pozice sv�tla
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT
          or GL_STENCIL_BUFFER_BIT);                    // Sma�e buffery
  glLoadIdentity();	                                    // Reset matice
  glTranslatef(0.0,0.0,-20.0);                          // P�esun 20 jednotek do hloubky
  glLightfv(GL_LIGHT1,GL_POSITION,@LightPos);           // Um�st�n� sv�tla
  glTranslatef(SpherePos[0],SpherePos[1],SpherePos[2]); // Um�st�n� koule
  gluSphere(q,1.5,32,16);                               // Vykreslen� koule
  glLoadIdentity;                                       // Reset matice
  glRotatef(-yrot,0.0,1.0,0.0);                         // Rotace na ose y
  glRotatef(-xrot,1.0,0.0,0.0);                         // Rotace na ose x
  glGetFloatv(GL_MODELVIEW_MATRIX,@Minv);               // Ulo�en� ModelView matice do Minv
  lp[0] := LightPos[0];                                 // Ulo�en� pozice sv�tla
  lp[1] := LightPos[1];
  lp[2] := LightPos[2];
  lp[3] := LightPos[3];
  VMatMult(Minv, lp);                                   // Vyn�soben� pozice sv�tla OpenGL matic�
  glTranslatef(-ObjPos[0],-ObjPos[1],-ObjPos[2]);       // Posun z�porn� o pozici objektu
  glGetFloatv(GL_MODELVIEW_MATRIX, @Minv);              // Ulo�en� ModelView matice do Minv
  wlp[0] := 0.0;                                        // Glob�ln� koordin�ty na nulu
  wlp[1] := 0.0;
  wlp[2] := 0.0;
  wlp[3] := 1.0;
  VMatMult(Minv, wlp);                                  // Origin�ln� glob�ln� sou�adnicov� syst�m relativn� k lok�ln�mu
  lp[0] := lp[0] + wlp[0];                              // Pozice sv�tla je relativn� k lok�ln�mu sou�adnicov�mu syst�mu objektu
  lp[1] := lp[1] + wlp[1];
  lp[2] := lp[2] + wlp[2];
  glLoadIdentity;                                       // Reset matice
  glTranslatef(0.0,0.0,-20.0);                          // P�esun 20 jednotek do hloubky
  DrawGLRoom;                                           // Vykreslen� m�stnosti
  glTranslatef(ObjPos[0],ObjPos[1],ObjPos[2]);          // Um�st�n� objektu
  glRotatef(xrot,1.0,0.0,0.0);                          // Rotace na ose x
  glRotatef(yrot,0.0,1.0,0.0);                          // Rotace na ose y
  DrawGLObject(obj);                                    // Vykreslen� objektu
  CastShadow(obj,lp);                                   // Vr�en� st�nu zalo�en� na siluet�
  glColor4f(0.7,0.4,0.0,1.0);                           // Oran�ov� barva
  glDisable(GL_LIGHTING);                               // Vypne sv�tlo
  glDepthMask(GL_FALSE);                                // Vypne masku hloubky
  glTranslatef(lp[0],lp[1],lp[2]);                      // Translace na pozici sv�tla
  gluSphere(q,0.2,16,8);                                // Vykreslen� mal� koule (reprezentuje sv�tlo)
  glEnable(GL_LIGHTING);                                // Zapne sv�tlo
  glDepthMask(GL_TRUE);                                 // Zapne masku hloubky
  xrot := xrot + xspeed;                                // Zv�t�en� �hlu rotace objektu
  yrot := yrot + yspeed;
  glFlush;
  Result := true;                                       // Vykreslen� prob�hlo v po��dku
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
      cStencilBits:= 1;                               // Stencil Buffer
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

procedure ProcessKeyboard;                                      // O�et�en� kl�vesnice
begin
  // Rotace objektu
  if keys[VK_LEFT] then yspeed := yspeed - 0.1;                 // �ipka vlevo - sni�uje y rychlost
  if keys[VK_RIGHT] then yspeed := yspeed + 0.1;                // �ipka vpravo - zvy�uje y rychlost
  if keys[VK_UP] then xspeed := xspeed - 0.1;                   // �ipka nahoru - sni�uje x rychlost
  if keys[VK_DOWN] then xspeed := xspeed + 0.1;                 // �ipka dol� - zvy�uje x rychlost
  // Pozice objektu
  if keys[VK_NUMPAD6] then ObjPos[0] := ObjPos[0] + 0.05;       // '6' - pohybuje objektem doprava
  if keys[VK_NUMPAD4] then ObjPos[0] := ObjPos[0] - 0.05;       // '4' - pohybuje objektem doleva
  if keys[VK_NUMPAD8] then ObjPos[1] := ObjPos[1] + 0.05;       // '8' - pohybuje objektem nahoru
  if keys[VK_NUMPAD5] then ObjPos[1] := ObjPos[1] - 0.05;       // '5' - pohybuje objektem dol�
  if keys[VK_NUMPAD9] then ObjPos[2] := ObjPos[2] + 0.05;       // '9' - p�ibli�uje objekt
  if keys[VK_NUMPAD7] then ObjPos[2] := ObjPos[2] - 0.05;       // '7' oddaluje objekt
  // Pozice sv�tla
  if keys[Ord('L')] then LightPos[0] := LightPos[0] + 0.05;     // 'L' - pohybuje sv�tlem doprava
  if keys[Ord('J')] then LightPos[0] := LightPos[0] - 0.05;     // 'J' - pohybuje sv�tlem doleva
  if keys[Ord('I')] then LightPos[1] := LightPos[1] + 0.05;     // 'I' - pohybuje sv�tlem nahoru
  if keys[Ord('K')] then LightPos[1] := LightPos[1] - 0.05;     // 'K' - pohybuje sv�tlem dol�
  if keys[Ord('O')] then LightPos[2] := LightPos[2] + 0.05;     // 'O' - p�ibli�uje sv�tlo
  if keys[Ord('U')] then LightPos[2] := LightPos[2] - 0.05;     // 'U' - oddaluje sv�tlo
  // Pozice koule
  if keys[Ord('D')] then SpherePos[0] := SpherePos[0] + 0.05;   // 'D' - pohybuje koul� doprava
  if keys[Ord('A')] then SpherePos[0] := SpherePos[0] - 0.05;   // 'A' - pohybuje koul� doleva
  if keys[Ord('W')] then SpherePos[1] := SpherePos[1] + 0.05;   // 'W' - pohybuje koul� nahoru
  if keys[Ord('S')] then SpherePos[1] := SpherePos[1] - 0.05;   // 'S'- pohybuje koul� dol�
  if keys[Ord('E')] then SpherePos[2] := SpherePos[2] + 0.05;   // 'E' - p�ibli�uje kouli
  if keys[Ord('Q')] then SpherePos[2] := SpherePos[2] - 0.05;   // 'Q' - oddaluje kouli
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
  if not CreateGLWindow('NeHe''s OpenGL Framework',800,600,32,FullScreen) then // Vytvo�en� OpenGL okna
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
            SwapBuffers(h_Dc);                        // Prohozen� buffer� (Double Buffering)
          if keys[VK_F1] then                         // Byla stisknuta kl�vesa F1?
            begin
            Keys[VK_F1] := false;                     // Ozna� ji jako nestisknutou
            KillGLWindow();                           // Zru�� okno
            FullScreen := not FullScreen;             // Negace fullscreen
            // Znovuvytvo�en� okna
            if not CreateGLWindow('NeHe''s OpenGL Framework',800,600,32,fullscreen) then
              Result := 0;                            // Konec programu pokud nebylo vytvo�eno
            end;
          ProcessKeyboard;
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  DecimalSeparator := '.';                            // Desetinn� te�ka
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );  // Start programu
  DecimalSeparator := ',';                            // Zp�t desetinn� ��rka
end.

