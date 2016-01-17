program lesson32a;

{   k�d pro Delphi 7}

uses
  Windows,
  SysUtils,
  Messages,
  OpenGL,
  MMSystem,
  NeHeGL in 'NeHeGL.pas';

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); stdcall; external 'opengl32';

type
  objects = record                                      // Struktura objektu
    rot: GLuint;                                        // Rotace (0 - ��dn�, 1 - po sm�ru hodinov�ch ru�i�ek, 2 - proti sm�ru)
    hit: boolean;                                       // Byl objekt zasa�en?
    frame: GLuint;                                      // Aktu�ln� sn�mek exploze
    dir: GLuint;                                        // Sm�r pohybu (0 - vlevo, 1 - vpravo, 2 - nahoru, 3 - dol�)
    texid: GLuint;                                      // Index do pole textur
    x: GLfloat;                                         // X pozice
    y: GLfloat;                                         // Y pozice
    spin: GLfloat;                                      // Sm�r rotace na ose z
    distance: GLfloat;                                  // Hloubka ve sc�n�
    end;

  TextureImage = record                                 // Struktura textury
    imageData: ^GLubyte;                                // Ukazatel na data
    bpp: GLuint;                                        // Barevn� hloubka
    width: GLuint;                                      // ���ka
    height: GLuint;                                     // V��ka
    texID: GLuint;                                      // ��slo textury
    end;

  dimensions = record                                   // Rozm�r objektu
    w: GLfloat;                                         // ���ka
    h: GLfloat;                                         // V��ka
    end;

var
  g_window: PGL_Window;                                 // Okno
  g_keys: PKeys;                                        // Kl�vesy
  base: GLuint;                                         // Display listy fontu
  roll: GLfloat;                                        // Rolov�n� mrak�
  level: GLint = 1;                                     // Aktu�ln� level
  miss: GLint;                                          // Po�et nesest�elen�ch objekt�
  kills: GLint;                                         // Po�et sest�elen�ch objekt� v dan�m levelu
  score: GLint;                                         // Aktu�ln� sk�re
  game: boolean;                                        // Konec hry?
  textures: array [0..9] of TextureImage;               // Deset textur
  obj: array [0..29] of objects;                        // 30 Objekt�
  // Velikost ka�d�ho objektu:          Modr� tv��,       k�bl,           ter�,         Coca-cola,        V�za
  size: array [0..4] of dimensions = ((w: 1.0;h: 1.0),(w: 1.0;h: 1.0),(w: 1.0;h: 1.0),(w: 0.5;h: 1.0),(w: 0.75;h: 1.5));

function LoadTGA(var texture: TextureImage; filename: string): boolean;
var
  TGAheader: array [0..11] of byte;                                   // Nekomprimovan� TGA hlavi�ka  {0,0,2,0,0,0,0,0,0,0,0,0}
  TGAcompare: array [0..11] of byte;                                  // Pro porovn�n� TGA hlavi�ky
  Header: array [0..5] of byte;                                       // Prvn�ch 6 u�ite�n�ch byt� z hlavi�ky
  BytesPerPixel: GLuint;                                              // Po�et byt� na pixel pou�it� v TGA souboru
  ImageSize: GLuint;                                                  // Ukl�d� velikost obr�zku p�i alokov�n� RAM
  temp: GLubyte;                                                      // Pomocn� prom�nn�
  TypeMode: GLuint;                                                   // GL m�d
  f: file;                                                            // Soubor TGA
  precteno: Gluint;                                                   // Po�et p�e�ten�ch byt�
  i: integer;                                                         // Cyklus
  B, R: PGLubyte;                                                     // Ukazatel na prohazovan� slo�ky barev
begin
  ZeroMemory(@TGAheader,sizeof(TGAheader));                           // Nulov�n� prvk� pole
  TGAheader[2] := 2;                                                  // T�et� prvek hlavi�ky je 2 - viz deklarace
  TypeMode := GL_RGBA;                                                // Implicitn�m GL m�dem je RGBA (32 BPP)
  if not FileExists(filename) then                                    // Existuje soubor?
    begin
    Result := false;                                                  // Konec funkce
    exit;
    end;
  AssignFile(f,filename);                                             // P�i�azen� souboru
  Reset(f,1);                                                         // Otev�e TGA soubor
  BlockRead(f,TGAcompare,Sizeof(TGAcompare),precteno);                // Na�te hlavi�ku
  if (precteno <> Sizeof(TGAcompare)) or                              // Poda�ilo se na��st 12 byt�?
    (not CompareMem(@TGAcompare,@TGAHeader,sizeof(TGAheader))) then   // Maj� pot�ebn� hodnoty?
    begin
    Result := false;                                                  // Konec funkce
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  BlockRead(f,Header,Sizeof(Header),precteno);                        // Pokud ano, na�te dal��ch �est byt�
  if precteno <> sizeof(Header) then                                  // Poda�ilo se na��st 6 byt�?
    begin
    Result := false;                                                  // Konec funkce
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  texture.width := Header[1] * 256 + Header[0];                       // Z�sk� ���ku obr�zku
  texture.height := Header[3] * 256 + Header[2];                      // Z�sk� v��ku obr�zku
  if (texture.width <= 0) or (texture.height <= 0)                    // Platn� ���ka? Platn� v��ka?
    or ((Header[4] <> 24) and (Header[4] <> 32)) then                 // Platn� barevn� hloubka?
    begin
    Result := false;                                                  // Konec funkce
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  texture.bpp := Header[4];                                           // Bity na pixel (24 nebo 32)
  BytesPerPixel := texture.bpp div 8;                                 // Byty na pixel
  ImageSize := texture.width * texture.height * BytesPerPixel;        // Velikost pam�ti pro data obr�zku
  texture.imageData := AllocMem(ImageSize);                           // Alokace pam�ti pro data obr�zku
  if texture.imageData = nil then                                     // Poda�ilo se pam� alokovat?
    begin
    Result := false;                                                  // Konec funkce
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  BlockRead(f,texture.imageData^,ImageSize,precteno);                 // Kop�rov�n� dat
  if precteno <> ImageSize then                                       // Poda�ilo se kop�rov�n� dat?
    begin
    Result := false;                                                  // Konec funkce
    FreeMem(texture.imageData);                                       // Uvoln� pam�
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  for i := 0 to (texture.width * texture.height) - 1 do               // Proch�z� data obr�zku
    begin
    B := Pointer(Integer(texture.imageData) + i * BytesPerPixel);     // Ukazatel na B
    R := Pointer(Integer(texture.imageData) + i * BytesPerPixel+2);   // Ukazatel na R
    temp := B^;                                                       // B ulo��me do pomocn� prom�nn�
    B^ := R^;                                                         // R je na spr�vn�m m�st�
    R^ := temp;                                                       // B je na spr�vn�m m�st�
    end;
  CloseFile(f);                                                       // Zav�e soubor
  glGenTextures(1,texture.texID);                                     // Generuje texturu
  glBindTexture(GL_TEXTURE_2D,texture.texID);                         // Zvol� texturu
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);     // Line�rn� filtrov�n�
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);     // Line�rn� filtrov�n�
  if texture.bpp = 24 then TypeMode := GL_RGB;                        // Je obr�zek 24 bitov�? Nastav� typ na GL_RGB
  glTexImage2D(GL_TEXTURE_2D,0,TypeMode,texture.width,texture.height,0,TypeMode,GL_UNSIGNED_BYTE,texture.imageData);  // Vytvo�� texturu
  FreeMem(texture.imageData);                                         // Uvoln� pam�
  Result := true;
end;

procedure BuildFont;                                                  // Vytvo�en� fontu
var
  cx, cy: GLfloat;                                                    // Koordin�ty x, y
  loop: integer;                                                      // Cyklus
begin
  base := glGenLists(95);                                             // 95 display list�
  glBindTexture(GL_TEXTURE_2D,textures[9].texID);                     // V�b�r textury
  for loop := 0 to 94 do                                              // Vytv��� 95 display list�
    begin
    cx := (loop mod 16) / 16;                                         // X pozice aktu�ln�ho znaku
    cy := (loop div 16) / 8;                                          // Y pozice aktu�ln�ho znaku
    glNewList(base + loop,GL_COMPILE);                                // Vytvo�en� display listu
      glBegin(GL_QUADS);                                              // Pro ka�d� znak jeden obd�ln�k
        glTexCoord2f(cx,1-cy-0.120);glVertex2i(0,0);                  // Lev� doln�
        glTexCoord2f(cx+0.0625,1-cy-0.120);glVertex2i(16,0);          // Prav� doln�
        glTexCoord2f(cx+0.0625,1-cy);glVertex2i(16,16);               // Prav� horn�
        glTexCoord2f(cx,1-cy);glVertex2i(0,16);                       // Lev� horn�
      glEnd;                                                          // Konec znaku
      glTranslated(10,0,0);                                           // P�esun na pravou stranu znaku
    glEndList;                                                        // Konec kreslen� display listu
    end;
end;

procedure glPrint(x,y: GLint;text: string);
begin
  if text = '' then exit;                                             // Byl p�ed�n text?
  glBindTexture(GL_TEXTURE_2D,textures[9].texID);                     // V�b�r textury
  glPushMatrix;                                                       // Ulo�� projek�n� matici
  glLoadIdentity;                                                     // Reset matice
  glTranslated(x,y,0);                                                // Pozice textu (0,0 - lev� doln�)
  glListBase(base-32);                                                // Po��tek
  glCallLists(length(text),GL_UNSIGNED_BYTE,Pchar(text));             // Vykreslen� textu na obrazovku
  glPopMatrix;                                                        // Obnoven� ulo�en� projek�n� matice
end;

procedure InitObject(num: integer);                                   // Inicializace objektu
var
  i,j: integer;                                                       // Cykly
  sorting: boolean;                                                   // T��d�n�
  tempobj: objects;                                                   // Pomocn� objekt
begin
  with obj[num] do
    begin
    rot := 1;                                                         // Rotace po sm�ru hodinov�ch ru�i�ek
    frame := 0;                                                       // Prvn� sn�mek exploze
    hit := false;                                                     // Je�t� nebyl zasa�en
    texid := Random(5);                                               // N�hodn� index textury
    distance := - Random(4001) / 100;                                 // N�hodn� hloubka
    y := -1.5 + (Random(451) / 100);                                  // N�hodn� y pozice
    x := ((distance - 15) / 2) - (5 * level) - Random(5 * level);     // N�hodn� x pozice zalo�en� na hloubce v obrazovce a s n�hodn�m zpo�d�n�m p�ed vstupem na sc�nu
    dir := Random(2);                                                 // N�hodn� sm�r pohybu
    if dir = 0 then                                                   // Pohybuje se doleva?
      begin
      rot := 2;                                                       // Rotace proti sm�ru hodinov�ch ru�i�ek
      x := -x;                                                        // V�choz� pozice vpravo
      end;
    if texid = 0 then                                                 // Modr� tv��
      y := -2.0;                                                      // V�dy t�sn� nad zem�
    if texid = 1 then                                                 // K�bl
      begin
      dir := 3;                                                       // Pad� dol�
      x := Random(Abs(Trunc(distance - 10))) + ((distance - 10) / 2);
      y := 4.5;                                                       // T�sn� pod mraky
      end;
    if texid = 2 then                                                 // Ter�
      begin
      dir := 2;                                                       // Vylet� vzh�ru
      x := Random(Abs(Trunc(distance - 10))) + ((distance - 10) / 2);
      y := -3.0 - Random(5 * level);                                  // Pod zem�
      end;
    end;
  For j := level-1 downto 1 do begin                                  // �azen� objekt� podle hloubky
    i := j;
    sorting := true;
    While (sorting) and (i > 0) do
      if (obj[i].distance < obj[i-1].distance) then begin
        tempobj := obj[i];
        obj[i] := obj[i-1];
        obj[i-1] := tempobj;
        Dec(i);
        end else sorting := false;
    end;

end;

function Initialize(window: PGL_Window; key: PKeys): boolean;	        // Inicializace OpenGL
var
  loop: integer;                                                      // Cyklus
begin
  g_window := window;
  g_keys := key;
  Randomize;                                                          // Inicializace gener�toru n�hodn�ch ��sel
  LoadTGA(textures[0],'Data/BlueFace.tga');                           // Modr� tv��
  LoadTGA(textures[1],'Data/Bucket.tga');                             // Kbel�k
  LoadTGA(textures[2],'Data/Target.tga');                             // Ter�
  LoadTGA(textures[3],'Data/Coke.tga');                               // Coca-Cola
  LoadTGA(textures[4],'Data/Vase.tga');                               // V�za
  LoadTGA(textures[5],'Data/Explode.tga');                            // Exploze
  LoadTGA(textures[6],'Data/Ground.tga');                             // Zem�
  LoadTGA(textures[7],'Data/Sky.tga');                                // Obloha
  LoadTGA(textures[8],'Data/Crosshair.tga');                          // Kurzor
  LoadTGA(textures[9],'Data/Font.tga');                               // Font
  if not (Assigned(textures[0].imageData) and Assigned(textures[1].imageData) and
          Assigned(textures[2].imageData) and Assigned(textures[3].imageData) and
          Assigned(textures[4].imageData) and Assigned(textures[5].imageData) and
          Assigned(textures[6].imageData) and Assigned(textures[7].imageData) and
          Assigned(textures[8].imageData) and Assigned(textures[9].imageData)) then
    begin
    Result := false;                                                  // Inicializace se nezda�ila
    exit;
    end; 
  BuildFont;                                                          // Vytvo�� display listy fontu
  glClearColor(0.0, 0.0, 0.0, 0.0);	  	                              // �ern� pozad�
  glClearDepth(1.0);				                                          // Nastaven� hloubkov�ho bufferu
  glDepthFunc(GL_LEQUAL);				                                      // Typ hloubkov�ho testov�n�
  glEnable(GL_DEPTH_TEST);			                                      // Povol� hloubkov� testov�n�
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);                   // Nastaven� alfa blendingu
  glEnable(GL_BLEND);                                                 // Zapne alfa blending
	//glAlphaFunc(GL_GREATER,0.1);                                      // Nastaven� alfa testingu
	//glEnable(GL_ALPHA_TEST);                                          // Zapne alfa testing
	glEnable(GL_TEXTURE_2D);                                            // Zapne mapov�n� textur
	glEnable(GL_CULL_FACE);                                             // O�ez�v�n� zadn�ch stran polygon�
  for loop := 0 to 29 do                                              // Proch�z� v�echny objekty
    InitObject(loop);                                                 // Inicializace ka�d�ho z nich
  Result:=true;                                                       // Inicializace prob�hla v po��dku
end;

procedure Deinitialize;                                               // Uvoln�n� prost�edk�
var
  loop: integer;                                                      // Cyklus
begin
  for loop := 0 to 9 do
    glDeleteTextures(loop,@textures[loop].texid);                     // Smaz�n� textur
  glDeleteLists(base,95);                                             // Smaz�n� display listu
end;

procedure Explosion(num: integer);                                    // Animace exploze objektu
var
  ex, ey: GLfloat;                                                    // Subkoordin�ty v textu�e
begin
  ex := ((obj[num].frame div 4) mod 4) / 4;                           // V�po�et x sn�mku exploze (0.0f - 0.75f)
  ey := ((obj[num].frame div 4) div 4) / 4;                           // V�po�et y sn�mku exploze (0.0f - 0.75f)
  glBindTexture(GL_TEXTURE_2D,textures[5].texID);			                // Textura exploze
	glBegin(GL_QUADS);											                            // Kreslen� obd�ln�k�
		glTexCoord2f(ex,1.0-ey); glVertex3f(-1.0,-1.0,0.0);               // Lev� doln�
		glTexCoord2f(ex+0.25,1.0-ey); glVertex3f(1.0,-1.0,0.0);           // Prav� doln�
		glTexCoord2f(ex+0.25,1.0-(ey+0.25)); glVertex3f(1.0,1.0,0.0);     // Prav� horn�
		glTexCoord2f(ex,1.0-(ey+0.25)); glVertex3f(-1.0,1.0,0.0);         // Lev� horn�
	glEnd;                                                              // Konec kreslen�
  Inc(obj[num].frame);										                            // Zv��� sn�mek exploze
	if obj[num].frame > 63 then									                        // Posledn� sn�mek?
		InitObject(num);										                              // Reinicializace objektu
end;

procedure Objekt(width, height: GLfloat; texid: GLuint);              // Vykresl� objekt
begin
  glBindTexture(GL_TEXTURE_2D,textures[texid].texID);		              // Zvol� spr�vnou texturu
	glBegin(GL_QUADS);											                            // Kreslen� obd�ln�k�
		glTexCoord2f(0.0,0.0); glVertex3f(-width,-height,0.0);            // Lev� doln�
		glTexCoord2f(1.0,0.0); glVertex3f( width,-height,0.0);            // Prav� doln�
		glTexCoord2f(1.0,1.0); glVertex3f( width, height,0.0);            // Prav� horn�
		glTexCoord2f(0.0,1.0); glVertex3f(-width, height,0.0);            // Lev� horn�
	glEnd;                                                              // Konec kreslen�
end;

procedure DrawTargets;                                                // Vykresl� objekty
var
  loop: integer;                                                      // Cyklus
begin
  glLoadIdentity;											                                // Reset matice
	glTranslatef(0.0,0.0,-10.0);								                        // Posun do hloubky
	for loop := 0 to level - 1 do						                            // Proch�z� aktivn� objekty
	  begin
		glLoadName(loop);										                              // P�i�ad� objektu jm�no (pro detekci z�sah�)
		glPushMatrix;											                                // Ulo�en� matice
		glTranslatef(obj[loop].x,obj[loop].y,obj[loop].distance);		      // Um�st�n� objektu
		if obj[loop].hit then									                            // Byl objekt zasa�en?
			Explosion(loop)									                                // Vykresl� sn�mek exploze
		  else                                                            // Objekt nebyl zasa�en
		  begin
			glRotatef(obj[loop].spin,0.0,0.0,1.0);		                      // Nato�en� na ose z
			Objekt(size[obj[loop].texid].w,size[obj[loop].texid].h,obj[loop].texid);	// Vykreslen�
		  end;
		glPopMatrix;											                                // Obnov� matici
	  end;
end;

procedure Selection;                                                  // Detekce zasa�en� objekt�
var
  buffer: array [0..511] of GLuint;                                   // Deklarace selection bufferu
  hits: GLint;                                                        // Po�et zasa�en�ch objekt�
  viewport: array [0..3] of GLint;                                    // Velikost viewportu. [0] = x, [1] = y, [2] = v��ka, [3] = ���ka
  choose, depth, loop: integer;                                       // Jm�no objektu, hloubka, cyklus
begin
  if game then exit;                                                  // Konec hry? // Nen� d�vod testovat na z�sah
  PlaySound('data/shot.wav',0,SND_ASYNC);                             // P�ehraje zvuk v�st�elu
  glGetIntegerv(GL_VIEWPORT,@viewport);                               // Nastav� pole podle velikosti a lokace sc�ny relativn� k oknu
  glSelectBuffer(512,@buffer);                                        // P�ik�e OpenGL, aby pro selekci objekt� pou�ilo pole buffer
  glRenderMode(GL_SELECT);                                            // P�eveden� OpenGL do selection m�du
  glInitNames;                                                        // Inicializace name stacku
  glPushName(0);                                                      // Vlo�� 0 (nejm�n� jedna polo�ka) na stack
  glMatrixMode(GL_PROJECTION);                                        // Zvol� projek�n� matici
  glPushMatrix;                                                       // Ulo�en� projek�n� matice
  glLoadIdentity;                                                     // Reset matice
  gluPickMatrix(mouse_x,viewport[3] - mouse_y,1.0,1.0,@viewport);     // Vytvo�en� matice, kter� zv�t�� malou ��st obrazovky okolo kurzoru my�i
  gluPerspective(45.0,(viewport[2] - viewport[0]) / (viewport[3] - viewport[1]),0.1,100.0); // Aplikov�n� perspektivn� matice
  glMatrixMode(GL_MODELVIEW);                                         // Modelview matice
  DrawTargets;                                                        // Renderuje objekty do selection bufferu
  glMatrixMode(GL_PROJECTION);                                        // Projek�n� matice
  glPopMatrix;                                                        // Obnoven� projek�n� matice
  glMatrixMode(GL_MODELVIEW);                                         // Modelview matice
  hits := glRenderMode(GL_RENDER);                                    // P�epnut� do renderovac�ho m�du, ulo�en� po�tu objekt� pod kurzorem
  if hits > 0 then                                                    // Bylo v�ce ne� nula z�sah�?
    begin
    choose := buffer[3];                                              // Ulo�� jm�no prvn�ho objektu
    depth := buffer[1];                                               // Ulo�� jeho hloubku
    for loop := 1 to hits - 1 do                                      // Proch�z� v�echny detekovan� z�sahy
      if buffer[loop*4+1] < depth then                                // Je tento objekt bl�e ne� n�kter� z p�edchoz�ch?
        begin
        choose := buffer[loop*4+3];						                        // Ulo�� jm�no bli���ho objektu
				depth := buffer[loop*4+1];                                    // Ulo�� jeho hloubku
        end;
    if not obj[choose].hit then                                       // Nebyl je�t� objekt zasa�en?
      begin
      obj[choose].hit := true;                                        // Ozna�� ho jako zasa�en�
      Inc(score);                                                     // Zv��� celkov� sk�re
      Inc(kills);                                                     // Zv��� po�et z�sah� v levelu
      if kills > (level * 5) then                                     // �as pro dal�� level?
        begin
        miss := 0;                                                    // Nulov�n� nezasa�en�ch objekt�
        kills := 0;                                                   // Nulov�n� zasa�en�ch objekt� v tomto levelu
        Inc(level);                                                   // Posun na dal�� level
        if level > 30 then level := 30;                               // Posledn� level? // Nastaven� levelu na posledn�
        end;
      end;
    end;
end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohyb� ve sc�n� a stisk kl�ves
var
  loop: integer;                                                      // Cyklus
begin
  if g_keys.keyDown[VK_ESCAPE] then                                   // Kl�vesa ESC?
    TerminateApplication(g_window^);                                  // Ukon�en� programu
  if g_keys.keyDown[Ord(' ')] and game then                           // Mezern�k na konci hry?
    begin
    for loop := 0 to 29 do                                            // Proch�z� v�echny objekty
      InitObject(loop);                                               // Jejich inicializace
    game := false;                                                    // Je�t� nen� konec hry
    score := 0;                                                       // Nulov� sk�re
    level := 1;                                                       // Prvn� level
    kills := 0;                                                       // Nula zasa�en�ch objekt�
    miss := 0;                                                        // Nula nezasa�en�ch objekt�
    end;
  if g_keys.keyDown[VK_F1] then                                       // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                      // P�epnut� fullscreen/okno
  roll := roll - milliseconds*0.00005;                                // Mraky pluj� a zem� se pohybuje
  for loop := 0 to level - 1 do                                       // Aktualizace v�ech viditeln�ch objekt�
    begin
    if obj[loop].rot = 1 then                                         // Rotace po sm�ru hodinov�ch ru�i�ek?
			obj[loop].spin := obj[loop].spin - 0.2*(loop+milliseconds);
		if obj[loop].rot = 2 then                                         // Rotace proti sm�ru hodinov�ch ru�i�ek?
			obj[loop].spin := obj[loop].spin + 0.2*(loop+milliseconds);
		if obj[loop].dir = 1 then                                         // Pohyb doprava?
			obj[loop].x := obj[loop].x + 0.012*milliseconds;
		if obj[loop].dir = 0 then                                         // Pohyb doleva?
			obj[loop].x := obj[loop].x - 0.012*milliseconds;
		if obj[loop].dir = 2 then                                         // Pohyb nahoru?
			obj[loop].y := obj[loop].y + 0.012*milliseconds;
		if obj[loop].dir = 3 then                                         // Pohyb dol�?
			obj[loop].y := obj[loop].y - 0.0025*milliseconds;
		// Objekt vylet�l vlevo ze sc�ny, pohybuje se vlevo a je�t� nebyl zasa�en
		if ((obj[loop].x < (obj[loop].distance - 15.0) / 2.0) and (obj[loop].dir = 0) and (not obj[loop].hit)) then
		  begin
			Inc(miss);											                                // Zv��en� po�tu nezasa�en�ch objekt�
			obj[loop].hit := true;								                          // Odstran�n� objektu (zaji��uje animaci exploze a reinicializaci)
		  end;
		// Objekt vylet�l vpravo ze sc�ny, pohybuje se vpravo a je�t� nebyl zasa�en
		if ((obj[loop].x > -(obj[loop].distance - 15.0) / 2.0) and (obj[loop].dir = 1) and (not obj[loop].hit)) then
		  begin
			Inc(miss);											                                // Zv��en� po�tu nezasa�en�ch objekt�
			obj[loop].hit := true;								                          // Odstran�n� objektu (zaji��uje animaci exploze a reinicializaci)
		  end;
		// Objekt narazil do zem�, pohybuje se dol� a je�t� nebyl zasa�en
		if ((obj[loop].y < -2.0) and (obj[loop].dir = 3) and (not obj[loop].hit)) then
		  begin
			Inc(miss);											                                // Zv��en� po�tu nezasa�en�ch objekt�
			obj[loop].hit := true;								                          // Odstran�n� objektu (zaji��uje animaci exploze a reinicializaci)
		  end;
    // Objekt je pod mraky a sm��uje vzh�ru
		if ((obj[loop].y > 4.5) and (obj[loop].dir = 2)) then
			obj[loop].dir := 3;                                             // Zm�na sm�ru na p�d
    end; 
end;

procedure Draw;                                                       // Vykreslen� sc�ny
var
  window: TRect;                                                      // Prom�nn� obd�ln�ku
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity;	                                                    // Reset matice
  glPushMatrix;                                                       // Ulo�� matici
	glBindTexture(GL_TEXTURE_2D,textures[7].texID);                     // Textura mrak�
	glBegin(GL_QUADS);                                                  // Kreslen� obd�ln�k�
		glTexCoord2f(1.0,roll/1.5+1.0); glVertex3f( 28.0,+7.0,-50.0);
		glTexCoord2f(0.0,roll/1.5+1.0); glVertex3f(-28.0,+7.0,-50.0);
		glTexCoord2f(0.0,roll/1.5+0.0); glVertex3f(-28.0,-3.0,-50.0);
		glTexCoord2f(1.0,roll/1.5+0.0); glVertex3f( 28.0,-3.0,-50.0);
		glTexCoord2f(1.5,roll+1.0); glVertex3f( 28.0,+7.0,-50.0);
		glTexCoord2f(0.5,roll+1.0); glVertex3f(-28.0,+7.0,-50.0);
		glTexCoord2f(0.5,roll+0.0); glVertex3f(-28.0,-3.0,-50.0);
		glTexCoord2f(1.5,roll+0.0); glVertex3f( 28.0,-3.0,-50.0);
		glTexCoord2f(1.0,roll/1.5+1.0); glVertex3f( 28.0,+7.0,0.0);
		glTexCoord2f(0.0,roll/1.5+1.0); glVertex3f(-28.0,+7.0,0.0);
		glTexCoord2f(0.0,roll/1.5+0.0); glVertex3f(-28.0,+7.0,-50.0);
		glTexCoord2f(1.0,roll/1.5+0.0); glVertex3f( 28.0,+7.0,-50.0);
		glTexCoord2f(1.5,roll+1.0); glVertex3f( 28.0,+7.0,0.0);
		glTexCoord2f(0.5,roll+1.0); glVertex3f(-28.0,+7.0,0.0);
		glTexCoord2f(0.5,roll+0.0); glVertex3f(-28.0,+7.0,-50.0);
		glTexCoord2f(1.5,roll+0.0); glVertex3f( 28.0,+7.0,-50.0);
	glEnd;                                                              // Konec kreslen�
	glBindTexture(GL_TEXTURE_2D,textures[6].texID);			                // Textura zem�
	glBegin(GL_QUADS);											                            // Kreslen� obd�ln�k�
		glTexCoord2f(7.0,4.0-roll); glVertex3f( 27.0,-3.0,-50.0);
		glTexCoord2f(0.0,4.0-roll); glVertex3f(-27.0,-3.0,-50.0);
		glTexCoord2f(0.0,0.0-roll); glVertex3f(-27.0,-3.0,0.0);
		glTexCoord2f(7.0,0.0-roll); glVertex3f( 27.0,-3.0,0.0);
	glEnd; 													                                    // Konec kreslen�
	DrawTargets;												                                // Sest�elovan� objekty
  glPopMatrix;                                                        // Obnoven� matice
	GetClientRect(g_window.hWnd,window);                                // Grabov�n� rozm�r� okna
	glMatrixMode(GL_PROJECTION);                                        // Projek�n� matice
	glPushMatrix;                                                       // Ulo�� projek�n� matici
	glLoadIdentity;                                                     // Reset projek�n� matice
	glOrtho(0,window.right,0,window.bottom,-1,1);                       // Nastaven� pravo�hl� sc�ny
	glMatrixMode(GL_MODELVIEW);                                         // Zvol� matici modelview
	glTranslated(mouse_x,window.bottom-mouse_y,0.0);                    // Posun na pozici kurzoru
	Objekt(16,16,8);                                                    // Vykresl� kurzor my�i
	glPrint(240,450,'NeHe Productions');                                // Logo
	glPrint(10,10,Format('Level: %d',[level]));                         // Level
	glPrint(250,10,Format('Score: %d',[score]));                        // Sk�re
	if miss > 9 then                                                    // Nestrefil hr�� v�ce ne� dev�t objekt�?
	  begin
		miss := 9;                                                        // Limit je dev�t
		game := true;                                                     // Konec hry
	  end;
	if game then                                                        // Konec hry?
		glPrint(490,10,'GAME OVER')                                       // Vyp�e konec hry
	else
		glPrint(490,10,Format('Morale: %d/10',[10-miss]));                // Vyp�e po�et objekt�, kter� nemus� sest�elit
	glMatrixMode(GL_PROJECTION);                                        // Projek�n� matice
	glPopMatrix;                                                        // Obnoven� projek�n� matice
	glMatrixMode(GL_MODELVIEW);                                         // Modelview matice
	glFlush;                                                            // Vypr�zdn� OpenGL renderovac� pipeline
end;

function WindowProc(hWnd: HWND;                                       // Handle okna
                 uMsg: UINT;                                          // Zpr�va pro okno
                 wParam: WPARAM;                                      // Dopl�kov� informace
                 lParam: LPARAM):                                     // Dopl�kov� informace
                                  LRESULT; stdcall;
var
  window: ^GL_Window;
  creation: ^CREATESTRUCT;
begin
  if uMsg = WM_SYSCOMMAND then                                        // Syst�mov� p��kaz
      case wParam of                                                  // Typ syst�mov�ho p��kazu
        SC_SCREENSAVE,SC_MONITORPOWER:                                // Pokus o zapnut� �et�i�e obrazovky, Pokus o p�echod do �sporn�ho re�imu?
          begin
            Result := 0;                                              // Zabr�n� oboj�mu
            exit;
          end;
      end;
  window := Pointer(GetWindowLong(hWnd,GWL_USERDATA));
  case uMsg of                                                        // V�tven� podle p��choz� zpr�vy
    WM_CREATE:
      begin
      creation := Pointer(lParam);
      window := Pointer(creation.lpCreateParams);
      SetWindowLong(hWnd,GWL_USERDATA,Integer(window));
      Result := 0;
      end;
    WM_CLOSE:                                                         // Povel k ukon�en� programu
      begin
      TerminateApplication(window^);                                  // Po�le zpr�vu o ukon�en�
      Result := 0                                                     // N�vrat do hlavn�ho cyklu programu
      end;
    WM_SIZE:                                                          // Zm�na velikosti okna
      begin
      case wParam of
        SIZE_MINIMIZED:
          begin
          window.isVisible := false;
          Result := 0;
          end;
        SIZE_MAXIMIZED,
        SIZE_RESTORED:
          begin
          window.isVisible := true;
          ReshapeGL(LOWORD(lParam),HIWORD(lParam));                 // LoWord=���ka, HiWord=V��ka
          Result := 0;                                              // N�vrat do hlavn�ho cyklu programu
          end;
      end;
     // Result := 0;
      end;
    WM_KEYDOWN:                                                     // Stisk kl�vesy
      begin
      if (wParam >= 0) and (wParam <= 255) then
        begin
        window^.keys^.keyDown[wParam] := true;                      // Ozn�m� to programu
        Result := 0;
        end;
      //Result := 0;                                                // N�vrat do hlavn�ho cyklu programu
      end;
    WM_KEYUP:                                                       // Uvoln�n� kl�vesy
      begin
      if (wParam >= 0) and (wParam <= 255) then
        begin
    	  window^.keys^.keyDown[wParam] := false;                     // Ozn�m� to programu
        Result := 0;                                                // N�vrat do hlavn�ho cyklu programu
        end;
      //exit;
      end;
    WM_TOGGLEFULLSCREEN:
      begin
      g_createFullScreen := not g_createFullScreen;
      PostMessage(hWnd,WM_QUIT,0,0);
      Result := 0;
      end;
    WM_LBUTTONDOWN:                                                 // Stisknut� lev�ho tla��tka my�i
      begin
      mouse_x := LOWORD(lParam);
      mouse_y := HIWORD(lParam);
      Selection;
      Result := 0;
      end;
    WM_MOUSEMOVE:                                                   // Pohyb my�i
      begin
      mouse_x := LOWORD(lParam);
      mouse_y := HIWORD(lParam);
      Result := 0;
      end;
    else
      // P�ed�n� ostatn�ch zpr�v syst�mu
      begin
      	Result := DefWindowProc(hWnd,uMsg,wParam,lParam);
      end;
    end;
end;

function RegisterWindowClass(application: Application): boolean;
var
  windowClass: WNDCLASSEX;
begin
  ZeroMemory(@windowClass,Sizeof(windowClass));
  with windowClass do
    begin
    cbSize := Sizeof(windowClass);
    style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC;                  // P�ekreslen� p�i zm�n� velikosti a vlastn� DC
    lpfnWndProc := @WindowProc;                                     // Definuje proceduru okna
    hInstance := application.hInstance;                             // Instance
    hbrBackground := COLOR_APPWORKSPACE;                            // Pozad� nen� nutn�
    hCursor := LoadCursor(0,IDC_ARROW);                             // Standardn� kurzor my�i
    lpszClassName := PChar(application.className);                  // Jm�no t��dy okna
    end;
  if RegisterClassEx(windowClass) = 0 then                          // Registruje t��du okna
    begin
    MessageBox(HWND_DESKTOP,'RegisterClassEx Failed!','Error',MB_OK or MB_ICONEXCLAMATION);
    Result := false;                                                // P�i chyb� vr�t� false
    exit;
    end;
  Result := true;
end;

function WinMain(hInstance: HINST;                                  // Instance
		 hPrevInstance: HINST;                                          // P�edchoz� instance
		 lpCmdLine: PChar;                                              // Parametry p��kazov� ��dky
		 nCmdShow: integer):                                            // Stav zobrazen� okna
                        integer; stdcall;
var
  app: Application;
  window: GL_Window;
  key: Keys;
  isMessagePumpActive: boolean;
  msg: TMsg;                                                        // Struktura zpr�v syst�mu
  tickCount: DWORD;
begin
  app.className := 'OpenGL';
  app.hInstance := hInstance;
  ZeroMemory(@window,Sizeof(window));
  with window do
    begin
    keys := @key;
    init.application := @app;
    init.title := 'NeHe''s Picking Tutorial';
    init.width := 640;
    init.height := 480;
    init.bitsPerPixel := 16;
    init.isFullScreen := true;
    end;
  ZeroMemory(@key,Sizeof(key));
  // Dotaz na u�ivatele pro fullscreen/okno
  if MessageBox(HWND_DESKTOP,'Would You Like To Run In FullScreen Mode?','Start FullScreen',
                MB_YESNO or MB_ICONQUESTION) = IDNO then
    window.init.isFullScreen := false;                                 // B�h v okn�
  if not RegisterWindowClass(app) then
    begin
    MessageBox(HWND_DESKTOP,'Error Registering Window Class!','Error',MB_OK or MB_ICONEXCLAMATION);
    Result := -1;
    exit;
    end;
  g_isProgramLooping := true;
  g_createFullScreen := window.init.isFullScreen;
  while g_isProgramLooping do
    begin
    window.init.isFullScreen := g_createFullScreen;
    if CreateWindowGL(window) then
      begin
      if not Initialize(@window,@key) then
        TerminateApplication(window)
        else
        begin
        isMessagePumpActive := true;
        while isMessagePumpActive do
          if PeekMessage(msg,0,0,0,PM_REMOVE) then                  // P�i�la zpr�va?
            if msg.message <> WM_QUIT then                          // Obdr�eli jsme zpr�vu pro ukon�en�?
              DispatchMessage(msg)                                
              else
              isMessagePumpActive := false                          // Konec programu
            else
            if not window.isVisible then
              WaitMessage
              else
              begin
              tickCount := GetTickCount;
              Update(tickCount - window.lastTickCount);
              window.lastTickCount := tickCount;
              Draw;
              SwapBuffers(window.hDc);
              end;
        end;
      Deinitialize;
      DestroyWindowGL(window);
      end
      else
      begin
      MessageBox(HWND_DESKTOP,'Error Creating OpenGL Window','Error',MB_OK or MB_ICONEXCLAMATION);
      g_isProgramLooping := false;
      end;
    end;
  UnregisterClass(PChar(app.className),app.hInstance);
  Result := 0;
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );                  // Start programu
end.

