program lesson22a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL,sysutils,
  GLaux;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

type    // Definice roz���en�ch mo�nost� OpenGL
  PFNGLMULTITEXCOORD1FARBPROC = procedure(target: GLenum; s,t : GLfloat); stdcall;
  PFNGLMULTITEXCOORD2FARBPROC = procedure(target: GLenum; s,t : GLfloat); stdcall;
  PFNGLMULTITEXCOORD3FARBPROC = procedure(target: GLenum; s,t : GLfloat); stdcall;
  PFNGLMULTITEXCOORD4FARBPROC = procedure(target: GLenum; s,t : GLfloat); stdcall;
  PFNGLACTIVETEXTUREARBPROC = procedure(target: GLenum); stdcall;
  PFNGLCLIENTACTIVETEXTUREARBPROC = procedure(target: GLenum); stdcall;

const
  MAX_EMBOSS = 0.008;               // Maxim�ln� posunut� efektem
  __ARB_ENABLE = true;              // Pou�ito pro vy�azen� multitexturingu
  MAX_EXTENSION_SPACE = 10240;      // M�sto pro �et�zce s OpenGL roz���en�mi
  MAX_EXTENSION_LENGTH = 256;       // Maximum znak� v jednom �et�zci s roz���en�m
  EXT_INFO = false;                 // true, pokud chcete p�i startu vid�t podporovan� roz���en� OpenGL
  GL_MAX_TEXTURE_UNITS_ARB = $84E2; // Konstanty roz���en�, kter� budeme pot�ebovat
  GL_RGB8 = $8051;
  GL_RGBA8 = $8058;
  GL_TEXTURE0_ARB = $84C0;
  GL_COMBINE_EXT = $8570;
  GL_COMBINE_RGB_EXT = $8571;
  GL_TEXTURE1_ARB = $84C1;

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  xrot: GLFloat;                    // X rotace
  yrot: GLFloat;                    // Y rotace
  xspeed: GLfloat;                  // Rychlost x rotace
  yspeed: GLfloat;                  // Rychlost y rotace
  z: GLfloat = -5.0;                // Hloubka v obrazovce
  filter: GLuint = 1;               // Jak� filtr pou��t
  texture: array [0..2] of GLuint;  // M�sto pro t�i textury
  bump: array [0..2] of GLuint;     // Na�e bumpmapy
  invbump: array [0..2] of GLuint;  // Invertovan� bumpmapy
  glLogo: GLuint;                   // M�sto pro OpenGL Logo
  multiLogo: GLuint;                // M�sto pro logo s multitexturingem
  LightAmbient: array [0..2] of GLfloat = ( 0.2, 0.2, 0.2);           // Barva ambientn�ho sv�tla je 20% b�l�
  LightDiffuse: array [0..2] of GLfloat = ( 1.0, 1.0, 1.0);           // Dif�zn� sv�tlo je b�l�
  LightPosition: array [0..2] of GLfloat = ( 0.0, 0.0, 2.0);          // Pozice je n�kde uprost�ed sc�ny
  Gray: array [0..3] of GLfloat = ( 0.5, 0.5, 0.5, 1.0 );             // Barva okraje textury
  emboss: bool = false;                                               // Jenom Emboss, ��dn� z�kladn� textura
  bumps: bool = true;                                                 // Pou��vat bumpmapping?
  Multitexturesupported: bool = false;                                // Indik�tor podpory multitexturingu
  UseMultitexture: bool = true;                                       // Pou�it multitexturing?
  maxTexelUnits: GLint = 1;                                           // Po�et texturovac�ch jednotek - nejm�n� 1
  glMultiTexCoord1fARB: PFNGLMULTITEXCOORD1FARBPROC = nil;
  glMultiTexCoord2fARB: PFNGLMULTITEXCOORD2FARBPROC = nil;
  glMultiTexCoord3fARB: PFNGLMULTITEXCOORD3FARBPROC = nil;
  glMultiTexCoord4fARB: PFNGLMULTITEXCOORD4FARBPROC = nil;
  glActiveTextureARB: PFNGLACTIVETEXTUREARBPROC = nil;
  glClientActiveTextureARB: PFNGLCLIENTACTIVETEXTUREARBPROC = nil;
  data : Array [0..119] of GLfloat = (
		// P�edn� st�na
		0.0, 0.0,		-1.0, -1.0, +1.0,
		1.0, 0.0,		+1.0, -1.0, +1.0,
		1.0, 1.0,		+1.0, +1.0, +1.0,
		0.0, 1.0,		-1.0, +1.0, +1.0,
		// Zadn� st�na
		1.0, 0.0,		-1.0, -1.0, -1.0,
		1.0, 1.0,		-1.0, +1.0, -1.0,
		0.0, 1.0,		+1.0, +1.0, -1.0,
		0.0, 0.0,		+1.0, -1.0, -1.0,
		// Horn� st�na
		0.0, 1.0,		-1.0, +1.0, -1.0,
		0.0, 0.0,		-1.0, +1.0, +1.0,
		1.0, 0.0,		+1.0, +1.0, +1.0,
		1.0, 1.0,		+1.0, +1.0, -1.0,
		// Doln� st�na
		1.0, 1.0,		-1.0, -1.0, -1.0,
		0.0, 1.0,		+1.0, -1.0, -1.0,
		0.0, 0.0,		+1.0, -1.0, +1.0,
		1.0, 0.0,		-1.0, -1.0, +1.0,
		// Prav� st�na
		1.0, 0.0,		+1.0, -1.0, -1.0,
		1.0, 1.0,		+1.0, +1.0, -1.0,
		0.0, 1.0,		+1.0, +1.0, +1.0,
		0.0, 0.0,		+1.0, -1.0, +1.0,
		// Lev� st�na
		0.0, 0.0,		-1.0, -1.0, -1.0,
		1.0, 0.0,		-1.0, -1.0,  1.0,
		1.0, 1.0,		-1.0,  1.0,  1.0,
		0.0, 1.0,		-1.0,  1.0, -1.0);

function isInString(strings: pchar; searchstring: pchar): boolean;
begin
  if Pos(searchstring,strings) <> 0 then Result := true       // Hled�n� cel�ho �et�zce jm�na roz���en�
    else Result := false;                                     // Sm�la, nic jsme nena�li!
end;

function initMultitexture: boolean;
var extensions: pchar;
begin
  extensions := glGetString(GL_EXTENSIONS);                                     // Z�sk�n� �et�zce s roz���en�mi
  if EXT_INFO then MessageBox(h_Wnd,extensions,'OpenGL extensions',MB_OK or MB_ICONINFORMATION);
  if (isInString(extensions,'GL_ARB_multitexture')) and __ARB_ENABLE            // Je multitexturing podporov�n? a P��znak pro povolen� multitexturingu
      and isInString(extensions,'GL_EXT_texture_env_combine') then              // Je podporov�no texture-environment-combining?
    begin
    glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB,@maxTexelUnits);
    glMultiTexCoord1fARB := wglGetProcAddress('glMultiTexCoord1fARB');
    glMultiTexCoord2fARB := wglGetProcAddress('glMultiTexCoord2fARB');
    glMultiTexCoord3fARB := wglGetProcAddress('glMultiTexCoord3fARB');
    glMultiTexCoord4fARB := wglGetProcAddress('glMultiTexCoord4fARB');
    glActiveTextureARB := wglGetProcAddress('glActiveTextureARB');
    glClientActiveTextureARB := wglGetProcAddress('glClientActiveTextureARB');
    if EXT_INFO then MessageBox(h_Wnd,'The GL_ARB_multitexture extension will be used.','Feature supported!',MB_OK or MB_ICONINFORMATION);
    Result := true;
    exit;
    end;
  UseMultitexture := false;                                                     // Nem��eme to pou��vat, pokud to nen� podporov�no!
  Result := false;
end;

procedure initLights;
begin
  glLightfv(GL_LIGHT1,GL_AMBIENT,@LightAmbient);          // Na�ten� informace o sv�tlech do GL_LIGHT1
  glLightfv(GL_LIGHT1,GL_DIFFUSE,@LightDiffuse);
  glLightfv(GL_LIGHT1,GL_POSITION,@LightPosition);
  glEnable(GL_LIGHT1);
end;

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

function LoadGLTextures: Bool;                              // Loading bitmapy a konverze na texturu
type Tznak = array [0..255] of byte;                        // Typ pole znak�
var TextureImage: array [0..0] of PTAUX_RGBImageRec;        // Ukl�d� bitmapu
    Status: Bool;                                           // Indikuje chyby
    i: integer;                                             // Cyklus
    bumpmap, alpha, alpha1: ^Tznak;                         // Pomocn� prom�nn� pro v�po�et bumpmapy a log
begin
  Status := false;
  ZeroMemory(@TextureImage,sizeof(TextureImage));           // Vynuluje pam�
  TextureImage[0] := LoadBMP('Data/Base.bmp');              // Nahraje bitmapu
  if Assigned(TextureImage[0]) then                         // V�e je bez probl�m�?
    begin
    Status := true;                                         // V�e je bez probl�m�
    glGenTextures(3,Texture[0]);                            // Generuje t�i textury
    // Vytvo�en� neline�rn� filtrovan� textury
    glBindTexture(GL_TEXTURE_2D,texture[0]);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGB8,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[0].data);
    // Vytvo�en� line�rn� filtrovan� textury
    glBindTexture(GL_TEXTURE_2D,texture[1]);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGB8,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[0].data);
    // Vytvo�en� mipmapovan� textury
    glBindTexture(GL_TEXTURE_2D,texture[2]);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
    gluBuild2DMipmaps(GL_TEXTURE_2D,GL_RGB8,TextureImage[0].sizeX,TextureImage[0].sizeY,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[0].data);
    end
    else Status := false;
  TextureImage[0] := LoadBMP('Data/Bump.bmp');              // Loading bumpmap
  if Assigned(TextureImage[0]) then
    begin
    glPixelTransferf(GL_RED_SCALE,0.5);                     // Sn�en� intenzity RGB na 50% - polovi�n� intenzita
    glPixelTransferf(GL_GREEN_SCALE,0.5);
    glPixelTransferf(GL_BLUE_SCALE,0.5);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP);        // Bez wrappingu (zalamov�n�)
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP);
    glTexParameterfv(GL_TEXTURE_2D,GL_TEXTURE_BORDER_COLOR,@gray);    // Barva okraje textury
    glGenTextures(3,bump[0]);                                         // Vytvo�� t�i textury
    // Vytvo�en� neline�rn� filtrovan� textury
    glBindTexture(GL_TEXTURE_2D,bump[0]);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGB8,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[0].data);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
    // Vytvo�en� line�rn� filtrovan� textury
    glBindTexture(GL_TEXTURE_2D,bump[1]);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGB8,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[0].data);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    // Vytvo�en� mipmapovan� textury
    glBindTexture(GL_TEXTURE_2D,bump[2]);
    gluBuild2DMipmaps(GL_TEXTURE_2D,GL_RGB8,TextureImage[0].sizeX,TextureImage[0].sizeY,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[0].data);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
    //**************************************************************************
    // Invertov�n� bumpmapy
    // V Delphi to bohu�el nejde tak jednodu�e jako v C++. P�eklada� ohl�s� chybu.
    // Dle n�pov�dy je toto spr�vn� opis konstrukce z C++. Douf�m :-)))
    // U vytv��en� textur nezapome�te p�edat pomocnou prom�nnou (bumpmap) m�sto
    // dat z TextureImage[0].data!!!!!!!
    //**************************************************************************
    GetMem(bumpmap,3*TextureImage[0].sizeX*TextureImage[0].sizeY);              // Alokace pam�ti pro pomocnou prom�nnou
    bumpmap := TextureImage[0].data;                                            // P�ed�n� dat do pomocn� prom�nn�
    for i:= 0 to 3*TextureImage[0].sizeX*TextureImage[0].sizeY-1 do             // Vlastn� invertov�n�
      begin
      bumpmap[i] := 255 - bumpmap[i];
      end;
    glGenTextures(3,invbump[0]);                                                // Vytvo�� t�i textury
    // Vytvo�en� neline�rn� filtrovan� textury
    glBindTexture(GL_TEXTURE_2D,invbump[0]);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGB8,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,bumpmap);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
    // Vytvo�en� line�rn� filtrovan� textury
    glBindTexture(GL_TEXTURE_2D,invbump[1]);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGB8,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,bumpmap);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    // Vytvo�en� mipmapovan� textury
    glBindTexture(GL_TEXTURE_2D,invbump[2]);
    gluBuild2DMipmaps(GL_TEXTURE_2D,GL_RGB8,TextureImage[0].sizeX,TextureImage[0].sizeY,GL_RGB,GL_UNSIGNED_BYTE,bumpmap);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
    glPixelTransferf(GL_RED_SCALE,1.0);                     // Vr�cen� intenzity RGB zp�t na 100%
    glPixelTransferf(GL_GREEN_SCALE,1.0);
    glPixelTransferf(GL_BLUE_SCALE,1.0);
    bumpmap := nil;
    FreeMem(bumpmap);                                       // Uvoln�n� pam�ti po pomocn� prom�nn�
    end
    else Status := false;
  TextureImage[0] := LoadBMP('Data/OpenGL_ALPHA.bmp');      // Na�te bitmapy log
  if Assigned(TextureImage[0]) then
    begin
    GetMem(alpha,4*TextureImage[0].sizeX*TextureImage[0].sizeY);                // Alokuje pam� pro RGBA8-Texturu
    //**************************************************************************
    // Alfa kan�l loga OpenGL
    // V Delphi to bohu�el nejde tak jednodu�e jako v C++. P�eklada� ohl�s� chybu.
    // Dle n�pov�dy je toto spr�vn� opis konstrukce z C++. Douf�m :-)))
    //**************************************************************************
    GetMem(alpha1,sizeof(TextureImage[0].data));                                // Alokace pam�ti pro pomocnou prom�nnou
    alpha1 := TextureImage[0].data;                                             // P�ed�n� dat do pomocn� prom�nn�
    for i:=0 to TextureImage[0].sizeX*TextureImage[0].sizeY-1 do
      alpha[4*i+3] := alpha1[i*3];                                              // Vezme pouze �ervenou barvu jako alpha kan�l
    alpha1 := nil;
    FreeMem(alpha1);                                                            // Uvoln�n� pam�ti po pomocn� prom�nn�
    TextureImage[0] := LoadBMP('Data/OpenGL.bmp');
    if not Assigned(TextureImage[0]) then Status := false;
    GetMem(alpha1,sizeof(TextureImage[0].data));                                // Alokace pam�ti pro pomocnou prom�nnou
    alpha1 := TextureImage[0].data;                                             // P�ed�n� dat do pomocn� prom�nn�
    for i:=0 to TextureImage[0].sizeX*TextureImage[0].sizeY-1 do
      begin
      alpha[4*i] := alpha1[i*3];                                                // R
      alpha[4*i+1] := alpha1[i*3+1];                                            // G
      alpha[4*i+2] := alpha1[i*3+2];                                            // B
      end;
    glGenTextures(1,glLogo);                                                    // Vytvo�� jednu texturu
    // Vytvo�� line�rn� filtrovanou RGBA8-Texturu
    glBindTexture(GL_TEXTURE_2D,glLogo);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA8,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGBA,GL_UNSIGNED_BYTE,alpha);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    alpha := nil;
    alpha1 := nil;
    FreeMem(alpha);                                                             // Uvoln� alokovanou pam�
    FreeMem(alpha1);                                                            // Uvoln�n� pam�ti po pomocn� prom�nn�
    end
    else Status := false;
  TextureImage[0] := LoadBMP('Data/multi_on_alpha.bmp');
  if Assigned(TextureImage[0]) then
    begin
    GetMem(alpha,4*TextureImage[0].sizeX*TextureImage[0].sizeY);                // Alokuje pam� pro RGBA8-Texturu
    //**************************************************************************
    // Alfa kan�l loga multitexturingu
    // V Delphi to bohu�el nejde tak jednodu�e jako v C++. P�eklada� ohl�s� chybu.
    // Dle n�pov�dy je toto spr�vn� opis konstrukce z C++. Douf�m :-)))
    //**************************************************************************
    GetMem(alpha1,sizeof(TextureImage[0].data));                                // Alokace pam�ti pro pomocnou prom�nnou
    alpha1 := TextureImage[0].data;                                             // P�ed�n� dat do pomocn� prom�nn�
    for i:=0 to TextureImage[0].sizeX*TextureImage[0].sizeY-1 do
      alpha[4*i+3] := alpha1[i*3];                                              // Vezme pouze �ervenou barvu jako alpha kan�l
    alpha1 := nil;
    FreeMem(alpha1);                                                            // Uvoln�n� pam�ti po pomocn� prom�nn�
    TextureImage[0] := LoadBMP('Data/multi_on.bmp');
    if not Assigned(TextureImage[0]) then Status := false;
    GetMem(alpha1,sizeof(TextureImage[0].data));                                // Alokace pam�ti pro pomocnou prom�nnou
    alpha1 := TextureImage[0].data;                                             // P�ed�n� dat do pomocn� prom�nn�
    for i:=0 to TextureImage[0].sizeX*TextureImage[0].sizeY-1 do
      begin
      alpha[4*i] := alpha1[i*3];                                                // R
      alpha[4*i+1] := alpha1[i*3+1];                                            // G
      alpha[4*i+2] := alpha1[i*3+2];                                            // B
      end;
    glGenTextures(1,multiLogo);                                                 // Vytvo�� jednu texturu
    // Vytvo�� line�rn� filtrovanou RGBA8-Texturu
    glBindTexture(GL_TEXTURE_2D,multiLogo);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA8,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGBA,GL_UNSIGNED_BYTE,alpha);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    alpha := nil;
    alpha1 := nil;
    FreeMem(alpha);                                                             // Uvoln� alokovanou pam�
    FreeMem(alpha1);                                                            // Uvoln�n� pam�ti po pomocn� prom�nn�
    end
    else Status := false;
  Result := Status;                                         // Ozn�m� p��padn� chyby
end;

procedure doCube;                                           // Kostka
var i: integer;
begin
  glBegin(GL_QUADS);
    // P�edn� st�na
    glNormal3f( 0.0, 0.0, +1.0);
    for i := 0 to 3 do
      begin
      glTexCoord2f(data[5*i],data[5*i+1]);
      glVertex3f(data[5*i+2],data[5*i+3],data[5*i+4]);
      end;
    // Zadn� st�na
    glNormal3f( 0.0, 0.0,-1.0);
    for i := 4 to 7 do
      begin
      glTexCoord2f(data[5*i],data[5*i+1]);
      glVertex3f(data[5*i+2],data[5*i+3],data[5*i+4]);
      end;
    // Horn� st�na
    glNormal3f( 0.0, 1.0, 0.0);
    for i := 8 to 11 do
      begin
      glTexCoord2f(data[5*i],data[5*i+1]);
      glVertex3f(data[5*i+2],data[5*i+3],data[5*i+4]);
      end;
    // Spodn� st�na
    glNormal3f( 0.0,-1.0, 0.0);
    for i := 12 to 15 do
      begin
      glTexCoord2f(data[5*i],data[5*i+1]);
      glVertex3f(data[5*i+2],data[5*i+3],data[5*i+4]);
      end;
    // Prav� st�na
    glNormal3f( 1.0, 0.0, 0.0);
    for i := 16 to 19 do
      begin
      glTexCoord2f(data[5*i],data[5*i+1]);
      glVertex3f(data[5*i+2],data[5*i+3],data[5*i+4]);
      end;
    // Lev� st�na
    glNormal3f(-1.0, 0.0, 0.0);
    for i := 20 to 23 do
      begin
      glTexCoord2f(data[5*i],data[5*i+1]);
      glVertex3f(data[5*i+2],data[5*i+3],data[5*i+4]);
      end;
  glEnd();
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
  Multitexturesupported := initMultitexture;        // Zjist� podporu multitexturingu
  if not LoadGLTextures then                        // Nahraje texturu
    begin
    Result := false;
    exit;
    end;
  glEnable(GL_TEXTURE_2D);                          // Zapne mapov�n� textur
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glClearColor(0.0, 0.0, 0.0, 0.5);	  	            // �ern� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glEnable(GL_DEPTH_TEST);			                    // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LEQUAL);				                    // Typ hloubkov�ho testov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST); // Nejlep�� perspektivn� korekce
  initLights;                                       // Inicializace sv�tel
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;

procedure VMatMult(var M, v: array of GLfloat);
var res: array [0..2] of GLfloat;
begin
  res[0]:=M[ 0]*v[0]+M[ 1]*v[1]+M[ 2]*v[2]+M[ 3]*v[3];
  res[1]:=M[ 4]*v[0]+M[ 5]*v[1]+M[ 6]*v[2]+M[ 7]*v[3];
  res[2]:=M[ 8]*v[0]+M[ 9]*v[1]+M[10]*v[2]+M[11]*v[3];
  v[0]:=res[0];
  v[1]:=res[1];
  v[2]:=res[2];
  v[3]:=M[15];			                                // Homogenn� sou�adnice
end;

procedure SetUpBumps(var n, c, l, s, t: array of GLfloat);
var
  v: array [0..2] of GLfloat;                           // Vertex z aktu�ln� pozice ke sv�tlu
  lenQ: GLfloat;                                        // Pou�ito p�i normalizaci
begin
  //****************************************************************************
  // Funkce nastav� posunut� textury
  // n : norm�la k plo�e, mus� m�t d�lku 1
  // c : n�jak� bod na povrchu
  // l : pozice sv�tla
  // s : sm�r texturovac�ch sou�adnic s (mus� b�t normalizov�n!)
  // t : sm�r texturovac�ch sou�adnic t (mus� b�t normalizov�n!)
  //****************************************************************************
  // Spo��t�n� v z aktu�ln�ho vertexu c ke sv�tlu a jeho normalizace
  v[0]:=l[0]-c[0];
  v[1]:=l[1]-c[1];
  v[2]:=l[2]-c[2];
  lenQ:=sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
  v[0] := v[0] / lenQ;
  v[1] := v[1] / lenQ;
  v[2] := v[2] / lenQ;
  // Zohledn�n� v tak, abychom dostali texturovac� sou�adnice
  c[0]:=(s[0]*v[0]+s[1]*v[1]+s[2]*v[2])*MAX_EMBOSS;
  c[1]:=(t[0]*v[0]+t[1]*v[1]+t[2]*v[2])*MAX_EMBOSS;
end;

procedure doLogo;			// MUS� SE ZAVOLAT A� NAKONEC!!! Zobraz� dv� loga
begin
  glDepthFunc(GL_ALWAYS);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glDisable(GL_LIGHTING);
  glLoadIdentity();
  glBindTexture(GL_TEXTURE_2D,glLogo);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0,0.0);	glVertex3f(0.23, -0.4,-1.0);
    glTexCoord2f(1.0,0.0);	glVertex3f(0.53, -0.4,-1.0);
    glTexCoord2f(1.0,1.0);	glVertex3f(0.53, -0.25,-1.0);
    glTexCoord2f(0.0,1.0);	glVertex3f(0.23, -0.25,-1.0);
  glEnd();
  if (useMultitexture) then
  begin
    glBindTexture(GL_TEXTURE_2D,multiLogo);
    glBegin(GL_QUADS);
      glTexCoord2f(0.0,0.0);	glVertex3f(-0.53, -0.4,-1.0);
      glTexCoord2f(1.0,0.0);	glVertex3f(-0.33, -0.4,-1.0);
      glTexCoord2f(1.0,1.0);	glVertex3f(-0.33, -0.3,-1.0);
      glTexCoord2f(0.0,1.0);	glVertex3f(-0.53, -0.3,-1.0);
    glEnd();
  end;
  glDepthFunc(GL_LEQUAL);
end;

function doMesh1TexelUnits : boolean;
var
  c : array [0..3] of GLfloat;					                // Aktu�ln� vertex
  n : array [0..3] of GLfloat;					                // Normalizovan� norm�la dan�ho povrchu
  s : array [0..3] of GLfloat;					                // Sm�r texturovac�ch sou�adnic s, normalizov�no
  t : array [0..3] of GLfloat;					                // Sm�r texturovac�ch sou�adnic t, normalizov�no
  l : array [0..3] of GLfloat;										      // Pozice sv�tla, kter� bude transformov�na do prostoru objektu
  Minv : array [0..15] of GLfloat;									    // P�evr�cen� modelview matice
  i : integer;
begin
  c[3] := 1.0;
  n[3] := 1.0;
  s[3] := 1.0;
  t[3] := 1.0;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);	// Sma�e obrazovku a hloubkov� buffer
  // Sestaven� p�evr�cen� modelview matice; nahrad� funkce Push a Pop jednou funkc� glLoadIdentity()
  // Jednoduch� sestaven� t�m, �e v�echny transformace provedeme opa�n� a v opa�n�m po�ad�
  glLoadIdentity();
  glRotatef(-yrot,0.0,1.0,0.0);
  glRotatef(-xrot,1.0,0.0,0.0);
  glTranslatef(0.0,0.0,-z);
  glGetFloatv(GL_MODELVIEW_MATRIX,@Minv);
  glLoadIdentity();
  glTranslatef(0.0,0.0,z);
  glRotatef(xrot,1.0,0.0,0.0);
  glRotatef(yrot,0.0,1.0,0.0);
  // Transformace pozice sv�tla do sou�adnic objektu:
  l[0]:=LightPosition[0];
  l[1]:=LightPosition[1];
  l[2]:=LightPosition[2];
  l[3]:=1.0;                                            // Homogen� sou�adnice
  VMatMult(Minv,l);
  //****************************************************************************
  // Prvn� f�ze:
  //   Pou�it� bump textury
  //   Vypnut� blendingu
  //   Vypnut� sv�tel
  //   Pou�it� texturovac�ch sou�adnic bez posunut�
  //   Vytvo�en� geometrie
  //****************************************************************************
  glBindTexture(GL_TEXTURE_2D, bump[filter]);
  glDisable(GL_BLEND);
  glDisable(GL_LIGHTING);
  doCube();
  //****************************************************************************
  // Druh� f�ze:
  //   Pou�it� p�evr�cen� bumpmapy
  //   Povolen� blendingu GL_ONE, GL_ONE
  //   Ponech� vypnut� sv�tla
  //   Pou�it� posunut�ch texturovac�ch sou�adnic (P�ed ka�dou st�nou krychle mus�me zavolat funkci SetUpBumps())
  //   Vytvo�en� geometrie
  //****************************************************************************
  glBindTexture(GL_TEXTURE_2D,invbump[filter]);
  glBlendFunc(GL_ONE,GL_ONE);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_BLEND);
  glBegin(GL_QUADS);
    // P�edn� st�na
    n[0]:=0.0;		n[1]:=0.0;		n[2]:=1.0;
    s[0]:=1.0;		s[1]:=0.0;		s[2]:=0.0;
    t[0]:=0.0;		t[1]:=1.0;		t[2]:=0.0;
    for i := 0 to 3 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glTexCoord2f(data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Zadn� st�na
    n[0]:=0.0;		n[1]:=0.0;		n[2]:=-1.0;
    s[0]:=-1.0;		s[1]:=0.0;		s[2]:=0.0;
    t[0]:=0.0;		t[1]:=1.0;		t[2]:=0.0;
    for i := 4 to 7 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glTexCoord2f(data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Horn� st�na
    n[0]:=0.0;		n[1]:=1.0;		n[2]:=0.0;
    s[0]:=1.0;		s[1]:=0.0;		s[2]:=0.0;
    t[0]:=0.0;		t[1]:=0.0;		t[2]:=-1.0;
    for i := 8 to 11 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glTexCoord2f(data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Spodn� st�na
    n[0]:=0.0;		n[1]:=-1.0;		n[2]:=0.0;
    s[0]:=-1.0;		s[1]:=0.0;		s[2]:=0.0;
    t[0]:=0.0;		t[1]:=0.0;		t[2]:=-1.0;
    for i := 12 to 15 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glTexCoord2f(data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Prav� st�na
    n[0]:=1.0;		n[1]:=0.0;		n[2]:=0.0;
    s[0]:=0.0;		s[1]:=0.0;		s[2]:=-1.0;
    t[0]:=0.0;		t[1]:=1.0;		t[2]:=0.0;
    for i := 16 to 19 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glTexCoord2f(data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Lev� st�na
    n[0]:=-1.0;		n[1]:=0.0;		n[2]:=0.0;
    s[0]:=0.0;		s[1]:=0.0;		s[2]:=1.0;
    t[0]:=0.0;		t[1]:=1.0;		t[2]:=0.0;
    for i := 20 to 23 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glTexCoord2f(data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
  glEnd();
  //****************************************************************************
  // T�et� f�ze:
  //   Pou�it� z�kladn� barevn� textury
  //   Povolun� blendingu GL_DST_COLOR, GL_SRC_COLOR
  //   Tuto blending rovnici n�sobit dv�ma: (Cdst*Csrc)+(Csrc*Cdst) = 2(Csrc*Cdst)!
  //   Povolen� sv�tel, aby vytvo�ily ambientn� a rozpt�len� sv�tlo
  //   Vr�cen� GL_TEXTURE matice zp�t na "norm�ln�" texturovac� sou�adnice
  //   Vytvo�it geometrii
  //****************************************************************************
  if not(emboss) then
  begin
    glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glBindTexture(GL_TEXTURE_2D,texture[filter]);
    glBlendFunc(GL_DST_COLOR,GL_SRC_COLOR);
    glEnable(GL_LIGHTING);
    doCube();
  end;
  xrot := xrot + xspeed;
  yrot := yrot + yspeed;
  if (xrot>360.0) then xrot := xrot - 360.0;
  if (xrot<0.0) then xrot := xrot + 360.0;
  if (yrot>360.0) then yrot := yrot - 360.0;
  if (yrot<0.0) then yrot := yrot + 360.0;
  doLogo();                                             // Nakonec loga
  result := true;
end;

function doMesh2TexelUnits : boolean;
var
  c : array [0..3] of GLfloat;                          // Aktu�ln� vertex
  n : array [0..3] of GLfloat;                          // Normalizovan� norm�la povrchu
  s : array [0..3] of GLfloat;                          // Sm�r texturovac�ch sou�adnic s, normalizov�no
  t : array [0..3] of GLfloat;                          // Sm�r texturovac�ch sou�adnic t, normalizov�no
  l : array [0..3] of GLfloat;                          // Pozice sv�tla k p�eveden� na sou�adnice objektu
  Minv : array [0..15] of GLfloat;                      // P�evr�cen� modelview matice
  i : integer;
begin
  c[3] := 1.0;
  n[3] := 1.0;
  s[3] := 1.0;
  t[3] := 1.0;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);	// Sma�e obrazovku a hloubkov� buffer
  // Sestaven� p�evr�cen� modelview matice, tohle nahrad� funkce Push a Pop jednou funkc� glLoadIdentity()
  // Jednoduch� sestaven� t�m, �e v�echny transformace provedeme opa�n� a v opa�n�m po�ad�
  glLoadIdentity();
  glRotatef(-yrot,0.0,1.0,0.0);
  glRotatef(-xrot,1.0,0.0,0.0);
  glTranslatef(0.0,0.0,-z);
  glGetFloatv(GL_MODELVIEW_MATRIX,@Minv);
  glLoadIdentity();
  glTranslatef(0.0,0.0,z);
  glRotatef(xrot,1.0,0.0,0.0);
  glRotatef(yrot,0.0,1.0,0.0);
  // Transformace pozice sv�tla na sou�adnice objektu:
  l[0]:=LightPosition[0];
  l[1]:=LightPosition[1];
  l[2]:=LightPosition[2];
  l[3]:=1.0;                                            // Homogen� sou�adnice
  VMatMult(Minv,l);
  //****************************************************************************
  // Prvn� f�ze:
  //    Bez blendingu
  //    Bez sv�tel
  //  Nastaven� texture combineru 0 na
  //    Pou�it� bumpmapy
  //    Pou�it� neposunut�ch texturovac�ch sou�adnic
  //    Nastavev� operace s texturou na GL_REPLACE, kter� pouze vykresl� texturu
  //  Nastaven� texture combineru 1 na
  //    Posunut� texturovac� sou�adnice
  //    Nastaven� operace s texturou na GL_ADD, co� je multitexturovac�m ekvivalentem k ONE, ONE blendingu
  //****************************************************************************
  // TEXTUROVAC� JEDNOTKA #0:
  glActiveTextureARB(GL_TEXTURE0_ARB);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, bump[filter]);
  glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);
  glTexEnvf (GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_REPLACE);
  // TEXTUROVAC� JEDNOTKA #1:
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, invbump[filter]);
  glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);
  glTexEnvf (GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_ADD);
  // Obecn� p�ep�na�e
  glDisable(GL_BLEND);
  glDisable(GL_LIGHTING);
  glBegin(GL_QUADS);
    // P�edn� st�na
    n[0]:=0.0;		n[1]:=0.0;		n[2]:=1.0;
    s[0]:=1.0;		s[1]:=0.0;		s[2]:=0.0;
    t[0]:=0.0;		t[1]:=1.0;		t[2]:=0.0;
    for i := 0 to 3 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glMultiTexCoord2fARB(GL_TEXTURE0_ARB, data[5*i], data[5*i+1]);
      glMultiTexCoord2fARB(GL_TEXTURE1_ARB,data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Zadn� st�na
    n[0]:=0.0;		n[1]:=0.0;		n[2]:=-1.0;
    s[0]:=-1.0;		s[1]:=0.0;		s[2]:=0.0;
    t[0]:=0.0;		t[1]:=1.0;		t[2]:=0.0;
    for i := 4 to 7 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glMultiTexCoord2fARB(GL_TEXTURE0_ARB,data[5*i]     , data[5*i+1]);
      glMultiTexCoord2fARB(GL_TEXTURE1_ARB,data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Horn� st�na
    n[0]:=0.0;		n[1]:=1.0;		n[2]:=0.0;
    s[0]:=1.0;		s[1]:=0.0;		s[2]:=0.0;
    t[0]:=0.0;		t[1]:=0.0;		t[2]:=-1.0;
    for i := 8 to 11 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glMultiTexCoord2fARB(GL_TEXTURE0_ARB,data[5*i]     , data[5*i+1]     );
      glMultiTexCoord2fARB(GL_TEXTURE1_ARB,data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Doln� st�na
    n[0]:=0.0;		n[1]:=-1.0;		n[2]:=0.0;
    s[0]:=-1.0;		s[1]:=0.0;		s[2]:=0.0;
    t[0]:=0.0;		t[1]:=0.0;		t[2]:=-1.0;
    for i := 12 to 15 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glMultiTexCoord2fARB(GL_TEXTURE0_ARB,data[5*i]     , data[5*i+1]     );
      glMultiTexCoord2fARB(GL_TEXTURE1_ARB,data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Prav� st�na
    n[0]:=1.0;		n[1]:=0.0;		n[2]:=0.0;
    s[0]:=0.0;		s[1]:=0.0;		s[2]:=-1.0;
    t[0]:=0.0;		t[1]:=1.0;		t[2]:=0.0;
    for i := 16 to 19 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glMultiTexCoord2fARB(GL_TEXTURE0_ARB,data[5*i]     , data[5*i+1]     );
      glMultiTexCoord2fARB(GL_TEXTURE1_ARB,data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
    // Lev� st�na
    n[0]:=-1.0;		n[1]:=0.0;		n[2]:=0.0;
    s[0]:=0.0;		s[1]:=0.0;		s[2]:=1.0;
    t[0]:=0.0;		t[1]:=1.0;		t[2]:=0.0;
    for i := 20 to 23 do
    begin
      c[0]:=data[5*i+2];
      c[1]:=data[5*i+3];
      c[2]:=data[5*i+4];
      SetUpBumps(n,c,l,s,t);
      glMultiTexCoord2fARB(GL_TEXTURE0_ARB,data[5*i]     , data[5*i+1]     );
      glMultiTexCoord2fARB(GL_TEXTURE1_ARB,data[5*i]+c[0], data[5*i+1]+c[1]);
      glVertex3f(data[5*i+2], data[5*i+3], data[5*i+4]);
    end;
  glEnd();
  //****************************************************************************
  // Druh� f�ze:
  //   Pou�it� z�kladn� textury
  //   Povolen� osv�tlen�
  //   Neposunut� texturovac� sou�adnice - vyresetovat GL_TEXTURE matice
  //   Nastaven� texture environment na GL_MODULATE
  //****************************************************************************
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glDisable(GL_TEXTURE_2D);
  glActiveTextureARB(GL_TEXTURE0_ARB);
  if not(emboss) then
  begin
    glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glBindTexture(GL_TEXTURE_2D,texture[filter]);
    glBlendFunc(GL_DST_COLOR,GL_SRC_COLOR);
    glEnable(GL_BLEND);
    glEnable(GL_LIGHTING);
    doCube();
  end;
  xrot := xrot + xspeed;
  yrot := yrot + yspeed;
  if (xrot>360.0) then xrot := xrot - 360.0;
  if (xrot<0.0) then xrot := xrot + 360.0;
  if (yrot>360.0) then yrot := yrot - 360.0;
  if (yrot<0.0) then yrot := yrot + 360.0;
  doLogo;                                               // Nakonec loga
  result := true;
end;

function doMeshNoBumps : boolean;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);	// Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity();									                    // Reset matice
  glTranslatef(0.0,0.0,z);
  glRotatef(xrot,1.0,0.0,0.0);
  glRotatef(yrot,0.0,1.0,0.0);
  if (useMultitexture) then
  begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glDisable(GL_TEXTURE_2D);
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;
  glDisable(GL_BLEND);
  glBindTexture(GL_TEXTURE_2D,texture[filter]);
  glBlendFunc(GL_DST_COLOR,GL_SRC_COLOR);
  glEnable(GL_LIGHTING);
  doCube();
  xrot := xrot + xspeed;
  yrot := yrot + yspeed;
  if (xrot>360.0) then xrot := xrot - 360.0;
  if (xrot<0.0) then xrot := xrot + 360.0;
  if (yrot>360.0) then yrot := yrot - 360.0;
  if (yrot<0.0) then yrot := yrot + 360.0;
  doLogo();                                             // Nakonec loga
  result := true;
end;

function DrawGLScene():bool;                            // Vykreslov�n�
begin
  if (bumps) then
  begin
    if (useMultitexture and (maxTexelUnits>1)) then
      result := doMesh2TexelUnits
    else
      result := doMesh1TexelUnits;
  end
  else result := doMeshNoBumps;
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
            SwapBuffers(h_Dc);                        // Prohozen� buffer� (Double Buffering)
          if keys[VK_F1] then                         // Byla stisknuta kl�vesa F1?
            begin
            Keys[VK_F1] := false;                     // Ozna� ji jako nestisknutou
            KillGLWindow();                           // Zru�� okno
            FullScreen := not FullScreen;             // Negace fullscreen
            // Znovuvytvo�en� okna
            if not CreateGLWindow('NeHe''s OpenGL Framework',640,480,16,fullscreen) then
              Result := 0;                            // Konec programu pokud nebylo vytvo�eno
            end;
          //E: p�ep�n�n� Emboss/bumpmapov� m�d
          //M: vyp�n�n� a zap�n�n� multitexturingu
          //B: vyp�n�n� a zap�n�n� bumpmappingu, pouze v emboss m�du
          //F: p�ep�n�n� filtr�, GL_NEAREST nen� vhodn� pro bumpmapping
          //KURSOROV� KL�VESY: ot��en� krychle
          if (keys[ord('E')]) then
            begin
              keys[ord('E')] := false;
              emboss := not(emboss);
            end;
          if (keys[ord('M')]) then
            begin
              keys[ord('M')] := false;
              useMultitexture := (not(useMultitexture) and multitextureSupported);
            end;
          if (keys[ord('B')]) then
            begin
              keys[ord('B')]:=false;
              bumps := not(bumps);
            end;
          if (keys[ord('F')]) then
            begin
              keys[ord('F')] := false;
              filter := filter +  1;
              filter := filter mod 3;
            end;
          if (keys[VK_PRIOR]) then
            begin
              z := z - 0.02;
            end;
          if (keys[VK_NEXT]) then
            begin
              z := z + 0.02;
            end;
          if (keys[VK_UP]) then
            begin
              xspeed := xspeed - 0.01;
            end;
          if (keys[VK_DOWN]) then
            begin
              xspeed := xspeed + 0.01;
            end;
          if (keys[VK_RIGHT]) then
            begin
              yspeed := yspeed + 0.01;
            end;
          if (keys[VK_LEFT]) then
            begin
              yspeed := yspeed - 0.01;
            end;
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

