program lesson21a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL,sysutils,mmsystem,
  GLaux;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

type
  gameobject = record                       // Struktura objektu ve h�e
    fx, fy: integer;                        // Pohybov� pozice
    x, y: integer;                          // Absolutn� pozice
    spin: glfloat;                          // Ot��en� objektu dokola
    end;
  timer = record                            // Informace pro �asova�
    frequency: Int64;                       // Frekvence
    resolution: glfloat;                    // Perioda
    mm_timer_start: LongWord;               // Startovn� �as multimedi�ln�ho timeru
    mm_timer_elapsed: LongWord;             // Uplynul� �as multimedi�ln� timeru
    performance_timer: bool;                // U��v�me Performance Timer?
    performance_timer_start: Int64;         // Startovn� �as Performance Timeru
    performance_timer_elapsed: Int64;       // Uplynul� �as Performance Timeru
    end;

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  vline: array [0..10,0..9] of bool;// Ukl�d� z�znamy o vertik�ln�ch link�ch
  hline: array [0..9,0..10] of bool;// Ukl�d� z�znamy o horizont�ln�ch link�ch
  ap: bool;                         // Stisknuto 'A'?
  filled: bool;                     // Bylo ukon�eno vypl�ov�n� m��ky?
  gameover: bool;                   // Konec hry?
  anti: bool = true;                // Antialiasing?
  loop1, loop2: integer;            // ��d�c� prom�nn� cykl�
  delay: integer;                   // Doba zastaven� nep��tel
  adjust: integer = 3;              // Rychlostn� kompenzace pro pomal� syst�my
  lives: integer = 5;               // Po�et �ivot� hr��e
  level: integer = 1;               // Vnit�n� obt�nost hry
  level2: integer = 1;              // Zobrazovan� level
  stage: integer = 1;               // Etapa/f�ze hry
  player: gameobject;               // Hr��
  enemy: array [0..8] of gameobject;// Nep��tel�
  hourglass: gameobject;            // Sklen�n� hodiny
  steps: array [0..5] of integer = (1,2,4,5,10,20); // Krokovac� hodnota pro p�izp�soben� pomal�ho videa
  texture: array [0..1] of gluint;  // Dv� textury
  base: gluint;                     // Z�kladn� display list pro font
  gametimer: timer;                 // �asova�

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
var TextureImage: array [0..1] of PTAUX_RGBImageRec;        // Ukl�d� bitmapu
    Status: Bool;                                           // Indikuje chyby
    i: integer;
begin
  Status := false;
  ZeroMemory(@TextureImage,sizeof(TextureImage));           // Vynuluje pam�
  TextureImage[0] := LoadBMP('Data/font.bmp');              // Nahraje bitmapu
  TextureImage[1] := LoadBMP('Data/image.bmp');
  if Assigned(TextureImage[0]) and Assigned(TextureImage[1]) then // V�e je bez probl�m�?
    begin
    Status := true;                                         // V�e je bez probl�m�
    glGenTextures(2,Texture[0]);                            // Generuje textury
    for i:=0 to 1 do
      begin
      glBindTexture(GL_TEXTURE_2D,texture[i]);
      glTexImage2D(GL_TEXTURE_2D,0,3,TextureImage[i].sizeX,TextureImage[i].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[i].data);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
      end;
    end;
  Result := Status;                                         // Ozn�m� p��padn� chyby
end;

procedure TimerInit;                                            // Inicializace timeru
begin
  ZeroMemory(@gametimer,sizeof(gametimer));                     // Vynuluje prom�nn� struktury
  // Zjist� jestli je Performance Counter dostupn� a pokud ano, bude na�tena jeho frekvence
  if not QueryPerformanceFrequency(gametimer.frequency) then
    begin                                                       // Performance Counter nen� dostupn�
    gametimer.performance_timer := false;                       // Nastav� Performance Timer na FALSE
    gametimer.mm_timer_start := timeGetTime;                    // Z�sk�n� aktu�ln�ho �asu
    gametimer.resolution := 1/1000;                             // Nastaven� periody
    gametimer.frequency := 1000;                                // Nastaven� frekvence
    gametimer.mm_timer_elapsed := gametimer.mm_timer_start;     // Uplynul� �as = po��te�n�
    end
    else
    begin                                                       // Performance Counter je mo�n� pou��vat
    QueryPerformanceCounter(gametimer.performance_timer_start); // Po��te�n� �as
    gametimer.performance_timer := true;                        // Nastaven� Performance Timer na TRUE
    gametimer.resolution := 1/gametimer.frequency;              // Spo��t�n� periody
    gametimer.performance_timer_elapsed := gametimer.performance_timer_start; //Nastav� uplynul� �as na po��te�n�
    end;
end;

function TimerGetTime: GLfloat;                                 // Z�sk� �as v milisekund�ch
var time: Int64;                                                // �as se ukl�d� do 64-bitov�ho integeru
begin
  if gametimer.performance_timer then                           // Performance Timer
    begin
    QueryPerformanceCounter(time);                              // Na�te aktu�ln� �as
    Result := (time - gametimer.performance_timer_start)*gametimer.resolution*1000;     // Vr�t� uplynul� �as v milisekund�ch
    end                                                                                 // Multimedi�ln� timer
    else Result := (timeGetTime - gametimer.mm_timer_start)*gametimer.resolution*1000;  // Vr�t� uplynul� �as v milisekund�ch
end;

procedure ResetObjects;                                         // Reset hr��e a nep��tel
begin
  with player do                                                // Hr��
    begin
    x := 0;                                                     // Hr�� bude vlevo naho�e
    y := 0;                                                     // Hr�� bude vlevo naho�e
    fx := 0;                                                    // Pohybov� pozice
    fy := 0;                                                    // Pohybov� pozice
    end;
  for loop1:=0 to (stage*level)-1 do                            // Proch�z� nep��tele
    begin
    with enemy[loop1] do
      begin
      x := 5 + random(6);                                       // Nastav� randomovou x pozici
      y := random(11);                                          // Nastav� randomovou y pozici
      fx := x * 60;                                             // Pohybov� pozice
      fy := y * 40;                                             // Pohybov� pozice
      end;
    end;
end;

procedure BuildFont;                                            // Vytvo�en� display list� fontu
var
  cx, cy: GLfloat;                                              // Koordin�ty x, y
  loop: integer;                                                // Cyklus
begin                                                           
  base := glGenLists(256);                                      // 256 display list�
  glBindTexture(GL_TEXTURE_2D,texture[0]);                      // V�b�r textury
  for loop:=0 to 255 do                                         // Vytv��� 256 display list�
    begin
    cx := (loop mod 16) / 16;                                   // X pozice aktu�ln�ho znaku
    cy := (loop div 16) /16;                                    // Y pozice aktu�ln�ho znaku
    glNewList(base + loop,GL_COMPILE);                          // Vytvo�en� display listu
      glBegin(GL_QUADS);                                        // Pro ka�d� znak jeden obd�ln�k
        glTexCoord2f(cx,1-cy-0.0625);glVertex2i(0,16);
        glTexCoord2f(cx+0.0625,1-cy-0.0625);glVertex2i(16,16);
        glTexCoord2f(cx+0.0625,1-cy);glVertex2i(16,0);
        glTexCoord2f(cx,1-cy);glVertex2i(0,0);
      glEnd;                                                    // Konec znaku
      glTranslated(15,0,0);                                     // P�esun na pravou stranu znaku
    glEndList;                                                  // Konec kreslen� display listu
    end;
end;

procedure KillFont;                                             // Uvoln� pam� fontu
begin
  glDeleteLists(base,256);                                      // Sma�e 256 display list�
end;

procedure glPrint(x,y: GLint;text: string;sada: integer);       // V�pis text�
begin
  if text = '' then exit;                                       // Nebyl p�ed�n �et�zec
  if sada>1 then sada:=1;                                       // Byla p�ed�na �patn� znakov� sada? Pokud ano, zvol� se kurz�va
  glEnable(GL_TEXTURE_2D);                                      // Zapne texturov� mapov�n�
  glLoadIdentity;                                               // Reset matice
  glTranslated(x,y,0);                                          // P�esun na po�adovanou pozici
  glListBase(base-32+(128*sada));                               // Zvol� znakovou sadu
  if sada = 0 then glScalef(1.5,2.0,1.0);                       // Pokud je ur�ena prvn� znakov� sada font bude v�t��
  glCallLists(length(text),GL_UNSIGNED_BYTE,Pchar(text));       // V�pis textu na monitor
  glDisable(GL_TEXTURE_2D);                                     // Vypne texturov� mapov�n�
end;

procedure ReSizeGLScene(Width: GLsizei; Height: GLsizei); // Zm�na velikosti a inicializace OpenGL okna
begin
  if (Height=0) then		                                  // Zabezpe�en� proti d�len� nulou
     Height:=1;                                           // Nastav� v��ku na jedna
  glViewport(0, 0, Width, Height);                        // Resetuje aktu�ln� nastaven�
  glMatrixMode(GL_PROJECTION);                            // Zvol� projek�n� matici
  glLoadIdentity();                                       // Reset matice
  glOrtho(0.0,Width,Height,0.0,-1.0,1.0);                 // Vytvo�� pravo�hlou sc�nu
  glMatrixMode(GL_MODELVIEW);                             // Zvol� matici Modelview
  glLoadIdentity;                                         // Reset matice
end;


function InitGL:bool;	                              // V�echno nastaven� OpenGL
begin
  if not LoadGLTextures then                        // Nahraje textury
    begin
    Result := false;
    exit;
    end;
  BuildFont;                                        // Vytvo�en� fontu
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glClearColor(0.0, 0.0, 0.0, 0.5);	  	            // �ern� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glHint(GL_LINE_SMOOTH,GL_NICEST);                 // Nastaven� antialiasingu linek
  glEnable(GL_BLEND);                               // Zapne blending
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); // Typ blendingu
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;


function DrawGLScene():bool;                            // Vykreslov�n�
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);  // Sma�e obrazovku a hloubkov� buffer
  glBindTexture(GL_TEXTURE_2D,texture[0]);              // Zvol� texturu fontu
  glColor3f(1.0,0.5,1.0);                               // Purpurov� barva
  glPrint(207,24,'CRAZY GRID',0);                       // Vyp�e logo hry
  glColor3f(1.0,1.0,0.0);                               // �lut� barva
  glPrint(20,20,Format('Level:%2d',[level2]),1);        // Vyp�e level
  glPrint(20,40,Format('Stage:%2d',[stage]),1);         // Vyp�e etapu
  if gameover then                                      // Konec hry?
    begin
    glColor3ub(random(255),random(255),random(255));    // N�hodn� barva
    glPrint(472,20,'GAME OVER',1);                      // Vyp�e GAME OVER
    glPrint(456,40,'PRESS SPACE',1);                    // Vyp�e PRESS SPACE
    end;
  for loop1:=0 to lives-2 do                            // Cyklus vykresluj�c� �ivoty
    begin
    glLoadIdentity();	                                  // Reset matice
    glTranslatef(490+(loop1*40),40,0);                  // P�esun doprava od titulku
    glRotatef(-player.spin,0.0,0.0,1.0);                // Rotace proti sm�ru hodinov�ch ru�i�ek
    glColor3f(0.0,1.0,0.0);                             // Zelen� barva
    glBegin(GL_LINES);                                  // Za��tek kreslen� �ivot�
      glVertex2d(-5,-5);                                // Lev� horn� bod
      glVertex2d(5,5);                                  // Prav� doln� bod
      glVertex2d(5,-5);                                 // Prav� horn� bod
      glVertex2d(-5,5);                                 // Lev� doln� bod
    glEnd;                                              // Konec kreslen�
    glRotatef(-player.spin*0.5,0.0,0.0,1.0);            // Rotace proti sm�ru hodinov�ch ru�i�ek
    glColor3f(0.0,0.75,0.0);                            // Tmav�� zelen� barva
    glBegin(GL_LINES);                                  // Pokra�ov�n� kreslen� �ivot�
      glVertex2d(-7,0);                                 // Lev� st�edov� bod
      glVertex2d(7,0);                                  // Prav� st�edov� bod
      glVertex2d(0,-7);                                 // Horn� st�edov� bod
      glVertex2d(0,7);                                  // Doln� st�edov� bod
    glEnd;                                              // Konec kreslen�
    end;
  filled := true;                                       // P�ed testem je v�echno vypln�n�
  glLineWidth(2.0);                                     // �ir�� ��ry
  glDisable(GL_LINE_SMOOTH);                            // Vypne antialiasing
  glLoadIdentity;                                       // Reset matice
  for loop1:=0 to 10 do                                 // Cyklus zleva doprava
    begin
    for loop2:=0 to 10 do                               // Cyklus ze shora dol�
      begin
      glColor3f(0.0,0.5,1.0);                           // Modr� barva
      if hline[loop1,loop2] then glColor3f(1.0,1.0,1.0);// Byla u� linka p�ejet�? B�l� barva
      if loop1 < 10 then                                // Nekreslit �pln� vpravo
        begin
        if not hline[loop1,loop2] then filled := false; // Nebyla linka je�t� p�ejet�? V�echno je�t� nen� vypln�no
        glBegin(GL_LINES);                              // Za��tek kreslen� horizont�ln�ch linek
          glVertex2d(20+(loop1*60),70+(loop2*40));      // Lev� bod
          glVertex2d(80+(loop1*60),70+(loop2*40));      // Prav� bod
        glEnd;                                          // Konec kreslen�
        end;
      glColor3f(0.0,0.5,1.0);                           // Modr� barva
      if vline[loop1,loop2] then glColor3f(1.0,1.0,1.0);// Byla u� linka p�ejet�? B�l� barva
      if loop2 < 10 then                                // Nekreslit �pln� dol�
        begin
        if not vline[loop1,loop2] then filled := false; // Nebyla linka je�t� p�ejet�? V�echno je�t� nebylo vypln�no
        glBegin(GL_LINES);                              // Za��tek kreslen� vertik�ln�ch linek
          glVertex2d(20+(loop1*60),70+(loop2*40));      // Horn� bod
          glVertex2d(20+(loop1*60),110+(loop2*40));     // Doln� bod
        glEnd;                                          // Konec kreslen�
        end;
      glEnable(GL_TEXTURE_2D);                          // Zapne mapov�n� textur
      glColor3f(1.0,1.0,1.0);                           // B�l� barva
      glBindTexture(GL_TEXTURE_2D,texture[1]);          // Zvol� texturu
      if (loop1 < 10) and (loop2 < 10) then             // Pouze pokud je obd�ln�k v hrac� plo�e
        begin
        // Jsou p�ejety v�echny �ty�i okraje obd�ln�ku?
        if hline[loop1,loop2] and hline[loop1,loop2+1] and
            vline[loop1,loop2] and vline[loop1+1,loop2] then
          begin
          glBegin(GL_QUADS);                            // Vykresl� otexturovan� obd�ln�k
            glTexCoord2f((loop1/10)+0.1,1-(loop2/10));
            glVertex2d(20+loop1*60+59,70+loop2*40+1);   // Prav� horn�
            glTexCoord2f(loop1/10,1-(loop2/10));
            glVertex2d(20+loop1*60+1,70+loop2*40+1);    // Lev� horn�
            glTexCoord2f(loop1/10,1-loop2/10+0.1);
            glVertex2d(20+loop1*60+1,70+loop2*40+39);   // Lev� doln�
            glTexCoord2f(loop1/10+0.1,1-loop2/10+0.1);
            glVertex2d(20+loop1*60+59,70+loop2*40+39);  // Prav� doln�
          glEnd;                                        // Konec kreslen�
          end;
        end;
      glDisable(GL_TEXTURE_2D);                         // Vypne mapov�n� textur
      end;
    end;
  glLineWidth(1.0);                                     // ���ka ��ry 1.0
  if anti then glEnable(GL_LINE_SMOOTH);                // M� b�t zapnut� antialiasing? Zapne antialiasing
  if hourglass.fx = 1 then                              // Hodiny se maj� vykreslit
    begin
    glLoadIdentity;                                     // Reset Matice
    glTranslatef(20+hourglass.x*60,70+hourglass.y*40,0);// Um�st�n�
    glRotatef(hourglass.spin,0.0,0.0,1.0);              // Rotace ve sm�ru hodinov�ch ru�i�ek
    glColor3ub(random(255),random(255),random(255));    // N�hodn� barva
    glBegin(GL_LINES);                                  // Vykreslen� p�es�pac�ch hodin
      glVertex2d(-5,-5);                                // Lev� horn� bod
      glVertex2d( 5, 5);                                // Prav� doln� bod
      glVertex2d( 5,-5);                                // Prav� horn� bod
      glVertex2d(-5, 5);                                // Lev� doln� bod
      glVertex2d(-5, 5);                                // Lev� doln� bod
      glVertex2d( 5, 5);                                // Prav� doln� bod
      glVertex2d(-5,-5);                                // Lev� horn� bod
      glVertex2d( 5,-5);                                // Prav� horn� bod
    glEnd();                                            // Konec kreslen�
    end;
  glLoadIdentity;                                       // Reset Matice
  glTranslatef(20+player.fx,70+player.fy,0.0);          // P�esun na pozici
  glRotatef(player.spin,0.0,0.0,1.0);                   // Rotace po sm�ru hodinov�ch ru�i�ek
  glColor3f(0.0,1.0,0.0);                               // Zelen� barva
  glBegin(GL_LINES);                                    // Vykreslen� hr��e
    glVertex2d(-5,-5);                                  // Lev� horn� bod
    glVertex2d( 5, 5);                                  // Prav� doln� bod
    glVertex2d( 5,-5);                                  // Prav� horn� bod
    glVertex2d(-5, 5);                                  // Lev� doln� bod
  glEnd();                                              // Konec kreslen�
  glRotatef(player.spin*0.5,0.0,0.0,1.0);               // Rotace po sm�ru hodinov�ch ru�i�ek
  glColor3f(0.0,0.75,0.0);                              // Tmav�� zelen� barva
  glBegin(GL_LINES);                                    // Pokra�ov�n� kreslen� hr��e
    glVertex2d(-7, 0);                                  // Lev� st�edov� bod
    glVertex2d( 7, 0);                                  // Prav� st�edov� bod
    glVertex2d( 0,-7);                                  // Horn� st�edov� bod
    glVertex2d( 0, 7);                                  // Doln� st�edov� bod
  glEnd();                                              // Konec kreslen�
  for loop1:=0 to (stage*level)-1 do                    // Vykresl� nep��tele
    begin
    glLoadIdentity;                                     // Reset matice
    glTranslatef(20+enemy[loop1].fx,70+enemy[loop1].fy,0.0);  // P�esun na pozici
    glColor3f(1.0,0.5,0.5);                             // R��ov� barva
    glBegin(GL_LINES);                                  // Vykreslen� nep��tel
      glVertex2d( 0,-7);                                // Horn� bod
      glVertex2d(-7, 0);                                // Lev� bod
      glVertex2d(-7, 0);                                // Lev� bod
      glVertex2d( 0, 7);                                // Doln� bod
      glVertex2d( 0, 7);                                // Doln� bod
      glVertex2d( 7, 0);                                // Prav� bod
      glVertex2d( 7, 0);                                // Prav� bod
      glVertex2d( 0,-7);                                // Horn� bod
    glEnd();                                            // Konec kreslen�
    glRotatef(enemy[loop1].spin,0.0,0.0,1.0);           // Rotace vnit�ku nep��tele
    glColor3f(1.0,0.0,0.0);                             // Krvav� barva
    glBegin(GL_LINES);                                  // Pokra�ov�n� kreslen� nep��tel
      glVertex2d(-7,-7);                                // Lev� horn� bod
      glVertex2d( 7, 7);                                // Prav� doln� bod
      glVertex2d(-7, 7);                                // Lev� doln� bod
      glVertex2d( 7,-7);                                // Prav� horn� bod
    glEnd();                                            // Konec kreslen�
    end;
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
  KillFont;                                             // Zru�� font
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
  start: glfloat;
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
  ResetObjects;                                       // Inicializuje pozici hr��e a nep��tel
  TimerInit;                                          // Zprovozn�n� timeru
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
          start := TimerGetTime;                      // Nagrabujeme aktu�ln� �as
          // Je program aktivn�, ale nelze kreslit? Byl stisknut ESC?
          if (active and not(DrawGLScene()) or keys[VK_ESCAPE]) then
            done:=true                                // Ukon��me program
          else                                        // P�ekreslen� sc�ny
            SwapBuffers(h_Dc);                        // Prohozen� buffer� (Double Buffering)
          while (TimerGetTime < start + steps[adjust]*2.0) do
            begin
            // Pl�tv� cykly procesoru na rychl�ch syst�mech
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
          if (keys[ord('A')] and not(ap)) then        // Stisk A
            begin
            ap := TRUE;                               // Nastav� p��znak
            anti := not(anti);                        // Zapne/vypne antialiasing
            end;
          if not(keys[ord('A')]) then ap := FALSE;    // Uvoln�n� A
          if (not(gameover) and active) then          // Nen�-li konec hry a okno je aktivn�
            begin
            for loop1 := 0 to (stage*level) - 1 do    // Proch�z� v�echny nep��tele
              begin
              if ((enemy[loop1].x<player.x) and (enemy[loop1].fy=enemy[loop1].y*40)) then enemy[loop1].x := enemy[loop1].x + 1;   // P�esun o pol��ko doprava
              if ((enemy[loop1].x>player.x) and (enemy[loop1].fy=enemy[loop1].y*40)) then enemy[loop1].x := enemy[loop1].x - 1;   // P�esun o pol��ko doleva
              if ((enemy[loop1].y<player.y) and (enemy[loop1].fx=enemy[loop1].x*60)) then enemy[loop1].y := enemy[loop1].y + 1;   // P�esun o pol��ko dol�
              if ((enemy[loop1].y>player.y) and (enemy[loop1].fx=enemy[loop1].x*60)) then enemy[loop1].y := enemy[loop1].y - 1;   // P�esun o pol��ko nahoru
              if ((delay >(3-level)) and (hourglass.fx <> 2)) then              // Hr�� nesebral p�es�pac� hodiny
                begin
                delay := 0;                                                     // Reset delay na nulu
                for loop2 := 0 to (stage*level) - 1 do                          // Proch�z� v�echny nep��tele
                  begin
                  if (enemy[loop2].fx<enemy[loop2].x*60) then                   // Fx je men�� ne� x
                    begin
                    enemy[loop2].fx := enemy[loop2].fx + steps[adjust];         // Zv��it fx
                    enemy[loop2].spin := enemy[loop2].spin + steps[adjust];     // Rotace ve sm�ru hodinov�ch ru�i�ek
                    end;
                  if (enemy[loop2].fx>enemy[loop2].x*60) then                   // Fx je v�t�� ne� x
                    begin
                    enemy[loop2].fx := enemy[loop2].fx - steps[adjust];         // Sn�it fx
                    enemy[loop2].spin := enemy[loop2].spin - steps[adjust];     // Rotace proti sm�ru hodinov�ch ru�i�ek
                    end;
                  if (enemy[loop2].fy<enemy[loop2].y*40) then                   // Fy je men�� ne� y
                    begin
                    enemy[loop2].fy := enemy[loop2].fy + steps[adjust];         // Zv��it fy
                    enemy[loop2].spin := enemy[loop2].spin + steps[adjust];     // Rotace ve sm�ru hodinov�ch ru�i�ek
                    end;
                  if (enemy[loop2].fy>enemy[loop2].y*40) then                   // Fy je v�t�� ne� y
                    begin
                    enemy[loop2].fy := enemy[loop2].fy - steps[adjust];         // Sn�it fy
                    enemy[loop2].spin := enemy[loop2].spin - steps[adjust];     // Rotace proti sm�ru hodinov�ch ru�i�ek
                    end;
                  end;
                end;
              // Setk�n� nep��tele s hr��em
              if ((enemy[loop1].fx = player.fx) and (enemy[loop1].fy = player.fy)) then
                begin
                lives := lives - 1;                                             // Hr�� ztr�c� �ivot
                if (lives = 0) then gameover := TRUE;                           // Nulov� po�et �ivot� - Konec hry
                ResetObjects();                                                 // Reset pozice hr��e a nep��tel
                PlaySound('Data/Die.wav', 0, SND_SYNC);                         // Zahraje um�r��ek
                end;
              end;
            if (keys[VK_RIGHT] and (player.x<10) and (player.fx =player.x*60) and (player.fy= player.y*40)) then
              begin
              hline[player.x][player.y] := TRUE;                                // Ozna�en� linky
              player.x := player.x + 1;                                         // Doprava
              end;
            if (keys[VK_LEFT] and (player.x>0) and (player.fx =player.x*60) and (player.fy= player.y*40)) then
              begin
              player.x := player.x - 1;                                         // Doleva
              hline[player.x][player.y] := TRUE;                                // Ozna�en� linky
              end;
            if (keys[VK_DOWN] and (player.y<10) and (player.fx =player.x*60) and (player.fy= player.y*40)) then
              begin
              vline[player.x][player.y] := TRUE;                                // Ozna�en� linky
              player.y := player.y + 1;                                         // Dol�
              end;
            if (keys[VK_UP] and (player.y>0) and (player.fx =player.x*60) and (player.fy= player.y*40)) then
              begin
              player.y := player.y - 1;                                         // Nahoru
              vline[player.x][player.y] := TRUE;                                // Ozna�en� linky
              end;
            if (player.fx<player.x*60) then                                     // Fx je men�� ne� x
              begin
              player.fx := player.fx + steps[adjust];                           // Zv�t�� fx
              end;
            if (player.fx>player.x*60) then                                     // Fx je v�t�� ne� x
              begin
              player.fx := player.fx - steps[adjust];                           // Zmen�� fx
              end;
            if (player.fy<player.y*40) then                                     // Fy je men�� ne� y
              begin
              player.fy := player.fy + steps[adjust];                           // Zv�t�� fy
              end;
            if (player.fy>player.y*40) then                                     // Fy je v�t�� ne� y
              begin
              player.fy := player.fy - steps[adjust];                           // Zmen�� fy
              end;
            end
            else                                                                // Jinak (if (not(gameover) and active))
            begin
            if (keys[ord(' ')]) then                                            // Stisknut� mezern�k
              begin
              gameover := FALSE;                                                // Konec hry
              filled := TRUE;                                                   // M��ka vypln�n�
              level := 1;                                                       // Level
              level2 := 1;                                                      // Zobrazovan� level
              stage := 0;                                                       // Obt�nost hry
              lives := 5;                                                       // Po�et �ivot�
              end;
            end;
          if (filled) then                                                      // Vypln�n� m��ka?
            begin
            PlaySound('Data/Complete.wav', 0, SND_SYNC);                        // Zvuk ukon�en� levelu
            stage := stage + 1;                                                 // Inkrementace obt�nosti
            if (stage>3) then                                                   // Je v�t�� ne� t�i?
              begin
              stage := 1;                                                       // Reset na jedni�ku
              level := level + 1;                                               // Zv�t�� level
              level2 := level2 + 1;                                             // Zv�t�� zobrazovan� level
              if (level>3) then                                                 // Je level v�t�� ne� t�i?
                begin
                level := 3;                                                     // Vr�t� ho zp�tky na t�i
                lives := lives + 1;                                             // �ivot nav�c
                if (lives>5) then lives := 5;                                   // M� v�c �ivot� ne� p�t? Maxim�ln� po�et �ivot� p�t
                end;
              end;
            ResetObjects();                                                     // Reset pozice hr��e a nep��tel
            for loop1 := 0 to 10 do                                             // Cyklus skrz x koordin�ty m��ky
              for loop2 := 0 to 10 do                                           // Cyklus skrz y koordin�ty m��ky
                begin
                if (loop1<10) then hline[loop1][loop2] := FALSE;                // X mus� b�t men�� ne� deset - Nulov�n�
                if (loop2<10) then vline[loop1][loop2] := FALSE;                // Y mus� b�t men�� ne� deset - Nulov�n�
                end;
            end;
          // Hr�� sebral p�es�pac� hodiny
          if ((player.fx = hourglass.x*60) and (player.fy = hourglass.y*40) and (hourglass.fx = 1)) then
            begin
            PlaySound('Data/freeze.wav', 0, SND_ASYNC or SND_LOOP);             // Zvuk zmrazen�
            hourglass.fx := 2;                                                  // Skryje hodiny
            hourglass.fy := 0;                                                  // Nuluje ��ta�
            end;
          player.spin := player.spin + 0.5*steps[adjust];                       // Rotace hr��e
          if (player.spin>360.0) then player.spin := player.spin - 360;         // �hel je v�t�� ne� 360�? Ode�te 360
          hourglass.spin := hourglass.spin - 0.25*steps[adjust];                // Rotace p�es�pac�ch hodin
          if (hourglass.spin<0.0) then hourglass.spin := hourglass.spin + 360.0;// �hel je men�� ne� 0�? P�i�te 360
          hourglass.fy := hourglass.fy + steps[adjust];                         // Zv�t�en� hodnoty ��ta�e p�es�pac�ch hodin
          if ((hourglass.fx = 0) and (hourglass.fy>6000/level)) then            // Hodiny jsou skryt� a p�etekl ��ta�
            begin
            PlaySound('Data/hourglass.wav', 0, SND_ASYNC);                      // Zvuk zobrazen� hodin
            hourglass.x := random(10) + 1;                                      // N�hodn� pozice
            hourglass.y := random(11);                                          // N�hodn� pozice
            hourglass.fx := 1;                                                  // Zobrazen� hodin
            hourglass.fy := 0;                                                  // Nulov�n� ��ta�e
            end;
          if ((hourglass.fx = 1) and (hourglass.fy>6000/level)) then            // Hodiny jsou zobrazen� a p�etekl ��ta�
            begin
            hourglass.fx := 0;                                                  // Skr�t hodiny
            hourglass.fy := 0;                                                  // Nulov�n� ��ta�e
            end;
          if ((hourglass.fx = 2) and (hourglass.fy>500+(500*level))) then       // Nep��tel� zmrazen� a p�etekl ��ta�
            begin
            PlaySound(nil, 0, 0);                                               // Vypne zvuk zmrazen�
            hourglass.fx := 0;                                                  // Skr�t hodiny
            hourglass.fy := 0;                                                  // Nulov�n� ��ta�e
            end;
          delay := delay + 1;                                                   // Inkrementuje ��ta� zpo�d�n� nep��tel
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

