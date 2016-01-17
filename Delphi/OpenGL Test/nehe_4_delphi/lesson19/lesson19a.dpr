program lesson19a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL, sysutils,
  GLaux;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

type
  particles = record          // Vytvo�� stukturu pro ��stici
    active: bool;             // Aktivn�?
    life: glfloat;            // �ivot
    fade: glfloat;            // Rychlost st�rnut�
    r, g, b: glfloat;         // Slo�ky barvy
    x, y, z: glfloat;         // Pozice
    xi, yi, zi: glfloat;      // Sm�r a rychlost
    xg, yg, zg: glfloat;      // Gravitace
    end;

const
  MAX_PARTICLES = 1000;      // Po�et vytv��en�ch ��stic

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  rainbow: bool = true;             // Duhov� efekt?
  sp: bool;                         // Stisknut� mezern�k?
  rp: bool;                         // Stisknut� enter?
  slowdown: GLfloat = 2.0;          // Zpomalen� ��stic
  xspeed, yspeed: GLfloat;          // Z�kladn� rychlost na os�ch
  zoom: GLfloat = -40.0;            // Zoom
  loop: gluint;                     // ��d�c� prom�nn� cykl�
  col: gluint;                      // Vybran� barva
  delay: gluint;                    // Zpo�d�n� pro duhov� efekt
  texture: array [0..0] of gluint;  // Ukl�d� texturu
  particle: array [0..MAX_PARTICLES-1] of particles;    // Pole ��stic
  colors: array [0..11,0..2] of glfloat = ((1.0,0.5,0.5),(1.0,0.75,0.5),(1.0,1.0,0.5),(0.75,1.0,0.5), // Barevn� paleta
                                           (0.5,1.0,0.5),(0.5,1.0,0.75),(0.5,1.0,1.0),(0.5,0.75,1.0),
                                           (0.5,0.5,1.0),(0.75,0.5,1.0),(1.0,0.5,1.0),(1.0,0.5,0.75));

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
var TextureImage: array [0..0] of PTAUX_RGBImageRec;        // Ukl�d� bitmapu
    Status: Bool;                                           // Indikuje chyby
begin
  Status := false;
  ZeroMemory(@TextureImage,sizeof(TextureImage));           // Vynuluje pam�
  TextureImage[0] := LoadBMP('Data/Particle.bmp');          // Nahraje bitmapu
  if Assigned(TextureImage[0]) then                         // V�e je bez probl�m�?
    begin
    Status := true;                                         // V�e je bez probl�m�
    glGenTextures(1,Texture[0]);                            // Generuje texturu
    glBindTexture(GL_TEXTURE_2D,texture[0]);                // Typick� vytv��en� textury z bitmapy
    glTexImage2D(GL_TEXTURE_2D,0,3,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[0].data);    // Vlastn� vytv��en� textury
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);   // Filtrov�n� p�i zv�t�en�
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);   // Filtrov�n� p�i zmen�en�
    end;
  Result := Status;                                         // Ozn�m� p��padn� chyby
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
  if not LoadGLTextures then                        // Nahraje texturu
    begin
    Result := false;
    exit;
    end;
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glClearColor(0.0, 0.0, 0.0, 0.0);	  	            // �ern� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glDisable(GL_DEPTH_TEST);                         // Vypne hloubkov� testov�n�
  glEnable(GL_BLEND);                               // Zapne blending
  glBlendFunc(GL_SRC_ALPHA,GL_ONE);                 // Typ blendingu
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST); // Perspektiva
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);           // Jemnost bod�
  glEnable(GL_TEXTURE_2D);                          // Zapne mapov�n� textur
  glBindTexture(GL_TEXTURE_2D,texture[0]);          // Vybere texturu
  for loop:=0 to MAX_PARTICLES-1 do                 // Inicializace ��stic
    begin
    particle[loop].active := true;                                  // Aktivace
    particle[loop].life := 1.0;                                     // O�iven�
    particle[loop].fade := Random(100)/1000 + 0.003;                // Rychlost st�rnut�
    particle[loop].r := colors[Trunc(loop*(12/MAX_PARTICLES)),0];   // �erven�
    particle[loop].g := colors[Trunc(loop*(12/MAX_PARTICLES)),1];   // Zelen�
    particle[loop].b := colors[Trunc(loop*(12/MAX_PARTICLES)),2];   // Modr�
    particle[loop].xi := (random(50)-26.0)*10.0;                    // Rychlost a sm�r pohybu na ose x
    particle[loop].yi := (random(50)-25.0)*10.0;                    // Rychlost a sm�r pohybu na ose y
    particle[loop].zi := (random(50)-25.0)*10.0;                    // Rychlost a sm�r pohybu na ose z
    particle[loop].xg := 0.0;                                       // Gravitace na ose x
    particle[loop].yg := -0.8;                                      // Gravitace na ose y
    particle[loop].zg := 0.0;                                       // Gravitace na ose z
    end;
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;

function DrawGLScene():bool;                            // Vykreslov�n�
var x,y,z: glfloat;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);  // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity();	                                    // Reset matice
  for loop:=0 to MAX_PARTICLES-1 do                     // Cyklus proch�z� ka�dou ��stici
    begin
    if particle[loop].active then                       // Pokud je ��stice aktivn�
      begin
      x := particle[loop].x;                            // x pozice
      y := particle[loop].y;                            // y pozice
      z := particle[loop].z+zoom;                       // z pozice + zoom
      // Barva ��stice
      glColor4f(particle[loop].r,particle[loop].g,particle[loop].b,particle[loop].life);
      glBegin(GL_TRIANGLE_STRIP);                       // Vytvo�� obd�ln�k pomoc� triangle strip
        glTexCoord2d(1,1); glVertex3f(x+0.5,y+0.5,z);   // Horn� prav�
        glTexCoord2d(0,1); glVertex3f(x-0.5,y+0.5,z);   // Horn� lev�
        glTexCoord2d(1,0); glVertex3f(x+0.5,y-0.5,z);   // Doln� prav�
        glTexCoord2d(0,0); glVertex3f(x-0.5,y-0.5,z);   // Doln� lev�
      glEnd;                                            // Ukon�� triangle strip
      particle[loop].x := particle[loop].x + particle[loop].xi/(slowdown*1000);   // Pohyb na ose x
      particle[loop].y := particle[loop].y + particle[loop].yi/(slowdown*1000);   // Pohyb na ose y
      particle[loop].z := particle[loop].z + particle[loop].zi/(slowdown*1000);   // Pohyb na ose z
      particle[loop].xi := particle[loop].xi + particle[loop].xg;                 // Gravita�n� p�soben� na ose x
      particle[loop].yi := particle[loop].yi + particle[loop].yg;                 // Gravita�n� p�soben� na ose y
      particle[loop].zi := particle[loop].zi + particle[loop].zg;                 // Gravita�n� p�soben� na ose z
      particle[loop].life := particle[loop].life - particle[loop].fade;           // Sn�� �ivot o st�rnut�
      if (particle[loop].life < 0.0) then                         // Pokud zem�ela
      begin
        particle[loop].life := 1.0;                               // Nov� �ivot
        particle[loop].fade := (random(100)/1000.0) + 0.003;      // N�hodn� st�rnut�
        particle[loop].x := 0.0;                                  // Vycentrov�n� doprost�ed obrazovky
        particle[loop].y := 0.0;                                  // Vycentrov�n� doprost�ed obrazovky
        particle[loop].z := 0.0;                                  // Vycentrov�n� doprost�ed obrazovky
        particle[loop].xi := xspeed + (random(60) - 32.0);        // Nov� rychlost a sm�r
        particle[loop].yi := yspeed + (random(60) - 30.0);        // Nov� rychlost a sm�r
        particle[loop].zi := (random(60) - 30.0);                 // Nov� rychlost a sm�r
        particle[loop].r := colors[col, 0];                       // Vybere barvu z palety
        particle[loop].g := colors[col, 1];                       // Vybere barvu z palety
        particle[loop].b := colors[col, 2];                       // Vybere barvu z palety
      end;
      // Pokud je stisknuta 8 a y gravitace je men�� ne� 1.5
      if (keys[VK_NUMPAD8] AND (particle[loop].yg < 1.5)) then particle[loop].yg := particle[loop].yg + 0.01;
      // Pokud je stisknuta 2 a y gravitace je men�� ne� -1.5
      if (keys[VK_NUMPAD2] AND (particle[loop].yg > -1.5)) then particle[loop].yg := particle[loop].yg - 0.01;
      // Pokud je stisknuta 6 a x gravitace je men�� ne� 1.5
      if (keys[VK_NUMPAD6] AND (particle[loop].xg < 1.5)) then particle[loop].xg := particle[loop].xg + 0.01;
      // Pokud je stisknuta 4 a x gravitace je men�� ne� -1.5
      if (keys[VK_NUMPAD4] AND (particle[loop].xg > -1.5)) then particle[loop].xg := particle[loop].xg - 0.01;
      if (keys[VK_TAB]) then                                      // Zp�sob� v�buch
      begin
        particle[loop].x := 0.0;                                  // Vycentrov�n� na st�ed obrazovky
        particle[loop].y := 0.0;                                  // Vycentrov�n� na st�ed obrazovky
        particle[loop].z := 0.0;                                  // Vycentrov�n� na st�ed obrazovky
        particle[loop].xi := (random(50)-26.0)*10.0;              // N�hodn� rychlost
        particle[loop].yi := (random(50)-25.0)*10.0;              // N�hodn� rychlost
        particle[loop].zi := (random(50)-25.0)*10.0;              // N�hodn� rychlost
      end;
      end;
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
  if FullScreen then slowdown := 1.0;                 // Zrychlit ve fullscreenu
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
          if (keys[VK_ADD] and (slowdown > 1.0)) then slowdown := slowdown - 0.01;		  // Urychlen� ��stic
          if (keys[VK_SUBTRACT] and (slowdown < 4.0)) then slowdown := slowdown + 0.01;	// Zpomalen� ��stic
          if (keys[VK_PRIOR]) then zoom := zoom + 0.1;                                  // P�ibl�en� pohledu
          if (keys[VK_NEXT]) then zoom := zoom - 0.1;                                   // Odd�len� pohledu
          if (keys[VK_RETURN] and not(rp)) then                                         // Stisk enteru
            begin
            rp := true;                                                                 // Nastav� p��znak
            rainbow := not(rainbow);                                                    // Zapne/vypne duhov� efekt
            end;
          if (not(keys[VK_RETURN])) then rp := false;                                   // Po uvoln�n� vypne p��znak
          if ((keys[ord(' ')] and not(sp)) or (rainbow and (delay > 25))) then          // Mezern�k nebo duhov� efekt
            begin
            if (keys[ord(' ')]) then rainbow := false;                                  // Pokud je stisknut vypne se duhov� m�d
            sp := true;                                                                 // Ozn�m� programu, �e byl stisknut mezern�k
            delay := 0;                                                                 // Resetuje zpo�d�n� duhov�ch barev
            col := col + 1;                                                             // Zm�n� barvu ��stice
            if (col > 11) then col := 0;                                                // Proti p�ete�en� pole
            end;
          if (not(keys[ord(' ')])) then sp := false;                                    // Uvoln�n� mezern�ku
          if (keys[VK_UP] and (yspeed < 200)) then yspeed := yspeed + 1.0;              // �ipka nahoru
          if (keys[VK_DOWN] and (yspeed > -200)) then yspeed := yspeed - 1.0;           // �ipka dol�
          if (keys[VK_RIGHT] and (xspeed < 200)) then xspeed := xspeed + 1.0;           // �ipka doprava
          if (keys[VK_LEFT] and (xspeed > -200)) then xspeed := xspeed - 1.0;           // �ipka doleva
          delay := delay + 1;                                                           // Inkrementace zpo�d�n� duhov�ho efektu
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

