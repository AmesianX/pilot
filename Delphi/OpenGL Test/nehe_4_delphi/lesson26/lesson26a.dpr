program lesson26a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL,sysutils,
  GLaux;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

const // Parametry sv�tla
  LightAmb: array [0..3] of GLFloat = (0.7,0.7,0.7,1.0);    // Okoln�
  LightDif: array [0..3] of GLFloat = (1.0,1.0,1.0,1.0);    // Rozpt�len�
  LightPos: array [0..3] of GLFloat = (4.0,4.0,6.0,1.0);    // Pozice

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  q: GLUquadricObj;                 // Quadratic pro kreslen� koule (m��e)
  xrot: GLFloat = 0;                // Rotace v ose x
  yrot: GLFloat = 0;                // Rotace v ose y
  xrotspeed: GLfloat = 0;           // Rychlost x rotace
  yrotspeed: GLfloat = 0;           // Rychlost y rotace
  zoom: GLfloat = -7.0;             // Hloubka v obrazovce
  height: GLfloat = 2.0;            // V��ka m��e nad sc�nou
  texture: array [0..2] of GLuint;  // 3 textury

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
var TextureImage: array [0..2] of PTAUX_RGBImageRec;        // Ukl�d� bitmapy
    Status: Bool;                                           // Indikuje chyby
    i: integer;                                             // Cyklus
begin
  Status := false;
  ZeroMemory(@TextureImage,sizeof(TextureImage));           // Vynuluje pam�
  TextureImage[0] := LoadBMP('Data/EnvWall.bmp');           // Nahraje bitmapu
  TextureImage[1] := LoadBMP('Data/Ball.bmp');              // Nahraje bitmapu
  TextureImage[2] := LoadBMP('Data/EnvRoll.bmp');           // Nahraje bitmapu
  if Assigned(TextureImage[0]) and Assigned(TextureImage[1])
    and Assigned(TextureImage[2]) then                      // V�e je bez probl�m�?
    begin
    Status := true;                                         // V�e je bez probl�m�
    glGenTextures(3,Texture[0]);                            // Generuje textury
    for i := 0 to 2 do
      begin
      glBindTexture(GL_TEXTURE_2D,texture[i]);                // Typick� vytv��en� textury z bitmapy
      glTexImage2D(GL_TEXTURE_2D,0,3,TextureImage[i].sizeX,TextureImage[i].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[i].data);    // Vlastn� vytv��en� textury
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);   // Filtrov�n� p�i zv�t�en�
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);   // Filtrov�n� p�i zmen�en�
      end;
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
  glClearColor(0.2, 0.5, 1.0, 1.0);	  	            // Sv�tle modr� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glClearStencil(0);                                // Nastaven� maz�n� stencil bufferu
  glEnable(GL_DEPTH_TEST);			                    // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LEQUAL);				                    // Typ hloubkov�ho testov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST); // Nejlep�� perspektivn� korekce
  glEnable(GL_TEXTURE_2D);                          // Zapne mapov�n� textur
  glLightfv(GL_LIGHT0,GL_AMBIENT,@LightAmb);        // Okoln�
  glLightfv(GL_LIGHT0,GL_DIFFUSE,@LightDif);        // Rozptylov�
  glLightfv(GL_LIGHT0,GL_POSITION,@LightPos);       // Pozice
  glEnable(GL_LIGHT0);                              // Povol� sv�tlo 0
  glEnable(GL_LIGHTING);                            // Povol� sv�tla
  q := gluNewQuadric;                               // Nov� quadratic
  gluQuadricNormals(q,GL_SMOOTH);                   // Norm�ly pro sv�tlo
  gluQuadricTexture(q,GL_TRUE);                     // Texturov� koordin�ty
  glTexGeni(GL_S,GL_TEXTURE_GEN_MODE,GL_SPHERE_MAP);// Automatick� mapov�n� textur
  glTexGeni(GL_T,GL_TEXTURE_GEN_MODE,GL_SPHERE_MAP);// Automatick� mapov�n� textur
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;

procedure DrawObject;                               // Vykresl� pl�ov� m��
begin
  glColor3f(1.0,1.0,1.0);                           // B�l� barva
  glBindTexture(GL_TEXTURE_2D,texture[1]);          // Zvol� texturu m��e
  gluSphere(q,0.35,32,16);                          // Nakresl� kouli
  glBindTexture(GL_TEXTURE_2D,texture[2]);          // Zvol� texturu sv�tla
  glColor4f(1.0,1.0,1.0,0.4);                       // B�l� barva s 40% alfou
  glEnable(GL_BLEND);                               // Zapne blending
  glBlendFunc(GL_SRC_ALPHA,GL_ONE);                 // M�d blendingu
  glEnable(GL_TEXTURE_GEN_S);                       // Zapne kulov� mapov�n�
  glEnable(GL_TEXTURE_GEN_T);                       // Zapne kulov� mapov�n�
  gluSphere(q,0.35,32,16);                          // Stejn� koule jako p�ed chv�l�
  glDisable(GL_TEXTURE_GEN_S);                      // Vypne kulov� mapov�n�
  glDisable(GL_TEXTURE_GEN_T);                      // Vypne kulov� mapov�n�
  glDisable(GL_BLEND);                              // Vepne blending
end;

procedure DrawFloor;                                // Vykresl� podlahu
begin
  glBindTexture(GL_TEXTURE_2D,texture[0]);          // Zvol� texturu podlahy
  glBegin(GL_QUADS);                                // Kreslen� obd�ln�k�
    glNormal3f(0.0, 1.0, 0.0);                      // Norm�lov� vektor m��� vzh�ru
    glTexCoord2f(0.0, 1.0);                         // Lev� doln� bod textury
    glVertex3f(-2.0, 0.0, 2.0);                     // Lev� doln� bod podlahy
    glTexCoord2f(0.0, 0.0);                         // Lev� horn� bod textury
    glVertex3f(-2.0, 0.0,-2.0);                     // Lev� horn� bod podlahy
    glTexCoord2f(1.0, 0.0);                         // Prav� horn� bod textury
    glVertex3f( 2.0, 0.0,-2.0);                     // Prav� horn� bod podlahy
    glTexCoord2f(1.0, 1.0);                         // Prav� doln� bod textury
    glVertex3f( 2.0, 0.0, 2.0);                     // Prav� doln� bod podlahy
  glEnd();                                          // Konec kreslen�
end;

function DrawGLScene():bool;                            // Vykreslov�n�
var
  eqr: array [0..3] of Double;                          // Pou�ito pro odra�en� objekt
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT
          or GL_STENCIL_BUFFER_BIT);                    // Sma�e obrazovku, hloubkov� buffer a stencil buffer
  eqr[0] := 0.0;                                        // Rovnice o�ez�vac� plochy
  eqr[1] := -1.0;
  eqr[2] := 0.0;
  eqr[3] := 0.0;
  glLoadIdentity();	                                    // Reset matice
  glTranslatef(0.0,-0.6,zoom);                          // Zoom a vyv��en� kamery nad podlahu
  glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);     // Nastav� masku barev, aby se nic nezobrazilo
  glEnable(GL_STENCIL_TEST);                            // Zapne stencil buffer pro pam�ov� obraz podlahy
  glStencilFunc(GL_ALWAYS,1,1);                         // Poka�d� prob�hne, reference, maska
  glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);              // Vykreslen�m nastav�me konkr�tn� bit ve stencil bufferu na 1
  glDisable(GL_DEPTH_TEST);                             // Vypne testov�n� hloubky
  DrawFloor;                                            // Vykresl� podlahu (do stencil bufferu ne na sc�nu)
  glEnable(GL_DEPTH_TEST);                              // Zapne testov�n� hloubky
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);         // Povol� zobrazov�n� barev
  glStencilFunc(GL_EQUAL,1,1);                          // Zobraz� se pouze pixely na jedni�k�ch ve stencil bufferu (podlaha)
  glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);                 // Nem�nit obsah stencil bufferu
  glEnable(GL_CLIP_PLANE0);                             // Zapne o�ez�vac� testy pro odraz
  glClipPlane(GL_CLIP_PLANE0,@eqr);                      // Rovnice o�ez�vac� roviny
  glPushMatrix;                                         // Z�loha matice
    glScalef(1.0,-1.0,1.0);                             // Zrcadlen� sm�ru osy y
    glLightfv(GL_LIGHT0,GL_POSITION,@LightPos);         // Um�st�n� sv�tla
    glTranslatef(0.0,height,0.0);                       // Um�st�n� m��e
    glRotatef(xrot,1.0,0.0,0.0);                        // Rotace na ose x
    glRotatef(yrot,0.0,1.0,0.0);                        // Rotace na ose y
    DrawObject;                                         // Vykresl� m�� (odraz)
  glPopMatrix;                                          // Obnov� matici
  glDisable(GL_CLIP_PLANE0);                            // Vypne o�ez�vac� rovinu
  glDisable(GL_STENCIL_TEST);                           // U� nebudeme pot�ebovat stencil testy
  glLightfv(GL_LIGHT0,GL_POSITION,@LightPos);           // Um�st�n� sv�tla
  glEnable(GL_BLEND);                                   // Zapne blending, jinak by se odraz m��e nezobrazil
  glDisable(GL_LIGHTING);                               // Kv�li blendingu vypneme sv�tla
  glColor4f(1.0,1.0,1.0,0.8);                           // B�l� barva s 80% pr�hlednost�
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);     // Funkce na b�zi alfy zdroje a jedna m�nus alfy c�le
  DrawFloor;                                            // Vykresl� podlahu
  glEnable(GL_LIGHTING);                                // Zapne sv�tla
  glDisable(GL_BLEND);                                  // Vypne blending
  glTranslatef(0.0,height,0.0);                         // Um�st�n� m��e
  glRotatef(xrot,1.0,0.0,0.0);                          // Rotace na ose x
  glRotatef(yrot,0.0,1.0,0.0);                          // Rotace na ose y
  DrawObject;                                           // Vykresl� m��
  xrot := xrot + xrotspeed;                             // Zv�t�� nato�en�
  yrot := yrot + yrotspeed;                             // Zv�t�� nato�en�
  glFlush;                                              // Vypr�zdn� pipeline
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
      cStencilBits:= 1;                               // Stencil buffer (D�LE�IT�)
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

procedure ProcessKeyboard;                                // Ovl�d�n� kl�vesnic�
begin
  if keys[VK_RIGHT] then yrotspeed := yrotspeed + 0.08;   // �ipka vpravo zv��� rychlost y rotace
  if keys[VK_LEFT] then yrotspeed := yrotspeed - 0.08;    // �ipka vlevo sn�� rychlost y rotace
  if keys[VK_DOWN] then xrotspeed := xrotspeed + 0.08;    // �ipka dol� zv��� rychlost x rotace
  if keys[VK_UP] then xrotspeed := xrotspeed - 0.08;      // �ipka nahoru sn�� rychlost x rotace
  if keys[Ord('A')] then zoom := zoom + 0.05;             // A p�ibl�� sc�nu
  if keys[Ord('Z')] then zoom := zoom - 0.05;             // Z odd�l� sc�nu
  if keys[VK_PRIOR] then height := height + 0.03;         // Page Up zv�t�� vzd�lenost m��e nad podlahou
  if keys[VK_NEXT] then height := height - 0.03;          // Page Down zmen�� vzd�lenost m��e nad podlahou
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
          ProcessKeyboard;                            // Vstup z kl�vesnice
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

