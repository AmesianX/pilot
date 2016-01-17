program lesson41a;

{   k�d pro Delphi 7}

//******************************************************************************
// P�vodn� verze funkce BuildTexture necht�la fungovat, tak jsem pou�il starou
// dobrou knihovnu glaux pro nahr�v�n� bitmapy. Kdyby n�kdo rozchodil ekvivalent
// funkce z C++, dejte mi v�d�t...
//******************************************************************************

uses
  Windows,
  glaux,
  Messages,
  OpenGL,
  NeHeGL;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); stdcall; external 'opengl32';

type
  PFNGLFOGCOORDFEXTPROC = procedure(coord: GLfloat); stdcall;   // Funk�n� prototyp

const
  GL_FOG_COORDINATE_SOURCE_EXT = $8450;                         // Symbolick� konstanty pot�ebn� pro roz���en� FogCoordfEXT
  GL_FOG_COORDINATE_EXT = $8451; 

var
  g_window: PGL_Window;                                         // Okno
  g_keys: PKeys;                                                // Kl�vesy
  fogColor: array [0..3] of GLfloat = (0.6,0.3,0.0,1.0);        // Barva mlhy
  camz: GLfloat;                                                // Pozice kamery na ose z
  glFogCoordfEXT: PFNGLFOGCOORDFEXTPROC = nil;                  // Ukazatel na funkci glFogCoordfEXT
  texture: GLuint;                                              // Jedna textura


function BuildTexture(szPathName: PChar; var texid: GLuint): boolean;           // Nahraje obr�zek a konvertuje ho na texturu
var
  TextureImage: PTAUX_RGBImageRec;
begin
  TextureImage := auxDIBImageLoadA(szPathName);
  if not Assigned(TextureImage) then
    begin
    Result := false;
    exit;
    end;
  glGenTextures(1,texid);                                                       // Generov�n� jedn� textury
  glBindTexture(GL_TEXTURE_2D,texid);                                           // Zvol� texturu
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);               // Line�rn� filtrov�n�
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D,0,3,TextureImage.sizeX,TextureImage.sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage.data);  // Vytvo�en� textury
  Result := true;                                                               // OK
end;

function Extension_Init: boolean;                                               // Je roz���en� EXT_fog_coord podporov�no?
var
  Extension_Name: string;
  glextstring: string;
begin
  Extension_Name := 'EXT_fog_coord';
  glextstring := glGetString(GL_EXTENSIONS);                                    // Grabov�n� seznamu podporovan�ch roz���en�
  if Pos(Extension_Name,glextstring) = 0 then                                   // Nen� podporov�no?
    begin
    Result := false;
    exit;
    end;
  glFogCoordfEXT := wglGetProcAddress('glFogCoordfEXT');                        // Nastav� ukazatel na funkci
  Result := true;                                                               // OK
end;

function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
begin
  g_window := window;                                                           // Okno
  g_keys := key;                                                                // Kl�vesnice
  if not Extension_Init then                                                    // Je roz���en� podporov�no?
    begin
    Result := false;                                                            // Konec
    exit;
    end;
  if not BuildTexture('data/wall.bmp',texture) then                             // Nahr�n� textury
    begin
    Result := false;                                                            // Konec
    exit;
    end;
  glEnable(GL_TEXTURE_2D);                                                      // Zapne mapov�n� textur
  glClearColor(0.0,0.0,0.0,0.5);                                                // �ern� pozad�
  glClearDepth(1.0);                                                            // Nastaven� hloubkov�ho bufferu
  glDepthFunc(GL_LEQUAL);                                                       // Typ testov�n� hloubky
  glEnable(GL_DEPTH_TEST);                                                      // Zapne testov�n� hloubky
  glShadeModel(GL_SMOOTH);                                                      // Jemn� st�nov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);                             // Nejlep�� perspektivn� korekce
  glEnable(GL_FOG);                                                             // Zapne mlhu
  glFogi(GL_FOG_MODE,GL_LINEAR);                                                // Line�rn� p�echody
  glFogfv(GL_FOG_COLOR,@fogColor);                                              // Barva
  glFogf(GL_FOG_START,0.0);                                                     // Po��tek
  glFogf(GL_FOG_END,1.0);                                                       // Konec
  glHint(GL_FOG_HINT,GL_NICEST);                                                // V�po�ty na jednotliv�ch pixelech
  glFogi(GL_FOG_COORDINATE_SOURCE_EXT,GL_FOG_COORDINATE_EXT);                   // Mlha v z�vislosti na sou�adnic�ch vertex�
  camz := -19.0;                                                                // Pozice kamery
  Result := true;                                                               // OK
end;

procedure Deinitialize;                                                         // Deinicializace
begin
  glDeleteTextures(1,@texture);                                                 // Sma�e texturu
end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohyb� ve sc�n� a stisk kl�ves
begin
  if g_keys.keyDown[VK_ESCAPE] then                                   // Kl�vesa ESC?
    TerminateApplication(g_window^);                                  // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                       // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                      // P�epnut� fullscreen/okno
  if g_keys.keyDown[VK_UP] and (camz < 14.0) then                     // �ipka nahoru
    begin
    camz := camz + milliseconds / 100.0;                              // Pohyb dop�edu
    end;
  if g_keys.keyDown[VK_DOWN] and (camz > -19.0) then                  // �ipka dol�
    begin
    camz := camz - milliseconds / 100.0;                              // Pohyb dozadu
    end;
end;

procedure Draw;                                                       // Vykreslen� sc�ny
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity;	                                                    // Reset matice
  glTranslatef(0.0,0.0,camz);                                         // Translace v hloubce
  glBegin(GL_QUADS);                                                  // Zadn� st�na
    glFogCoordfEXT(1.0); glTexCoord2f(0.0,0.0);glVertex3f(-2.5,-2.5,-15.0);
    glFogCoordfEXT(1.0); glTexCoord2f(1.0,0.0);glVertex3f( 2.5,-2.5,-15.0);
    glFogCoordfEXT(1.0); glTexCoord2f(1.0,1.0);glVertex3f( 2.5, 2.5,-15.0);
    glFogCoordfEXT(1.0); glTexCoord2f(0.0,1.0);glVertex3f(-2.5, 2.5,-15.0);
  glEnd;
  glBegin(GL_QUADS);                                                  // Podlaha
    glFogCoordfEXT(1.0); glTexCoord2f(0.0,0.0);glVertex3f(-2.5,-2.5,-15.0);
    glFogCoordfEXT(1.0); glTexCoord2f(1.0,0.0);glVertex3f( 2.5,-2.5,-15.0);
    glFogCoordfEXT(0.0); glTexCoord2f(1.0,1.0);glVertex3f( 2.5,-2.5, 15.0);
    glFogCoordfEXT(0.0); glTexCoord2f(0.0,1.0);glVertex3f(-2.5,-2.5, 15.0);
  glEnd;
  glBegin(GL_QUADS);                                                  // Strop
    glFogCoordfEXT(1.0); glTexCoord2f(0.0,0.0);glVertex3f(-2.5,2.5,-15.0);
    glFogCoordfEXT(1.0); glTexCoord2f(1.0,0.0);glVertex3f( 2.5,2.5,-15.0);
    glFogCoordfEXT(0.0); glTexCoord2f(1.0,1.0);glVertex3f( 2.5,2.5, 15.0);
    glFogCoordfEXT(0.0); glTexCoord2f(0.0,1.0);glVertex3f(-2.5,2.5, 15.0);
  glEnd;
  glBegin(GL_QUADS);                                                  // Prav� st�na
    glFogCoordfEXT(0.0); glTexCoord2f(0.0,0.0);glVertex3f(2.5,-2.5, 15.0);
    glFogCoordfEXT(0.0); glTexCoord2f(0.0,1.0);glVertex3f(2.5, 2.5, 15.0);
    glFogCoordfEXT(1.0); glTexCoord2f(1.0,1.0);glVertex3f(2.5, 2.5,-15.0);
    glFogCoordfEXT(1.0); glTexCoord2f(1.0,0.0);glVertex3f(2.5,-2.5,-15.0);
  glEnd;
  glBegin(GL_QUADS);                                                  // Lev� st�na
    glFogCoordfEXT(0.0); glTexCoord2f(0.0,0.0);glVertex3f(-2.5,-2.5, 15.0);
    glFogCoordfEXT(0.0); glTexCoord2f(0.0,1.0);glVertex3f(-2.5, 2.5, 15.0);
    glFogCoordfEXT(1.0); glTexCoord2f(1.0,1.0);glVertex3f(-2.5, 2.5,-15.0);
    glFogCoordfEXT(1.0); glTexCoord2f(1.0,0.0);glVertex3f(-2.5,-2.5,-15.0);
  glEnd;
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
  tickCount: DWORD;
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
    WM_PAINT:
      begin
      tickCount := GetTickCount;
      Update(tickCount - window.lastTickCount);
      window.lastTickCount := tickCount;
      Draw;
      SwapBuffers(window.hDc);
      Result := 0;
      end;
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
begin
  app.className := 'OpenGL';
  app.hInstance := hInstance;
  ZeroMemory(@window,Sizeof(window));
  with window do
    begin
    keys := @key;
    init.application := @app;
    init.title := 'Lesson 41: NeHe''s Volumetric Fog Tutorial';
    init.width := 640;
    init.height := 480;
    init.bitsPerPixel := 32;
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

