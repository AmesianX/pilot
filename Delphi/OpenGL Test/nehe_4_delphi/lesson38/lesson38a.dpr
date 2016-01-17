program lesson38a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL,
  NeHeGL in 'NeHeGL.pas';

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); stdcall; external 'opengl32';

type
  Objekt = record                                           // Struktura nazvan� objekt
    tex: integer;                                           // Kterou texturu namapovat
    x, y, z: GLfloat;                                       // X, Y, Z Pozice
    yi: GLfloat;                                            // Rychlost p�du
    spinz: GLfloat;                                         // �hel oto�en� kolem osy z
    spinzi: GLfloat;                                        // Rychlost ot��en� kolem osy z
    flap: GLfloat;                                          // M�v�n� k��dly
    fi: GLfloat;                                            // Sm�r m�v�n�
    end;

const
  GL_BGR_EXT = $80E0;                                       // Extension konstanta z jednotky gl.pas, abych ji nemusel celou linkovat

var
  g_window: PGL_Window;                                     // Okno
  g_keys: PKeys;                                            // Kl�vesy
  texture: array [0..2] of GLuint;                          // M�sto pro 3 textury
  obj: array [0..49] of Objekt;                             // Vytvo�� 50 objekt� na b�zi struktury


{$R zdroje.res}                                             // Soubor pro Resource (*D�LE�IT�*) - tam jsou na�i mot�li

procedure SetObject(loop: integer);                         // Nastaven� z�kladn�ch vlastnost� objektu
begin
  with obj[loop] do
    begin
    tex := Random(3);                                       // V�b�r jedn� ze t�� textur
    x := Random(34) - 17.0;                                 // N�hodn� x od -17.0 do 17.0
    y := 18.0;                                              // Pozici y nastav�me na 18 (nad obrazovku)
    z := -((Random(30000) / 1000.0) + 10.0);                // N�hodn� z od -10.0 do -40.0
    spinzi := Random(10000) / 5000.0 - 1.0;                 // Spinzi je n�hodn� ��slo od -1.0 do 1.0
    flap := 0.0;                                            // Flap za�ne na 0.0
    fi := 0.05 + Random(100) / 1000.0;                      // Fi je n�hodn� ��slo od 0.05 do 0.15
    yi := 0.001 + Random(1000) / 10000.0;                   // Yi je n�hodn� ��slo od 0.001 do 0.101
    end;
end;

procedure LoadGLTextures;                                                       // Vytvo�� textury z bitmap ve zdrojov�m souboru
var
  hBMP: HBITMAP;                                                                // Ukazatel na bitmapu
  BMP: BITMAP;                                                                  // Struktura bitmapy
  loop: integer;
  TextureID: array [0..2] of PAnsiChar;                                         // ID bitmap
begin
  TextureID[0] := 'BUTTERFLY1';                                                 // ID bitmap, kter� chceme na��st
  TextureID[1] := 'BUTTERFLY2';
  TextureID[2] := 'BUTTERFLY3';
  glGenTextures(3,texture[0]);                                                  // Vygenerov�n� t�� textur
  for loop := 0 to 2 do                                                         // Projde v�echny bitmapy ve zdroj�ch
    begin
    hBMP := LoadImage(GetModuleHandle(nil),TextureID[loop],IMAGE_BITMAP,0,0,LR_CREATEDIBSECTION); // Nahraje bitmapu ze zdroj�
    if hBMP <> 0 then                                                           // Pokud existuje bitmapa
      begin
      GetObject(hBMP,sizeof(BMP),@BMP);                                         // Z�sk�n� objektu
      glPixelStorei(GL_UNPACK_ALIGNMENT,4);                                     // 4 byty na jeden pixel
      glBindTexture(GL_TEXTURE_2D,texture[loop]);                               // Zvol� texturu
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);           // Line�rn� filtrov�n�
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR); // Mipmapovan� line�rn� filtrov�n�
      gluBuild2DMipmaps(GL_TEXTURE_2D,3,BMP.bmWidth,BMP.bmHeight,GL_BGR_EXT,GL_UNSIGNED_BYTE,BMP.bmBits); // Vygenerov�n� mipmapovan� textury (3 byty, ���ka, v��ka a BMP data)
      DeleteObject(hBMP);                                                       // Sma�e objekt bitmapy
      end;
    end;
end;

function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
var
  loop: integer;                                                                // ��d�c� prom�nn� cykl�
begin
  g_window := window;
  g_keys := key;
  LoadGLTextures;                                                               // Nahraje textury ze zdroj�
  glClearColor(0.0,0.0,0.0,0.5);                                                // �ern� pozad�
  glClearDepth(1.0);                                                            // Nastaven� hloubkov�ho bufferu
  glDepthFunc(GL_LEQUAL);                                                       // Typ testov�n� hloubky
  glDisable(GL_DEPTH_TEST);                                                     // Vypnut� hloubkov�ho testov�n�
  glShadeModel(GL_SMOOTH);                                                      // Jemn� st�nov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);                             // Perspektivn� korekce
  glEnable(GL_TEXTURE_2D);                                                      // Povol� texturov� mapov�n�
  glBlendFunc(GL_ONE,GL_SRC_ALPHA);                                             // Nastaven� blendingu (nen�ro�n� / rychl�)
  glEnable(GL_BLEND);                                                           // Povolen� blendingu
  for loop := 0 to 49 do SetObject(loop);                                       // Inicializace 50 mot�l�
  Result := true;                                                               // Inicializace �sp�n�
end;

procedure Deinitialize;                                                         // Deinicializace
begin
  glDeleteTextures(3,@texture);                                                 // Sma�e textury
end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohyb� ve sc�n� a stisk kl�ves
begin
  if g_keys.keyDown[VK_ESCAPE] then                                   // Kl�vesa ESC?
    TerminateApplication(g_window^);                                  // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                       // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                      // P�epnut� fullscreen/okno
end;

procedure Draw;                                                       // Vykreslen� sc�ny
var
  loop: integer;                                                      // ��d�c� prom�nn� cykl�
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                // Sma�e obrazovku a hloubkov� buffer
  for loop := 0 to 49 do                                              // Projde 50 mot�lk�
    begin
    glLoadIdentity;                                                   // Reset matice
    glBindTexture(GL_TEXTURE_2D,texture[obj[loop].tex]);              // Zvol� texturu
    glTranslatef(obj[loop].x,obj[loop].y,obj[loop].z);                // Um�st�n�
    glRotatef(45.0,1.0,0.0,0.0);                                      // Rotace na ose x
    glRotatef(obj[loop].spinz,0.0,0.0,1.0);                           // Rotace na ose y
    glBegin(GL_TRIANGLES);                                            // Kreslen� troj�heln�k�
      // Prvn� troj�heln�k
      glTexCoord2f(1.0,1.0); glVertex3f( 1.0, 1.0, 0.0);              // Prav� horn� bod
      glTexCoord2f(0.0,1.0); glVertex3f(-1.0, 1.0, obj[loop].flap);   // Lev� horn� bod
      glTexCoord2f(0.0,0.0); glVertex3f(-1.0,-1.0, 0.0);              // Lev� doln� bod
      // Druh� troj�heln�k
      glTexCoord2f(1.0,1.0); glVertex3f( 1.0, 1.0, 0.0);              // Prav� horn� bod
      glTexCoord2f(0.0,0.0); glVertex3f(-1.0,-1.0, 0.0);              // Lev� doln� bod
      glTexCoord2f(1.0,0.0); glVertex3f( 1.0,-1.0, obj[loop].flap);   // Prav� doln� bod
    glEnd;                                                            // Konec kreslen�
    obj[loop].y := obj[loop].y - obj[loop].yi;                        // P�d mot�la dol�
    obj[loop].spinz := obj[loop].spinz + obj[loop].spinzi;            // Zv��en� nato�en� na ose z o spinzi
    obj[loop].flap := obj[loop].flap + obj[loop].fi;                  // Zv�t�en� m�chnut� k��dlem o fi
    if obj[loop].y < -18.0 then                                       // Je mot�l mimo obrazovku?
      SetObject(loop);                                                // Nastav�me mu nov� parametry
    if (obj[loop].flap > 1.0) or (obj[loop].flap < -1.0) then         // M�me zm�nit sm�r m�vnut� k��dly
      obj[loop].fi := - obj[loop].fi;                                 // Zm�n� sm�r m�vnut�
    end;
  Sleep(15);                                                          // Pozastaven� programu na 15 milisekund
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
    init.title := 'NeHe''s Resource File Tutorial';
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

