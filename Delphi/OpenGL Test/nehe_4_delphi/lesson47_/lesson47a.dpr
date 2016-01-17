program lesson47a;

{   k�d pro Delphi 7}

uses
  Windows,
  SysUtils,
  Messages,
  OpenGL,
  cg,
  cgGL,
  NeHeGL in 'NeHeGL.pas';

const
  TWO_PI = 6.2831853071;                                        // PI * 2
  SIZE = 64;                                                    // Velikost meshe

var
  g_window: PGL_Window;                                         // Okno
  g_keys: PKeys;                                                // Kl�vesy
  cg_enable: boolean = true;                                    // Flag spu�t�n� CG
  sp: boolean;
  mesh: array [0..SIZE-1,0..SIZE-1,0..2] of GLfloat;            // Data meshe
  wave_movement: GLfloat = 0.0;                                 // Pro vytvo�en� sinusov� vlny
  cgKontext: CGcontext;                                         // CG kontext
  cgPrg: CGprogram;                                             // CG vertex program
  cgVertexProfile: CGprofile;                                   // CG profil
  position, color, modelViewMatrix, wave: CGparameter;          // Parametry pro shader


function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
var
  x, z: integer;
  Error: CGerror;
begin
  g_window := window;                                                           // Okno
  g_keys := key;                                                                // Kl�vesnice
  glClearColor(0.0,0.0,0.0,0.5);                                                // �ern� pozad�
  glClearDepth(1.0);                                                            // Nastaven� hloubkov�ho bufferu
  glDepthFunc(GL_LEQUAL);                                                       // Typ testov�n� hloubky
  glEnable(GL_DEPTH_TEST);                                                      // Zapne testov�n� hloubky
  glShadeModel(GL_SMOOTH);                                                      // Jemn� st�nov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);                             // Nastaven� perspektivy
  glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);                                     // Dr�t�n� model
  for x := 0 to SIZE - 1 do                                                     // Inicializace meshe
    for z := 0 to SIZE - 1 do
      begin
      mesh[x,z,0] := (SIZE / 2) - x;                                            // Vycentrov�n� na ose x
      mesh[x,z,1] := 0.0;                                                       // Ploch� rovina
      mesh[x,z,2] := (SIZE / 2) - z;                                            // Vycentrov�n� na ose z
      end;
  cgKontext := cgCreateContext;                                                 // Vytvo�en� CG kontextu
  if cgKontext = nil then                                                       // OK?
    begin
    MessageBox(0,'Failed To Create Cg Context','Error',MB_OK);
    Result := false;
    exit;
    end;
  cgVertexProfile := cgGLGetLatestProfile(CG_GL_VERTEX);                        // Z�sk�n� minul�ho profilu vertex�
  if cgVertexProfile = CG_PROFILE_UNKNOWN then                                  // OK?
    begin
    MessageBox(0,'Invalid profile type','Error',MB_OK);
    Result := false;
    exit;
    end;
  cgGLSetOptimalOptions(cgVertexProfile);                                       // Nastaven� profilu
  // Nahraje a zkompiluje vertex shader
  cgPrg := cgCreateProgramFromFile(cgKontext,CG_SOURCE,'CG/Wave.cg',cgVertexProfile,'main',nil);
  if cgPrg = nil then                                                           // OK?
    begin
    Error := cgGetError;                                                        // Typ chyby
    MessageBox(0,cgGetErrorString(Error),'Error',MB_OK);
    Result := false;
    exit;
    end;
  cgGLLoadProgram(cgPrg);                                                       // Nahraje program do grafick� karty
  // Handle na prom�nn�
  position := cgGetNamedParameter(cgPrg,'IN.position');
  color := cgGetNamedParameter(cgPrg,'IN.color');
  wave := cgGetNamedParameter(cgPrg,'IN.wave');
  modelViewMatrix := cgGetNamedParameter(cgPrg,'ModelViewProj');
  Result := true;                                                               // OK
end;

procedure Deinitialize;                                                         // Deinicializace
begin
  cgDestroyContext(cgKontext);                                                  // Sma�e CG kontext
end;

procedure Update(milliseconds: GLfloat);                                        // Aktualizace pohyb� ve sc�n� a stisk kl�ves
begin
  if g_keys.keyDown[VK_ESCAPE] then                                             // Kl�vesa ESC?
    TerminateApplication(g_window^);                                            // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                                 // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                                // P�epnut� fullscreen/okno
  if g_keys.keyDown[Ord(' ')] and (not sp) then                                 // Mezern�k
    begin
    sp := true;
    cg_enable := not cg_enable;                                                 // Zapne/vypne CG program
    end;
  if not g_keys.keyDown[Ord(' ')] then                                          // Uvoln�n� mezern�ku
    begin
    sp := false;
    end;
end;

procedure Draw;                                                                 // Vykreslen� sc�ny
var
  x, z: integer;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                          // Sma�e obrazovku
  glLoadIdentity;                                                               // Reset matice
  gluLookAt(0.0,25.0,-45.0,0.0,0.0,0.0,0,1,0);                                  // Pozice kamery
  // Nastaven� modelview matice v shaderu
  cgGLSetStateMatrixParameter(modelViewMatrix,CG_GL_MODELVIEW_PROJECTION_MATRIX,CG_GL_MATRIX_IDENTITY);
  if cg_enable then                                                             // Zapnout CG shader?
    begin
    cgGLEnableProfile(cgVertexProfile);                                         // Zapne profil
    cgGLBindProgram(cgPrg);                                                     // Zvol� program
    cgGLSetParameter4f(color,0.5,1.0,0.5,1.0);                                  // Nastav� barvu (sv�tle zelen�)
    end;
  for x := 0 to SIZE - 2 do                                                     // Vykreslen� meshe
    begin
    glBegin(GL_TRIANGLE_STRIP);                                                 // Ka�d� prou�ek jedn�m triangle stripem
    for z := 0 to SIZE - 2 do
      begin
      cgGLSetParameter3f(wave,wave_movement,1.0,1.0);                           // Parametr vlny
      glVertex3f(mesh[x,z,0],mesh[x,z,1],mesh[x,z,2]);                          // Vertex
      glVertex3f(mesh[x+1,z,0],mesh[x+1,z,1],mesh[x+1,z,2]);                    // Vertex
      wave_movement := wave_movement + 0.00001;                                 // Inkrementace parametru vlny
      if wave_movement > TWO_PI then                                            // V�t�� ne� dv� p� (6,28)?
        wave_movement := 0.0;                                                   // Vynulovat
      end;
    glEnd;                                                                      // Konec triangle stripu
    end;
  if cg_enable then                                                             // Zapnut� CG shader?
    cgGLDisableProfile(cgVertexProfile);                                        // Vypne profil
	glFlush;                                                                      // Vypr�zdn� OpenGL renderovac� pipeline
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
  timer: Int64;
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
    WM_ERASEBKGND:
      begin
      Result := 0;
      end; 
    WM_PAINT:
      begin
      if window.hrTimer then
        begin
        QueryPerformanceCounter(timer);
				tickCount := timer;
        end
        else
        tickCount := GetTickCount;
      Update((tickCount - window.lastTickCount) * window.timerResolution * 1000.0);
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
    hbrBackground := COLOR_WINDOW+1;                                // Pozad� nen� nutn�
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
    init.title := 'Lesson 47: NeHe & Owen Bourne''s Cg Vertex Shader Tutorial';
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

