program lesson40a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL,
  NeHeGL in 'NeHeGL.pas',
  Physics in 'Physics.pas',
  Physics2 in 'Physics2.pas';

var
  g_window: PGL_Window;                                     // Okno
  g_keys: PKeys;                                            // Kl�vesy
  RS: RopeSimulation;                                       // Lano


function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
begin
  g_window := window;
  g_keys := key;
  RS := RopeSimulation.Create(80,                                               // Vytvo�en� objektu simulace lana - 80 ��stic
                              0.05,                                             // Ka�d� ��stice v�� 50 gram�
                              10000.0,                                          // Tuhost pru�in
                              0.05,                                             // D�lka pru�in, p�i nep�soben� ��dn� sil
                              0.2,                                              // Konstanta vnit�n�ho t�en� pru�iny
                              Vektor(0,-9.81,0),                                // Gravita�n� zrychlen�
                              0.02,                                             // Odpor vzduchu
                              100.0,                                            // S�la odrazu od zem�
                              0.2,                                              // T�ec� s�la zem�
                              2.0,                                              // Absorb�n� s�la zem�
                              -1.5);                                            // Poloha zem� na ose y
  RS.getMass(RS.numOfMasses-1).vel.z := 10.0;                                   // Um�st�n� v hloubce
  glClearColor(0.0,0.0,0.0,0.5);                                                // �ern� pozad�
  glClearDepth(1.0);
  glShadeModel(GL_SMOOTH);                                                      // Jemn� st�nov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);                             // Perspektivn� korekce
  Result := true;                                                               // Inicializace �sp�n�
end;

procedure Deinitialize;                                                         // Deinicializace
begin
  RS.release;
  RS := nil;
end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohyb� ve sc�n� a stisk kl�ves
var
  ropeConnectionVel: Vector3D;
  dt: GLfloat;
  maxPossible_dt: GLfloat;
  numOfIterations: integer;
  a: integer;
begin
  ropeConnectionVel := Vektor(0,0,0);
  if g_keys.keyDown[VK_ESCAPE] then                                   // Kl�vesa ESC?
    TerminateApplication(g_window^);                                  // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                       // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                      // P�epnut� fullscreen/okno
  if g_keys.keyDown[VK_RIGHT] then                                    // Vpravo
    ropeConnectionVel.x := ropeConnectionVel.x + 3.0;
  if g_keys.keyDown[VK_LEFT] then                                     // Vlevo
    ropeConnectionVel.x := ropeConnectionVel.x - 3.0;
  if g_keys.keyDown[VK_UP] then                                       // Dozadu
    ropeConnectionVel.z := ropeConnectionVel.z - 3.0;
  if g_keys.keyDown[VK_DOWN] then                                     // Dop�edu
    ropeConnectionVel.z := ropeConnectionVel.z + 3.0;
  if g_keys.keyDown[VK_HOME] then                                     // Nahoru
    ropeConnectionVel.y := ropeConnectionVel.y + 3.0;
  if g_keys.keyDown[VK_END] then                                      // Dolu
    ropeConnectionVel.y := ropeConnectionVel.y - 3.0;
  RS.setRopeConnectionVel(ropeConnectionVel);                         // Vektor pohybu
  dt := milliseconds / 1000.0;                                        // P�epo��t� milisekundy na sekundy
  // Abychom nep�ekro�ili hranici kdy u� se simulace nechov� re�ln�
  maxPossible_dt := 0.002;                                            // Nastaven� maxim�ln� hodnoty dt na 0.1 sekund
  numOfIterations := Trunc(dt / maxPossible_dt) + 1;                  // V�po�et po�tu opakov�n� simulace v z�vislosti na dt a maxim�ln� mo�n� hodnot� dt
  if numOfIterations <> 0 then                                        // Vyhneme se d�len� nulou
    dt := dt / numOfIterations;                                       // dt by se m�la aktualizovat pomoc� numOfIterations
  for a := 0 to numOfIterations - 1 do                                // Simulaci pot�ebujeme opakovat numOfIterations-kr�t
    begin
    RS.operate(dt);                                                   // Krok simulace
    end;
end;

procedure Draw;                                                                 // Vykreslen� sc�ny
var
  a: integer;                                                                   // ��d�c� prom�nn� cykl�
  mass1, mass2: Mass;
  pos1, pos2: Vector3D;
begin
  glMatrixMode(GL_MODELVIEW);                                                   // Druh matice
	glLoadIdentity;                                                               // Reset
  gluLookAt(0,0,4,0,0,0,0,1,0);                                                 // Nastaven� kamery
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                          // Sma�e obrazovku a hloubkov� buffer
  glBegin(GL_QUADS);                                                            // Podlaha
		glColor3ub(0,0,255);
		glVertex3f(20,RS.groundHeight,20);
		glVertex3f(-20,RS.groundHeight,20);
		glColor3ub(0,0,0);
		glVertex3f(-20,RS.groundHeight,-20);
		glVertex3f(20,RS.groundHeight,-20);
	glEnd;
  glColor3ub(0,0,0);                                                            // St�n lana
  for a := 0 to RS.numOfMasses - 2 do
    begin
    mass1 := RS.getMass(a);
    pos1 := mass1.pos;
    mass2 := RS.getMass(a+1);
    pos2 := mass2.pos;
    glLineWidth(2);
		glBegin(GL_LINES);
			glVertex3f(pos1.x,RS.groundHeight,pos1.z);
			glVertex3f(pos2.x,RS.groundHeight,pos2.z);
		glEnd;
    end;
  glColor3ub(255,255,0);                                                        // Lano
  for a := 0 to RS.numOfMasses - 2 do
    begin
    mass1 := RS.getMass(a);
    pos1 := mass1.pos;
    mass2 := RS.getMass(a+1);
    pos2 := mass2.pos;
    glLineWidth(4);
		glBegin(GL_LINES);
			glVertex3f(pos1.x,pos1.y,pos1.z);
			glVertex3f(pos2.x,pos2.y,pos2.z);
		glEnd;
    end;
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
    init.title := 'NeHe & Erkin Tunca''s Rope Physics Tutorial';
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

