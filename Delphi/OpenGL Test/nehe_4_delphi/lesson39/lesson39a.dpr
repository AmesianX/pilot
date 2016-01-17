program lesson39a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  SysUtils,
  OpenGL,
  NeHeGL in 'NeHeGL.pas',
  Physics in 'Physics.pas';

var
  g_window: PGL_Window;                                     // Okno
  g_keys: PKeys;                                            // Kl�vesy
  CV: ConstantVelocity;                                     // Objekt s konstantn� rzchlost�
  MUG: MotionUnderGravitation;                              // Objekt v gravita�n�m poli
  MCWS: MassConnectedWithSpring;                            // Objekt na pru�in�
  slowMotionRatio: GLfloat = 10.0;                          // Zpomalen� simulace
  timeElapsed: GLfloat = 0;                                 // Uplynul� �as
  base: GLuint;
  gmf: array [0..255] of GLYPHMETRICSFLOAT;


procedure BuildFont(window: PGL_Window);                  // Vytvo�en� fontu
var Font: HFONT;                                          // Prom�nn� fontu
begin
  base := glGenLists(256);                                // 256 znak�
  font := CreateFont(-12,                                 // V��ka
                      0,                                  // ���ka
                      0,                                  // �hel escapement
                      0,                                  // �hel orientace
                      FW_BOLD,                            // Tu�nost
                      0,                                  // Kurz�va
                      0,                                  // Podtr�en�
                      0,                                  // P�e�krtnut�
                      ANSI_CHARSET,                       // Znakov� sada
                      OUT_TT_PRECIS,                      // P�esnost v�stupu (TrueType)
                      CLIP_DEFAULT_PRECIS,                // P�esnost o�ez�n�
                      ANTIALIASED_QUALITY,                // V�stupn� kvalita
                      FF_DONTCARE or DEFAULT_PITCH,       // Rodina a pitch
                      nil);                               // Jm�no fontu
  SelectObject(window.hDc,font);                          // V�b�r fontu do DC
  wglUseFontOutlines(window.hDc,                          // Vybere DC
                      0,                                  // Po��te�n� znak
                      255,                                // Koncov� znak
                      base,                               // Adresa prvn�ho znaku
                      0,                                  // Hranatost
                      0.0,                                // Hloubka v ose z
                      WGL_FONT_POLYGONS,                  // Polygony ne dr�t�n� model
                      @gmf);                              // Adresa bufferu pro ulo�en� informac�
end;

procedure KillFont;                                       // Sma�e font
begin
  glDeleteLists(base,256);                                // Sma�e v�ech 256 znak� (display list�)
end;

procedure glPrint(x, y, z: GLfloat; text: string);
var
  delka: glfloat;                                                               // D�lka znaku
  loop: integer;                                                                // Cyklus
begin
  if text = '' then exit;                                                       // Byl p�ed�n text?
  delka := 0;
  for loop:=1 to length(text) do delka := delka + gmf[Ord(text[loop])].gmfCellIncX; // Inkrementace o ���ku znaku
  glTranslatef(x - delka,y,z);                                                  // Zarovn�n�
  glPushAttrib(GL_LIST_BIT);                                                    // Ulo�� sou�asn� stav display list�
  glListBase(base);                                                             // Nastav� prvn� display list na base
  glCallLists(length(text),GL_UNSIGNED_BYTE,Pchar(text));                       // Vykresl� display listy
  glPopAttrib;                                                                  // Obnov� p�vodn� stav display list�
  glTranslatef(-x,-y,-z);                                                       // Zp�t na sou�adnice p�ed v�pisem textu
end;

function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
begin
  g_window := window;
  g_keys := key;
  CV := ConstantVelocity.Create;                                                // Vytvo�en� objekt�
  MUG := MotionUnderGravitation.Create(Vektor(0.0,-9.81,0.0));
  MCWS := MassConnectedWithSpring.Create(2.0);
  glClearColor(0.0,0.0,0.0,0.5);                                                // �ern� pozad�
  glShadeModel(GL_SMOOTH);                                                      // Jemn� st�nov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);                             // Perspektivn� korekce
  BuildFont(window);                                                            // Vytvo�� font
  Result := true;                                                               // Inicializace �sp�n�
end;

procedure Deinitialize;                                                         // Deinicializace
begin
  KillFont;                                                                     // Zru�en� fontu
  CV.Release;                                                                   // Uvoln�n� prost�edk�
  CV.Free;
  CV := nil;
  MUG.Release;
  MUG.Free;
  MUG := nil;
  MCWS.Release;
  MCWS.Free;
  MCWS := nil;
end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohyb� ve sc�n� a stisk kl�ves
var
  dt: GLfloat;
  maxPossible_dt: GLfloat;
  numOfIterations: integer;
  a: integer;
begin
  if g_keys.keyDown[VK_ESCAPE] then                                   // Kl�vesa ESC?
    TerminateApplication(g_window^);                                  // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                       // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                      // P�epnut� fullscreen/okno
  if g_keys.keyDown[VK_F2] then                                       // Kl�vesa F2?
    slowMotionRatio := 1.0;                                           // Re�ln� �as
  if g_keys.keyDown[VK_F3] then                                       // Kl�vesa F3?
    slowMotionRatio := 10.0;                                          // Zpomalen� 10x
  dt := milliseconds / 1000.0;                                        // P�epo��t� milisekundy na sekundy
  dt := dt / slowMotionRatio;                                         // D�len� dt zpomalovac� prom�nnou
  timeElapsed := timeElapsed + dt;                                    // Zv�t�en� uplynul�ho �asu
  // Abychom nep�ekro�ili hranici kdy u� se simulace nechov� re�ln�
  maxPossible_dt := 0.1;                                              // Nastaven� maxim�ln� hodnoty dt na 0.1 sekund
  numOfIterations := Trunc(dt / maxPossible_dt) + 1;                  // V�po�et po�tu opakov�n� simulace v z�vislosti na dt a maxim�ln� mo�n� hodnot� dt
  if numOfIterations <> 0 then                                        // Vyhneme se d�len� nulou
    dt := dt / numOfIterations;                                       // dt by se m�la aktualizovat pomoc� numOfIterations
  for a := 0 to numOfIterations - 1 do                                // Simulaci pot�ebujeme opakovat numOfIterations-kr�t
    begin
    CV.operate(dt);                                                   // Proveden� simulace konstantn� rychlosti za dt sekund
    MUG.operate(dt);                                                  // Proveden� simulace pohybu v gravitaci za dt sekund
    MCWS.operate(dt);                                                 // Proveden� simulace pru�iny za dt sekund
    end;
end;

procedure Draw;                                                                 // Vykreslen� sc�ny
var
  x, y, a: integer;                                                             // ��d�c� prom�nn� cykl�
  massObj: Mass;
  pos: Vector3D;
begin
  glMatrixMode(GL_MODELVIEW);                                                   // Druh matice
	glLoadIdentity;                                                               // Reset
  gluLookAt(0,0,40,0,0,0,0,1,0);                                                // Nastaven� kamery
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                          // Sma�e obrazovku a hloubkov� buffer
  glColor3ub(0,0,255);										                                      // Modr�
	glBegin(GL_LINES);                                                            // Kreslen� �ar
    for x := -20 to 20 do                                                       // vertik�ln� ��ry v m��ce
      begin
      glVertex3f(x, 20,0);
		  glVertex3f(x,-20,0);
      end;
    for y := -20 to 20 do                                                       // Horizont�ln� ��ry v m��ce
      begin
      glVertex3f( 20,y,0);
		  glVertex3f(-20,y,0);
      end;
  glEnd;                                                                        // Konec kreslen� �ar
  glColor3ub(255,0,0);                                                          // �erven�
  for a := 0 to CV.numOfMasses - 1 do                                           // Vykresl� v�echny objekty s konstantn� rychlost�
    begin
    massObj := CV.getMass(a);
    pos := massObj.pos;
    glPrint(pos.x,pos.y + 1,pos.z,'Mass with constant vel');
		glPointSize(4);
		glBegin(GL_POINTS);
			glVertex3f(pos.x,pos.y,pos.z);
		glEnd;
    end;
  glColor3ub(255,255,0);                                                        // �lut�
  for a := 0 to MUG.numOfMasses - 1 do                                          // Vykresl� v�echny objekty v gravita�n�m poli
    begin
    massObj := MUG.getMass(a);
    pos := massObj.pos;
    glPrint(pos.x,pos.y + 1,pos.z,'Motion under gravitation');
		glPointSize(4);
		glBegin(GL_POINTS);
			glVertex3f(pos.x,pos.y,pos.z);
		glEnd;
    end;
  glColor3ub(0,255,0);                                                          // Zelen�
  for a := 0 to MCWS.numOfMasses - 1 do                                         // Vykresl� v�echny objekty na pru�in�
    begin
    massObj := MCWS.getMass(a);
    pos := massObj.pos;
    glPrint(pos.x,pos.y + 1,pos.z,'Mass connected with spring');
		glPointSize(8);
		glBegin(GL_POINTS);
			glVertex3f(pos.x,pos.y,pos.z);
		glEnd;
		// Vykreslen� pru�iny (��ra mezi objektem a �chytem)
		glBegin(GL_LINES);
			glVertex3f(pos.x,pos.y,pos.z);
			pos := MCWS.connectionPos;
			glVertex3f(pos.x,pos.y,pos.z);
		glEnd;
    end;
  glColor3ub(255,255,255);									                                    // B�l�
	glPrint(-5.0,14,0,Format('Time elapsed (seconds): %.2f',[timeElapsed]));	    // V�pis uplynul�ho �asu
	glPrint(-5.0,13,0,Format('Slow motion ratio: %.2f',[slowMotionRatio]));	      // V�pis zpomalen�
	glPrint(-5.0,12,0,'Press F2 for normal motion');
	glPrint(-5.0,11,0,'Press F3 for slow motion');
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
    init.title := 'NeHe & Erkin Tunca''s Physics Tutorial';
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

