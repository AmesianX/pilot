program lesson46a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL,
  NeHeGL in 'NeHeGL.pas',
  ArcBall in 'ArcBall.pas';

const
  PI2 = 2.0*3.1415926535;

var
  g_window: PGL_Window;                                         // Okno
  g_keys: PKeys;                                                // Kl�vesy
  quadratic: GLUquadricObj;
  Transform: Matrix4fT;                                         // Fin�ln� transformace
  LastRot, ThisRot: Matrix3fT;                                  // Minul� rotace, Sou�asn� rotace
  isDragging: boolean = false;                                  // T�hnuto my��?
  

function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
begin
  g_window := window;                                                           // Okno
  g_keys := key;                                                                // Kl�vesnice
  ZeroMemory(@Transform,sizeof(matrix4fT));
  Transform.M[0] := 1.0;
  Transform.s.M00 := 1.0;
  Transform.s.XX := 1.0;
  Transform.M[5] := 1.0;
  Transform.s.M11 := 1.0;
  Transform.s.YY := 1.0;
  Transform.M[10] := 1.0;
  Transform.s.M22 := 1.0;
  Transform.s.ZZ := 1.0;
  Transform.M[15] := 1.0;
  Transform.s.M33 := 1.0;
  ZeroMemory(@LastRot,sizeof(matrix3fT));
  LastRot.M[0] := 1.0;
  LastRot.s.M00 := 1.0;
  LastRot.s.XX := 1.0;
  LastRot.M[4] := 1.0;
  LastRot.s.M11 := 1.0;
  LastRot.s.YY := 1.0;
  LastRot.M[8] := 1.0;
  LastRot.s.M22 := 1.0;
  LastRot.s.ZZ := 1.0;
  ZeroMemory(@ThisRot,sizeof(matrix3fT));
  ThisRot.M[0] := 1.0;
  ThisRot.s.M00 := 1.0;
  ThisRot.s.XX := 1.0;
  ThisRot.M[4] := 1.0;
  ThisRot.s.M11 := 1.0;
  ThisRot.s.YY := 1.0;
  ThisRot.M[8] := 1.0;
  ThisRot.s.M22 := 1.0;
  ThisRot.s.ZZ := 1.0;
  glClearColor(0.0,0.0,0.0,0.5);
  glClearDepth(1.0);
  glDepthFunc(GL_LEQUAL);
	glEnable(GL_DEPTH_TEST);
	glShadeModel(GL_FLAT);
	glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
  quadratic := gluNewQuadric;
  gluQuadricNormals(quadratic,GLU_SMOOTH);
  gluQuadricTexture(quadratic,GL_TRUE);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  glEnable(GL_COLOR_MATERIAL);
  Result := true;                                                               // OK
end;

procedure Deinitialize;                                                         // Deinicializace
begin
  gluDeleteQuadric(quadratic);
  ArcBalls.Free;
end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohyb� ve sc�n� a stisk kl�ves
var
  ThisQuat: Quat4ft;
begin
  if g_keys.keyDown[VK_ESCAPE] then                                   // Kl�vesa ESC?
    TerminateApplication(g_window^);                                  // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                       // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                      // P�epnut� fullscreen/okno
  if isRClicked then                                                  // Kliknut� prav�m tla��tkem - reset v�ech rotac�
    begin
    Matrix3fSetIdentity(@LastRot);
    Matrix3fSetIdentity(@ThisRot);
    Matrix4fSetRotationFromMatrix3f(@Transform,@ThisRot);
    end;
  if not isDragging then                                              // Net�hne se my��?
    begin
    if isClicked then                                                 // Kliknut�?
      begin
      isDragging := true;                                             // P��prava na dragging
      LastRot := ThisRot;                                             // Nastaven� minul� statick� rotace na tuto
      ArcBalls.click(@MousePt);                                       // Aktualizace startovn�ho vektoru a p��prava na dragging
      end;
    end
    else                                                              // U� se t�hne
    if isClicked then                                                 // Je je�t� stisknuto tla��tko?
      begin
      ZeroMemory(@ThisQuat,sizeof(Quat4ft));
      ArcBalls.drag(@MousePt,@ThisQuat);                              // Aktualizace koncov�ho vektoru a z�sk�n� rotace jako quaternionu
      Matrix3fSetRotationFromQuat4f(@ThisRot,@ThisQuat);              // Konvertov�n� quaternionu na Matrix3fT
      Matrix3fMulMatrix3f(@ThisRot,@LastRot);                         // Akumulace minul� rotace do t�to
      Matrix4fSetRotationFromMatrix3f(@Transform,@ThisRot);           // Nastaven� koncov� transforma�n� rotace na tuto
      end
      else                                                            // U� nen� stisknuto
      isDragging := false;                                            // Konec draggingu
end;

procedure Torus(MinorRadius, MajorRadius: GLfloat);
var
  i, j: integer;
  wrapFrac, phi, sinphi, cosphi, r: GLfloat;
begin
	glBegin(GL_TRIANGLE_STRIP);									// Start A Triangle Strip
		for i := 0 to 19 do										// Stacks
			for j := -1 to 19 do									// Slices
			  begin
				wrapFrac := (j mod 20) / 20;
				phi := PI2 * wrapFrac;
				sinphi := sin(phi);
				cosphi := cos(phi);
				r := MajorRadius + MinorRadius * cosphi;
				glNormal3f(sin(PI2*(i mod 20+wrapFrac)/20)*cosphi,sinphi,cos(PI2*(i mod 20+wrapFrac)/20)*cosphi);
				glVertex3f(sin(PI2*(i mod 20+wrapFrac)/20)*r,MinorRadius*sinphi,cos(PI2*(i mod 20+wrapFrac)/20)*r);
				glNormal3f(sin(PI2*(i+1 mod 20+wrapFrac)/20)*cosphi,sinphi,cos(PI2*(i+1 mod 20+wrapFrac)/20)*cosphi);
				glVertex3f(sin(PI2*(i+1 mod 20+wrapFrac)/20)*r,MinorRadius*sinphi,cos(PI2*(i+1 mod 20+wrapFrac)/20)*r);
			  end;
	glEnd;
end;

procedure Draw;                                                       // Vykreslen� sc�ny
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity;	                                                    // Reset matice
  glTranslatef(-1.5,0.0,-6.0);                                        // Translace doleva a do hloubky
  glPushMatrix;                                                       // Ulo�en� matice
  glMultMatrixf(@Transform.M);                                        // Aplikov�n� transformace
  glColor3f(0.75,0.75,1.0);                                           // Barva
  Torus(0.30,1.00);                                                   // Vykreslen� toroidu (speci�ln� funkce)
  glPopMatrix;                                                        // Obnoven� p�vodn� matice
  glLoadIdentity;                                                     // Reset matice
  glTranslatef(1.5,0.0,-6.0);                                         // Translace doprava a do hloubky
  glPushMatrix;                                                       // Ulo�en� matice
  glMultMatrixf(@Transform.M);                                        // Aplikov�n� transformace
  glColor3f(1.0,0.75,0.75);                                           // Barva
  gluSphere(quadratic,1.3,20,20);                                     // Vykreslen� koule
  glPopMatrix;                                                        // Obnoven� matice
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
    WM_MOUSEMOVE:                                                   // Pohyb
      begin
      MousePt.s.X := LOWORD(lParam);
      MousePt.s.Y := HIWORD(lParam);
      isClicked := (LOWORD(wParam) = MK_LBUTTON);
      isRClicked := (LOWORD(wParam) = MK_RBUTTON);
      Result := 0;
      end;
    WM_LBUTTONUP:                                                   // Uvoln�n� lev�ho tla��tka
      begin
      isClicked := false;
      Result := 0;
      end;
    WM_RBUTTONUP:                                                   // Uvoln�n� prav�ho tla��tka
      begin
      isRClicked := false;
      Result := 0;
      end;
    WM_LBUTTONDOWN:                                                 // Kliknut� lev�m tla��tkem
      begin
      isClicked := true;
      Result := 0;
      end;
    WM_RBUTTONDOWN:                                                 // Kliknut� prav�m tla��tkem
      begin
      isRClicked := true;
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
    init.title := 'Lesson 48: NeHe & Terence J. Grant''s ArcBall Rotation Tutorial';
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
  ArcBalls := ArcBallT.Create(640,480);
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );                  // Start programu
end.

