program lesson42a;

{   k�d pro Delphi 7}

uses
  Windows,
  SysUtils,
  Messages,
  OpenGL,
  NeHeGL in 'NeHeGL.pas';

procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); stdcall; external 'opengl32';

const
  width = 128;                                                  // ���ka textury (mus� b�t mocninou ��sla 2)
  height = 128;                                                 // V��ka textury (mus� b�t mocninou ��sla 2)

var
  g_window: PGL_Window;                                         // Okno
  g_keys: PKeys;                                                // Kl�vesy
  mx, my: integer;                                              // ��d�c� prom�nn� cykl�
  done: boolean;                                                // Bludi�t� vygenerov�no?
  sp: boolean;                                                  // Flag stisku mezern�ku
  r, g, b: array [0..3] of Byte;                                // �ty�i n�hodn� barvy
  tex_data: array of Byte;                                      // Data textury
  xrot, yrot, zrot: GLfloat;                                    // �hly rotac� objekt�
  quadric: GLUquadricObj;                                       // Objekt quadraticu


procedure UpdateTex(dmx, dmy: integer);                                         // Zab�l� ur�en� pixel na textu�e
begin
  tex_data[0 + ((dmx + (width * dmy)) * 3)] := 255;                             // �erven� slo�ka
  tex_data[1 + ((dmx + (width * dmy)) * 3)] := 255;                             // Zelen� slo�ka
  tex_data[2 + ((dmx + (width * dmy)) * 3)] := 255;                             // Modr� slo�ka
end;

procedure Reset;                                                                // Reset textury, barev, aktu�ln� pozice v bludi�ti
var
  loop: integer;
begin
  ZeroMemory(tex_data,width * height * 3);                                      // Nuluje pam� textury
  RandSeed := GetTickCount;                                                     // Inicializace gener�toru n�hodn�ch ��sel
  for loop := 0 to 3 do                                                         // Generuje �ty�i n�hodn� barvy
    begin
    r[loop] := Random(128) + 128;                                               // �erven� slo�ka
    g[loop] := Random(128) + 128;                                               // Zelen� slo�ka
    b[loop] := Random(128) + 128;                                               // Modr� slo�ka
    end;
  mx := Random(width div 2) * 2;                                                // N�hodn� x pozice
  my := Random(height div 2) * 2;                                               // N�hodn� y pozice
end;

function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
begin
  SetLength(tex_data,width * height * 3);                                       // Alokace pam�ti pro texturu
  g_window := window;                                                           // Okno
  g_keys := key;                                                                // Kl�vesnice
  Reset;
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP);                    // Clamp parametry textury
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);               // Line�rn� filtrov�n�
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D,0,GL_RGB,width,height,0,GL_RGB,GL_UNSIGNED_BYTE,tex_data); // Vytvo�� texturu
  glClearColor(0.0,0.0,0.0,0.0);                                                // �ern� pozad�
  glClearDepth(1.0);                                                            // Nastaven� hloubkov�ho bufferu
  glDepthFunc(GL_LEQUAL);                                                       // Typ testov�n� hloubky
  glEnable(GL_DEPTH_TEST);                                                      // Zapne testov�n� hloubky
  glEnable(GL_COLOR_MATERIAL);                                                  // Zapne vybarvov�n� materi�l�
  glEnable(GL_TEXTURE_2D);                                                      // Zapne mapov�n� textur
  quadric := gluNewQuadric;                                                     // Vytvo�� objekt quadraticu
  gluQuadricNormals(quadric,GLU_SMOOTH);                                        // Norm�ly pro sv�tlo
  gluQuadricTexture(quadric,GL_TRUE);                                           // Texturov� koordin�ty
  glEnable(GL_LIGHT0);                                                          // Zapne sv�tlo 0
  Result := true;                                                               // OK
end;

procedure Deinitialize;                                                         // Deinicializace
begin
  SetLength(tex_data,0);                                                        // Sma�e data textury
end;

procedure Update(milliseconds: GLfloat);                                        // Aktualizace pohyb� ve sc�n� a stisk kl�ves
var
  dir: integer;                                                                 // Ukl�d� aktu�ln� sm�r pohybu
  x, y: integer;
begin
  if g_keys.keyDown[VK_ESCAPE] then                                             // Kl�vesa ESC?
    TerminateApplication(g_window^);                                            // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                                 // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                                // P�epnut� fullscreen/okno
  if g_keys.keyDown[Ord(' ')] and (not sp) then                                 // Mezern�k
    begin
    sp := true;
    Reset;                                                                      // Resetuje sc�nu
    end;
  if not g_keys.keyDown[Ord(' ')] then                                          // Uvoln�n� mezern�ku
    begin
    sp := false;
    end;
  xrot := xrot + milliseconds * 0.02;                                           // Aktualizace �hl� nato�en�
  yrot := yrot + milliseconds * 0.03;
  zrot := zrot + milliseconds * 0.015;
  done := true;                                                                 // P�edpokl�d� se, �e je u� bludi�t� kompletn�
  x := 0;
  repeat                                                                        // Proch�z� v�echny m�stnosti na ose x
    y := 0;
    repeat                                                                      // Proch�z� v�echny m�stnosti na ose y
    if tex_data[((x + (width * y)) * 3)] = 0 then                                                            // Pokud m� pixel �ernou barvu
      done := false;                                                            // Bludi�t� je�t� nen� hotov�
    Inc(y,2);
    until y >= height;
  Inc(x,2);
  until x >= width;
  if done then                                                                  // Je bludi�t� hotov�?
    begin                                                                       // Zm�na titulku okna
    SetWindowText(g_window.hWnd,'Lesson 42: Multiple Viewports... 2003 NeHe Productions... Maze Complete!');
    Sleep(5000);                                                                // Zastaven� na p�t sekund
    SetWindowText(g_window.hWnd,'Lesson 42: Multiple Viewports... 2003 NeHe Productions... Building Maze!');
    Reset;                                                                      // Reset bludi�t� a sc�ny
    end;
  if (((tex_data[(((mx+2)+(width*my))*3)] = 255) or (mx >(width-4)))            // M�me kam j�t?
      and ((tex_data[(((mx-2)+(width*my))*3)] = 255) or (mx < 2))
      and ((tex_data[((mx+(width*(my+2)))*3)] = 255) or (my > (height-4)))
      and ((tex_data[((mx+(width*(my-2)))*3)] = 255) or( my <2 ))) then
    begin
    repeat
    mx := Random(width div 2) * 2;                                              // Nov� pozice
    my := Random(height div 2) * 2;
    until tex_data[((mx + (width * my)) * 3)] <> 0;                             // Hled� se nav�t�ven� m�stnost
    end;
  dir := Random(4);                                                             // N�hodn� sm�r pohybu
  if (dir = 0) and (mx <= (width-4)) then                                       // Sm�r doprava; vpravo je m�sto
    if tex_data[(((mx+2) + (width*my)) * 3)] = 0 then                           // M�stnost vpravo je�t� nebyla nav�t�vena
      begin
      UpdateTex(mx+1, my);                                                      // Ozna�� pr�chod mezi m�stnostmi
      mx := mx + 2;                                                             // Posunut� doprava
      end;
  if (dir = 1) and (my <= (height-4)) then                                      // Sm�r dol�; dole je m�sto
    if tex_data[((mx + (width * (my+2))) * 3)] = 0 then                         // M�stnost dole je�t� nebyla nav�t�vena
    begin
    UpdateTex(mx, my+1);                                                        // Ozna�� pr�chod mezi m�stnostmi
    my := my + 2;                                                               // Posunut� dol�
    end;
  if (dir = 2) and (mx >= 2) then                                               // Sm�r doleva; vlevo je m�sto
    if tex_data[(((mx-2) + (width*my)) * 3)] = 0 then                           // M�stnost vlevo je�t� nebyla nav�t�vena
    begin
    UpdateTex(mx-1, my);                                                        // Ozna�� pr�chod mezi m�stnostmi
    mx := mx - 2;                                                               // Posunut� doleva
    end;
  if (dir = 3) and (my >= 2) then                                               // Sm�r nahoru; naho�e je m�sto
    if tex_data[((mx + (width * (my-2))) * 3)] = 0 then                         // M�stnost naho�e je�t� nebyla nav�t�vena
    begin
    UpdateTex(mx, my-1);                                                        // Ozna�� pr�chod mezi m�stnostmi
    my := my - 2;                                                               // Posunut� nahoru
    end;                                                                        // Ozna�en� nov� m�stnosti
  UpdateTex(mx,my);
end;

procedure Draw;                                                                 // Vykreslen� sc�ny
var
  rect: TRect;                                                                  // Struktura obd�ln�ku
  window_width: integer;                                                        // ���ka okna
  window_height: integer;                                                       // V��ka okna
  loop: integer;
begin
  GetClientRect(g_window.hWnd,rect);                                            // Grabov�n� rozm�r� okna
  window_width := rect.Right - rect.Left;                                       // ���ka okna
  window_height := rect.Bottom - rect.Top;                                      // V��ka okna
  glTexSubImage2D(GL_TEXTURE_2D,0,0,0,width,height,GL_RGB,GL_UNSIGNED_BYTE,tex_data); // Zvol� aktualizovanou texturu
  glClear(GL_COLOR_BUFFER_BIT);                                                 // Sma�e obrazovku
  for loop := 0 to 3 do                                                         // Proch�z� viewporty
    begin
    glColor3ub(r[loop],g[loop],b[loop]);                                        // Barva
    if loop = 0 then                                                            // Prvn� sc�na
      begin
      glViewport(0,window_height div 2,window_width div 2,window_height div 2); // Lev� horn� viewport, velikost poloviny okna
      glMatrixMode(GL_PROJECTION);                                              // Projek�n� matice
      glLoadIdentity;                                                           // Reset projek�n� matice
      gluOrtho2D(0,window_width div 2,window_height div 2,0);                   // Pravo�hl� projekce
      end;
    if loop = 1 then                                                            // Druh� sc�na
      begin
      glViewport(window_width div 2,window_height div 2,window_width div 2,window_height div 2);  // Prav� horn� viewport, velikost poloviny okna
      glMatrixMode(GL_PROJECTION);                                              // Projek�n� matice
      glLoadIdentity;                                                           // Reset projek�n� matice
      gluPerspective(45.0,width / height,0.1,500.0);                            // Perspektivn� projekce
      end;
    if loop = 2 then                                                            // T�et� sc�na
      begin
      glViewport(window_width div 2,0,window_width div 2,window_height div 2);  // Prav� doln� viewport, velikost poloviny okna
      glMatrixMode(GL_PROJECTION);                                              // Projek�n� matice
      glLoadIdentity;                                                           // Reset projek�n� matice
      gluPerspective(45.0,width / height,0.1,500.0);                            // Perspektivn� projekce
      end;
    if loop = 3 then                                                            // �tvrt� sc�na
      begin
      glViewport(0,0,window_width div 2,window_height div 2);                   // Lev� doln� viewport, velikost poloviny okna
      glMatrixMode(GL_PROJECTION);                                              // Projek�n� matice
      glLoadIdentity;                                                           // Reset projek�n� matice
      gluPerspective(45.0,width / height,0.1,500.0);                            // Perspektivn� projekce
      end;
    glMatrixMode(GL_MODELVIEW);                                                 // Matice modelview
    glLoadIdentity;                                                             // Reset matice
    glClear(GL_DEPTH_BUFFER_BIT);                                               // Sma�e hloubkov� buffer
    if loop = 0 then                                                            // Prvn� sc�na, bludi�t� p�es cel� viewport
      begin
      glBegin(GL_QUADS);
        glTexCoord2f(1.0,0.0); glVertex2i(window_width div 2,0);
        glTexCoord2f(0.0,0.0); glVertex2i(0,0);
        glTexCoord2f(0.0,1.0); glVertex2i(0,window_height div 2);
        glTexCoord2f(1.0,1.0); glVertex2i(window_width div 2,window_height div 2);
      glEnd;
      end;
    if loop = 1 then                                                            // Druh� sc�na, koule
      begin
      glTranslatef(0.0,0.0,-14.0);                                              // P�esun do hloubky
      glRotatef(xrot,1.0,0.0,0.0);                                              // Rotace
      glRotatef(yrot,0.0,1.0,0.0);
      glRotatef(zrot,0.0,0.0,1.0);
      glEnable(GL_LIGHTING);                                                    // Zapne sv�tlo
      gluSphere(quadric,4.0,32,32);                                             // Koule
      glDisable(GL_LIGHTING);                                                   // Vypne sv�tlo
      end;
    if loop = 2 then                                                            // T�et� sc�na, bludi�t� na rovin�
      begin
      glTranslatef(0.0,0.0,-2.0);                                               // P�esun do hloubky
      glRotatef(-45.0,1.0,0.0,0.0);                                             // Rotace o 45 stup��
      glRotatef(zrot / 1.5,0.0,0.0,1.0);                                        // Rotace na ose z
      glBegin(GL_QUADS);
        glTexCoord2f(1.0,1.0); glVertex3f( 1.0, 1.0,0.0);
        glTexCoord2f(0.0,1.0); glVertex3f(-1.0, 1.0,0.0);
        glTexCoord2f(0.0,0.0); glVertex3f(-1.0,-1.0,0.0);
        glTexCoord2f(1.0,0.0); glVertex3f( 1.0,-1.0,0.0);
      glEnd;
      end;
    if loop = 3 then                                                            // T�et� sc�na, v�lec
      begin
      glTranslatef(0.0,0.0,-7.0);                                               // P�esun do hloubky
      glRotatef(-xrot/2,1.0,0.0,0.0);                                           // Rotace
      glRotatef(-yrot/2,0.0,1.0,0.0);
      glRotatef(-zrot/2,0.0,0.0,1.0);
      glEnable(GL_LIGHTING);                                                    // Zapne sv�tlo
      glTranslatef(0.0,0.0,-2.0);                                               // Vycentrov�n�
      gluCylinder(quadric,1.5,1.5,4.0,32,16);                                   // V�lec
      glDisable(GL_LIGHTING);                                                   // Vypne sv�tlo
      end;
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
    init.title := 'Lesson 42: Multiple Viewports... 2003 NeHe Productions... Building Maze!';
    init.width := 1024;
    init.height := 768;
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

