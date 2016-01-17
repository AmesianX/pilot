program lesson34a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL;

const
  MAP_SIZE = 1024;                  // Velikost .RAW obr�zku v��kov� mapy
  STEP_SIZE = 16;                   // Hustota grabov�n� pixel�
  HEIGHT_RATIO = 1.5;               // Zoom v��ky ter�nu na ose y

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  bRender: boolean = true;          // Polygony - true, dr�t�n� model - false
  scaleValue: GLfloat = 0.15;       // M���tko velikosti ter�nu (v�echny osy)
  g_HeightMap: array [0..(MAP_SIZE*MAP_SIZE)-1] of byte;  // Ukl�d� data v��kov� mapy

procedure LoadRawFile(strName: string; nSize: integer; var pHeightMap: array of byte);  // Nahraje .RAW soubor
var
  pFile: file of byte;                                                          // Soubor
  precteno: integer;                                                            // Pomocn� prom�nn�
begin
  AssignFile(pFile,strName);
  {$I-}
  Reset(pFile);                                                                 // Otev�en� souboru pro �ten� v bin�rn�m m�du
  {$I+}
  if IOResult <> 0 then                                                         // Otev�en� v po��dku?
    begin
    MessageBox(0,'Can''t Find The Height Map!','Error',MB_OK);
    exit;
    end;
  BlockRead(pFile,pHeightMap,nSize,precteno);                                  // Na�te soubor do pam�ti
  if precteno <> nSize then                                                     // Nastala chyba?
    MessageBox(0,'Failed To Get Data!','Error',MB_OK);                          
  CLoseFile(pFile);                                                             // Zav�en� souboru
end;

procedure ReSizeGLScene(Width: GLsizei; Height: GLsizei); // Zm�na velikosti a inicializace OpenGL okna
begin
  if (Height=0) then		                                  // Zabezpe�en� proti d�len� nulou
     Height:=1;                                           // Nastav� v��ku na jedna
  glViewport(0, 0, Width, Height);                        // Resetuje aktu�ln� nastaven�
  glMatrixMode(GL_PROJECTION);                            // Zvol� projek�n� matici
  glLoadIdentity();                                       // Reset matice
  gluPerspective(45.0,Width/Height,0.1,500.0);            // V�po�et perspektivy
  glMatrixMode(GL_MODELVIEW);                             // Zvol� matici Modelview
  glLoadIdentity;                                         // Reset matice
end;

function InitGL:bool;	                                              // V�echno nastaven� OpenGL
begin
  glShadeModel(GL_SMOOTH);			                                    // Povol� jemn� st�nov�n�
  glClearColor(0.0, 0.0, 0.0, 0.5);	  	                            // �ern� pozad�
  glClearDepth(1.0);				                                        // Nastaven� hloubkov�ho bufferu
  glEnable(GL_DEPTH_TEST);			                                    // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LEQUAL);				                                    // Typ hloubkov�ho testov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);                 // Nejlep�� perspektivn� korekce
  LoadRawFile('Data/Terrain.raw',MAP_SIZE*MAP_SIZE,g_HeightMap);   // Na�ten� dat v��kov� mapy
  Result:=true;                                                     // Inicializace prob�hla v po��dku
end;
 
function Height(var pHeightMap: array of byte; X, Y: integer): integer; // P�epo��t� 2D sou�adnice na 1D a vr�t� ulo�enou hodnotu
var
  tmpX, tmpY: integer;                                              // Pomocn� prom�nn�
begin
  tmpX := X mod MAP_SIZE;                                           // Proti p�ete�en� pam�ti
  tmpY := Y mod MAP_SIZE;
  Result := pHeightMap[(tmpY * MAP_SIZE) + tmpX];                   // Vr�t� hodnotu z pole
end;

procedure SetVertexColor(var pHeightMap: array of byte; X, Y: integer); // Z�sk� barvu v z�vislosti na v��ce
var
  fColor: GLfloat;                                                  // Barva
begin
  fColor := (Height(pHeightMap,X,Y) / 256) - 0.15;                  // Z�sk�n� hodnoty, p�epo�et do rozmez� 0.0 a� 1.0, ztmaven�
  glColor3f(0,0,fColor);                                            // Odst�ny modr� barvy
end;

procedure RenderHeightMap(var pHeightMap: array of byte);               // Renderuje ter�n
var
  arrayX, arrayY: integer;                                          // Pro proch�zen� polem
  x, y, z: integer;                                                 // Sou�adnice vertex�
begin
  arrayX := 0;
  arrayY := 0;
  if bRender then                                                   // Co chce u�ivatel renderovat?
    glBegin(GL_QUADS)                                               // Polygony
    else
    glBegin(GL_LINES);                                              // Dr�t�n� model
  while arrayX < MAP_SIZE do                                        // ��dky v��kov� mapy
    begin
    while arrayY < MAP_SIZE do                                      // Sloupce v��kov� mapy
      begin
      // Sou�adnice lev�ho doln�ho vertexu
      x := arrayX;
      y := Height(pHeightMap,arrayX,arrayY);
      z := arrayY;
      SetVertexColor(pHeightMap,x,z);                               // Barva vertexu
      glVertex3i(x,y,z);                                            // Definov�n� vertexu
      // Sou�adnice lev�ho horn�ho vertexu
      x := arrayX;
      y := Height(pHeightMap,arrayX,arrayY + STEP_SIZE);
      z := arrayY + STEP_SIZE;
      SetVertexColor(pHeightMap,x,z);                               // Barva vertexu
      glVertex3i(x,y,z);                                            // Definov�n� vertexu
      // Sou�adnice prav�ho horn�ho vertexu
      x := arrayX + STEP_SIZE;
      y := Height(pHeightMap,arrayX + STEP_SIZE,arrayY + STEP_SIZE);
      z := arrayY + STEP_SIZE;
      SetVertexColor(pHeightMap,x,z);                               // Barva vertexu
      glVertex3i(x,y,z);                                            // Definov�n� vertexu
      // Sou�adnice prav�ho doln�ho vertexu
      x := arrayX + STEP_SIZE;
      y := Height(pHeightMap,arrayX + STEP_SIZE,arrayY);
      z := arrayY;
      SetVertexColor(pHeightMap,x,z);                               // Barva vertexu
      glVertex3i(x,y,z);                                            // Definov�n� vertexu
      Inc(arrayY,STEP_SIZE);
      end;
    arrayY := 0;
    Inc(arrayX,STEP_SIZE);
    end;
  glEnd;                                                            // Konec kreslen�
  glColor4f(1.0,1.0,1.0,1.0);                                       // Reset barvy
end;

function DrawGLScene():bool;                                        // Vykreslov�n�
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);              // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity();	                                                // Reset matice
  gluLookAt(212,60,194,186,55,171,0,1,0);                           // Um�st�n� a nato�en� kamery
  glScalef(scaleValue,scaleValue * HEIGHT_RATIO,scaleValue);        // Zoom ter�nu
  RenderHeightMap(g_HeightMap);                                     // Renderov�n� ter�nu
  Result := true;                                                   // Vykreslen� prob�hlo v po��dku
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
    WM_LBUTTONDOWN:                                     // Lev� tla��tko my�i
      begin
      bRender := not bRender;                           // P�epne mezi polygony a dr�t�n�m modelem
      Result := 0;                                      // N�vrat do hlavn�ho cyklu programu
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
          if keys[VK_UP] then                         // �ipka nahoru
            scaleValue := scaleValue + 0.001;         // Vyv��� hory
          if keys[VK_DOWN] then                       // �ipka dol�
            scaleValue := scaleValue - 0.001;         // Sn�� hory
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

