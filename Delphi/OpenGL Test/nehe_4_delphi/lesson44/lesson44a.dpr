program lesson44a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL,
  SysUtils,
  Font in 'Font.pas',
  Camera in 'Camera.pas';

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;

const
  GL_BGR_EXT = $80E0;               // Opengl roz���en�
  
var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  infoOn: boolean = false;          // Zobrazit v�pis informac�?
  gFrames: integer = 0;
  gStartTime: DWORD;                // Po��te�n� �as
  gCurrentTime: DWORD;
  gFPS: GLfloat;                    // Sn�mkov� frekvence
  gFont: glFont;                    // Font
  gCamera: glCamera;                // Kamera
  qobj: GLUquadricObj;              // V�lec
  cylList: GLint;                   // Display list v�lce


function LoadTexture(szFileName: LPTSTR; var texid: GLuint): boolean;           // Vytvo�� texturu z bitmapov�ho obr�zku
var
  hBMP: HBITMAP;                                                                // Ukazatel na bitmapu
  BMP: BITMAP;                                                                  // Struktura bitmapy
begin
  glGenTextures(1,texid);                                                       // Vytvo�� texturu
  hBMP := LoadImage(GetModuleHandle(nil),szFileName,IMAGE_BITMAP,0,0,LR_CREATEDIBSECTION or LR_LOADFROMFILE);
  if hBMP = 0 then                                                              // Existuje bitmapa?
    begin
    Result := false;                                                            // Pokud ne, tak konec
    exit;
    end;
  GetObject(hBMP,sizeof(BMP),@BMP);                                             // Vypln� strukturu bitmapy
  glPixelStorei(GL_UNPACK_ALIGNMENT,4);							                            // Druh ukl�d�n� pixel� (Word / 4 Byty na pixel)
	glBindTexture(GL_TEXTURE_2D,texid);								                            // Zvol� aktu�ln� texturu
	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);	              // Line�ln� filtrov�n�
	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
	glTexImage2D(GL_TEXTURE_2D,0,3,BMP.bmWidth,BMP.bmHeight,0,GL_BGR_EXT,GL_UNSIGNED_BYTE,BMP.bmBits);
	DeleteObject(hBMP);												                                    // Zru�� objekt
	Result := true;													                                      // OK
end;

procedure ReSizeGLScene(Width: GLsizei; Height: GLsizei); // Zm�na velikosti a inicializace OpenGL okna
begin
  gCamera.m_WindowHeight := Height;                       // Nastav� velikost okna pro kameru
  gCamera.m_WindowWidth := Width;
  if (Height=0) then		                                  // Zabezpe�en� proti d�len� nulou
     Height:=1;                                           // Nastav� v��ku na jedna
  glViewport(0, 0, Width, Height);                        // Resetuje aktu�ln� nastaven�
  glMatrixMode(GL_PROJECTION);                            // Zvol� projek�n� matici
  glLoadIdentity;                                         // Reset matice
  gluPerspective(45.0,Width/Height,1.0,1000.0);           // V�po�et perspektivy
  glMatrixMode(GL_MODELVIEW);                             // Zvol� matici Modelview
  glLoadIdentity;                                         // Reset matice
end;


function InitGL:bool;	                                                          // V�echno nastaven� OpenGL
var
  tex: GLuint;
begin
  tex := 0;
  glShadeModel(GL_SMOOTH);			                                                // Povol� jemn� st�nov�n�
  glClearColor(0.0, 0.0, 0.0, 0.5);	  	                                        // �ern� pozad�
  glClearDepth(1.0);				                                                    // Nastaven� hloubkov�ho bufferu
  glEnable(GL_DEPTH_TEST);			                                                // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LEQUAL);				                                                // Typ hloubkov�ho testov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);                             // Nejlep�� perspektivn� korekce
  if LoadTexture('Art/Font.bmp',tex) then                                       // Nahraje texturu fontu
    begin
    gFont.SetFontTexture(tex);									                                // Nastav� texturu fontu
		gFont.SetWindowSize(1024,768);								                              // Velikost okna pro font
		gFont.BuildFont(1.0);                                                       // Vytvo�� font
    end
    else
    MessageBox(0,'Failed to load font texture.','Error',MB_OK);                 // Chyba
  gCamera.m_MaxHeadingRate := 1.0;								                              // Maxim�ln� hodnoty pro kameru
	gCamera.m_MaxPitchRate := 1.0;
	gCamera.m_HeadingDegrees := 0.0;
	if not LoadTexture('Art/HardGlow2.bmp',gCamera.m_GlowTexture) then            // Nahr�n� textur pro efekty
    begin
		MessageBox(0,'Failed to load Hard Glow texture.','Error',MB_OK);
		Result := false;
    exit;
    end;
	if not LoadTexture('Art/BigGlow3.bmp',gCamera.m_BigGlowTexture) then
    begin
		MessageBox(0,'Failed to load Big Glow texture.','Error',MB_OK);
    Result := false;
    exit;
    end;
	if not LoadTexture('Art/Halo3.bmp',gCamera.m_HaloTexture) then
    begin
		MessageBox(0,'Failed to load Halo texture.','Error',MB_OK);
    Result := false;
    exit;
    end;
	if not LoadTexture('Art/Streaks4.bmp',gCamera.m_StreakTexture) then
    begin
		MessageBox(0,'Failed to load Streaks texture.','Error',MB_OK);
    Result := false;
    exit;
    end;
	cylList := glGenLists(1);                                                     // Vytvo�� display list v�lce
	qobj := gluNewQuadric;
	gluQuadricDrawStyle(qobj,GLU_FILL);
	gluQuadricNormals(qobj,GLU_SMOOTH);
	glNewList(cylList,GL_COMPILE);
		glEnable(GL_COLOR_MATERIAL);
		glColor3f(0.0,0.0,1.0);
		glEnable(GL_LIGHT0);
		glEnable(GL_LIGHTING);
		glTranslatef(0.0,0.0,-2.0);
		gluCylinder(qobj,0.5,0.5,4.0,15,5);
		glDisable(GL_LIGHTING);
		glDisable(GL_LIGHT0);
		glDisable(GL_COLOR_MATERIAL);
	glEndList; 
	gStartTime := GetTickCount;										                                // �as startu aplikace
  Result:=true;                                                                 // Inicializace prob�hla v po��dku
end;
  
procedure DrawGLInfo;                                                           // V�pis informac�
var
  modelMatrix: array [0..15] of GLfloat;                                        // Matice ModelView
  projMatrix: array [0..15] of GLfloat;                                         // Projek�n� matice
  DiffTime: GLfloat;                                                            // �asov� rozd�l
  text: string;                                                                 // Pomocn� �et�zec pro form�tov�n� textu
begin
	glGetFloatv(GL_PROJECTION_MATRIX,@projMatrix);				                        // Na�te data projek�n� matice
	glGetFloatv(GL_MODELVIEW_MATRIX,@modelMatrix);				                        // Na�te data matice modelview
	// V�pis pozice kamery
	glColor4f(1.0,1.0,1.0,1.0);
	text := Format('m_Position............. = %.02f, %.02f, %.02f',[gCamera.m_Position.x,gCamera.m_Position.y,gCamera.m_Position.z]);
	gFont.glPrintf(10,720,1,text);
	// V�pis sm�ru kamery
	text := Format('m_DirectionVector...... = %.02f, %.02f, %.02f',[gCamera.m_DirectionVector.i,gCamera.m_DirectionVector.j,gCamera.m_DirectionVector.k]);
	gFont.glPrintf(10,700,1,text);
	// V�pis polohy sv�tla
	text := Format('m_LightSourcePos....... = %.02f, %.02f, %.02f',[gCamera.m_LightSourcePos.x,gCamera.m_LightSourcePos.y,gCamera.m_LightSourcePos.z]);
	gFont.glPrintf(10,680,1,text);
	// V�pis pr�se��ku
	text := Format('ptIntersect............ = %.02f, %.02f, %.02f',[gCamera.ptIntersect.x,gCamera.ptIntersect.y,gCamera.ptIntersect.x]);
	gFont.glPrintf(10,660,1,text);
	// V�pis vektoru sv�tlo - kamera
	text := Format('vLightSourceToCamera... = %.02f, %.02f, %.02f',[gCamera.vLightSourceToCamera.i,gCamera.vLightSourceToCamera.j,gCamera.vLightSourceToCamera.k]);
	gFont.glPrintf(10,640,1,text);
	// V�pis vektoru sv�tlo - pr�se��k
	text := Format('vLightSourceToIntersect = %.02f, %.02f, %.02f',[gCamera.vLightSourceToIntersect.i,gCamera.vLightSourceToIntersect.j,gCamera.vLightSourceToIntersect.k]);
	gFont.glPrintf(10,620,1,text);
	// Matice ModelView
	text := 'GL_MODELVIEW_MATRIX';
	gFont.glPrintf(10,580,1,text);
	// 1. ��dek
	text := Format('%.02f, %.02f, %.02f, %.02f',[modelMatrix[0],modelMatrix[1],modelMatrix[2],modelMatrix[3]]);
	gFont.glPrintf(10,560,1,text);
	// 2. ��dek
	text := Format('%.02f, %.02f, %.02f, %.02f',[modelMatrix[4],modelMatrix[5],modelMatrix[6],modelMatrix[7]]);
	gFont.glPrintf(10,540,1,text);
	// 3. ��dek
	text := Format('%.02f, %.02f, %.02f, %.02f',[modelMatrix[8],modelMatrix[9],modelMatrix[10],modelMatrix[11]]);
	gFont.glPrintf(10,520,1,text);
	// 4. ��dek
	text := Format('%.02f, %.02f, %.02f, %.02f',[modelMatrix[12],modelMatrix[13],modelMatrix[14],modelMatrix[15]]);
	gFont.glPrintf(10,500,1,text);
	// Projek�n� matice
	text := 'GL_PROJECTION_MATRIX';
	gFont.glPrintf(10,460,1,text);
	// 1. ��dek
	text := Format('%.02f, %.02f, %.02f, %.02f',[projMatrix[0],projMatrix[1],projMatrix[2],projMatrix[3]]);
	gFont.glPrintf(10,440,1,text);
	// 2. ��dek
	text := Format('%.02f, %.02f, %.02f, %.02f',[projMatrix[4],projMatrix[5],projMatrix[6],projMatrix[7]]);
	gFont.glPrintf(10,420,1,text);
	// 3. ��dek
	text := Format('%.02f, %.02f, %.03f, %.03f',[projMatrix[8],projMatrix[9],projMatrix[10],projMatrix[11]]);
	gFont.glPrintf(10,400,1,text);
	// 4. ��dek
	text := Format('%.02f, %.02f, %.03f, %.03f',[projMatrix[12],projMatrix[13],projMatrix[14],projMatrix[15]]);
	gFont.glPrintf(10,380,1,text);
	// O�ez�vac� roviny
	gFont.glPrintf(10,320,1,'FRUSTUM CLIPPING PLANES');
	// Prav� rovina
	text := Format('%.02f, %.02f, %.02f, %.02f',[gCamera.m_Frustum[0,0],gCamera.m_Frustum[0,1],gCamera.m_Frustum[0,2],gCamera.m_Frustum[0,3]]);
	gFont.glPrintf(10,300,1,text);
	// Lev� rovina
	text := Format('%.02f, %.02f, %.02f, %.02f',[gCamera.m_Frustum[1,0],gCamera.m_Frustum[1,1],gCamera.m_Frustum[1,2],gCamera.m_Frustum[1,3]]);
	gFont.glPrintf(10,280,1,text);
	// Spodn� rovina
	text := Format('%.02f, %.02f, %.02f, %.02f',[gCamera.m_Frustum[2,0],gCamera.m_Frustum[2,1],gCamera.m_Frustum[2,2],gCamera.m_Frustum[2,3]]);
	gFont.glPrintf(10,260,1,text);
	// Horn� rovina
	text := Format('%.02f, %.02f, %.02f, %.02f',[gCamera.m_Frustum[3,0],gCamera.m_Frustum[3,1],gCamera.m_Frustum[3,2],gCamera.m_Frustum[3,3]]);
	gFont.glPrintf(10,240,1,text);
	// Zadn� rovina
	text := Format('%.02f, %.02f, %.02f, %.02f',[gCamera.m_Frustum[4,0],gCamera.m_Frustum[4,1],gCamera.m_Frustum[4,2],gCamera.m_Frustum[4,3]]);
	gFont.glPrintf(10,220,1,text);
	// P�edn� rovina
	text := Format('%.02f, %.02f, %.02f, %.02f',[gCamera.m_Frustum[5,0],gCamera.m_Frustum[5,1],gCamera.m_Frustum[5,2],gCamera.m_Frustum[5,3]]);
	gFont.glPrintf(10,200,1,text);
	if gFrames >= 100 then											                                  // Aktualizace FPS
    begin
		gCurrentTime := GetTickCount;							                                  // Aktu�ln� �as
		DiffTime := gCurrentTime - gStartTime;			                                // �asov� rozd�l
		gFPS := (gFrames / DiffTime) * 1000.0;					                            // V�po�et FPS
		gStartTime := gCurrentTime;								                                  // Ulo�en� aktu�ln�ho �asu
		gFrames := 1;											                                          // ��ta� sn�mk�
	  end
	  else
		Inc(gFrames);												                                        // Zv��� ��ta�
	// V�pis FPS
	text := Format('FPS %.02f',[gFPS]);
	gFont.glPrintf(10,160,1,text);
end;

procedure CheckKeys;
begin
  if keys[Ord('W')] then										// Stisk W?
		gCamera.ChangePitch(-0.2);
	if keys[Ord('S')] then										// Stisk S?
		gCamera.ChangePitch(0.2);
	if keys[Ord('D')] then										// Stisk D?
		gCamera.ChangeHeading(0.2);
	if keys[Ord('A')] then										// Stisk A?
		gCamera.ChangeHeading(-0.2);
	if keys[Ord('Z')] then										// Stisk Z?
		gCamera.m_ForwardVelocity := 0.01;
	if keys[Ord('C')] then										// Stisk C?
		gCamera.m_ForwardVelocity := -0.01;
	if keys[Ord('X')] then										// Stisk X?
		gCamera.m_ForwardVelocity := 0.0;
	if keys[Ord('1')] then										// Stisk 1?
		infoOn := true;
	if keys[Ord('2')] then										// Stisk 2?
		infoOn := false;											
end;

function DrawGLScene():bool;                                                    // Vykreslov�n�
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                          // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity; 	                                                            // Reset matice
  gCamera.m_LightSourcePos.z := gCamera.m_Position.z - 50.0;                    // Sv�tlo proti kame�e
  glPushMatrix;                                                                 // Vykreslen� v�lce
  glLoadIdentity;
	glTranslatef(0.0,0.0,-20.0);
	glRotatef(GetTickCount / 50.0,0.3,0.0,0.0);
	glRotatef(GetTickCount / 50.0,0.0,0.5,0.0);
	glCallList(cylList);
	glPopMatrix;
	gCamera.SetPrespective;										                                    // Nastav� pohled na sc�nu
	gCamera.RenderLensFlare;										                                  // Vykresl� �o�kov� efekty
	gCamera.UpdateFrustumFaster;									                                // Aktualizace o�ez�vac�ch rovin
	if infoOn then											                                          // M�me vykreslovat informace?
		DrawGLInfo;
	CheckKeys;                                                                    // O�et�en� stisknut�ch kl�ves
  Result := true;                                                               // Vykreslen� prob�hlo v po��dku
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
  gCamera := glCamera.Create;
  gFont := glFont.Create;
  // Dotaz na u�ivatele pro fullscreen/okno
  if MessageBox(0,'Would You Like To Run In FullScreen Mode?','Start FullScreen',
                             MB_YESNO or MB_ICONQUESTION)=IDNO then
    FullScreen:=false                                 // B�h v okn�
  else
    FullScreen:=true;                                 // Fullscreen
  if not CreateGLWindow('NeHe''s OpenGL Framework',640,480,32,FullScreen) then // Vytvo�en� OpenGL okna
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
        end;
    end;                                              // Konec smy�ky while
  gluDeleteQuadric(qobj);                             // Sma�e objekt v�lce
  glDeleteLists(cylList,1);                           // Sma�e display list
  gCamera.Free;
  gFont.Free;
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

