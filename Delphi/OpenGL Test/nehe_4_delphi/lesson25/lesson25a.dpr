program lesson25a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages, sysutils,
  OpenGL;

type
  Vertex = record                   // Struktura pro bod ve 3D
    x, y, z: GLfloat;               // X, y, z slo�ky pozice
    end;

  Objekt = record                   // Struktura objektu
    verts: integer;                 // Po�et bod�, ze kter�ch se skl�d�
    points: array of Vertex;        // Ukazatel do pole vertex�
    end;

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  xrot, yrot, zrot: GLfloat;        // Rotace
  xspeed, yspeed, zspeed: GLfloat;  // Rychlost rotace
  cx, cy: GLfloat;                  // Pozice
  cz: GLfloat = -15;                // Pozice
  key: integer = 1;                 // Pr�v� zobrazen� objekt
  morph: boolean = false;           // Prob�h� pr�v� morfov�n�?
  steps: integer = 200;             // Po�et krok� zm�ny
  step: integer = 0;                // Aktu�ln� krok
  maxver: integer;                  // Eventu�ln� ukl�d� maxim�ln� po�et bod� v jednom objektu
  morph1, morph2, morph3, morph4: Objekt; // Koule, toroid, v�lec (trubka), n�hodn� body
  helper, sour, dest: Objekt;       // Pomocn�, zdrojov� a c�lov� objekt

function ObjAllocate(var k: Objekt; n: integer): boolean;   // Alokuje dynamickou pam� pro objekt
begin
  Result := true;
  try
  SetLength(k.points,n);                                    // Alokuje pam�
  except
    on EOutOfMemory do Result := false;                     // P�i chyb� vrac� false
  end;
end;

procedure ObjFree(var k: Objekt);                           // Uvoln� dynamickou pam� objektu
begin
  SetLength(k.points,0);                                    // Uvoln� pam�
end;

procedure ReadStr(var f: textfile; var s: string);          // Na�te jeden pou�iteln� ��dek ze souboru
begin
  readln(f,s);                                              // Na�ti ��dek
  while ((copy(s, 1, 1) = '/') or (length(s) = 0)) do       // Pokud nen� pou�iteln� na�ti dal��
    readln(f,s);
end;

function ObjLoad(name: string; var k: Objekt): boolean;     // Nahraje objekt ze souboru
var
  ver: integer;                                             // Po�et bod�
  rx, ry, rz: GLfloat;                                      // X, y, z pozice
  filein: TextFile;                                         // Soubor
  oneline: string;                                          // Znakov� buffer
  i: integer;                                               // Cyklus
begin
  AssignFile(filein,name);
  {$I-}
  Reset(filein);                                            // Otev�e soubor
  {$I+}
  if IOResult <> 0 then                                     // Poda�ilo se soubor otev��t?
    begin
    Result := false;                                        // Konec
    exit;
    end;
  ReadStr(filein,oneline);                                  // Na�te prvn� ��dku ze souboru
  Delete(oneline,1,10);                                     // Odstran� prvn�ch 10 znak� "Vertices: "
  ver := StrToIntDef(oneline,0);                            // Po�et vertex�
  k.verts := ver;                                           // Nastav� polo�ku struktury na spr�vnou hodnotu
  if not ObjAllocate(k,ver) then                            // Alokace pam�ti pro objekt
    begin
    CloseFile(filein);                                      // Zav�e soubor
    Result := false;                                        // Konec
    exit;
    end;
  for i := 0 to ver - 1 do                                  // Postupn� na��t� body
    begin
      ReadStr(filein,oneline);                              // Na�te ��dek ze souboru
      rx := StrToFloat(Copy(oneline,1,6));                  // Najde a ulo�� t�i ��sla
      ry := StrToFloat(Copy(oneline,10,8));
      rz := StrToFloat(Copy(oneline,19,Length(oneline)));
      k.points[i].x := rx;                                  // Nastav� vnit�n� prom�nnou struktury
      k.points[i].y := ry;                                  // Nastav� vnit�n� prom�nnou struktury
      k.points[i].z := rz;                                  // Nastav� vnit�n� prom�nnou struktury
    end;
  CloseFile(filein);                                        // Zav�e soubor
  if ver > maxver then maxver := ver;                       // Aktualizuje maxim�ln� po�et vertex�
  Result := true;
end;

function Calculate(i: integer): Vertex;                     // Spo��t� o kolik pohnout bodem p�i morfingu
var
  a: Vertex;                                                // Pomocn� bod
begin
  a.x := (sour.points[i].x - dest.points[i].x) / steps;     // Spo��t� posun
  a.y := (sour.points[i].y - dest.points[i].y) / steps;     // Spo��t� posun
  a.z := (sour.points[i].z - dest.points[i].z) / steps;     // Spo��t� posun
  Result := a;                                              // Vr�t� v�sledek
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
var
  i: integer;                                       // Cyklus
begin
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);                // Typ blendingu
  glClearColor(0.0, 0.0, 0.0, 0.0);	  	            // �ern� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glDepthFunc(GL_LESS); 				                    // Typ hloubkov�ho testov�n�
  glEnable(GL_DEPTH_TEST);			                    // Povol� hloubkov� testov�n�
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST); // Nejlep�� perspektivn� korekce
  maxver := 0;                                      // Nulov�n� maxim�ln�ho po�tu bod�
  if not ObjLoad('Data/Sphere.txt', morph1) then    //Na�te kouli?
    begin
    Result := false;                                // Konec
    exit;
    end;
  if not ObjLoad('Data/Torus.txt', morph2) then     // Na�te toroid?
    begin
    Result := false;                                // Konec
    exit;
    end;
  if not ObjLoad('Data/Tube.txt', morph3) then      // Na�te v�lec?
    begin
    Result := false;                                // Konec
    exit;
    end;
  if not ObjAllocate(morph4,486) then               // Alokace pam�ti pro 486 bod�?
    begin
    Result := false;                                // Konec
    exit;
    end;
  for i := 0 to 485 do                              // Cyklus generuje n�hodn� sou�adnice
    begin
    morph4.points[i].x := (Random(14000) / 1000) - 7;   // N�hodn� hodnota
    morph4.points[i].y := (Random(14000) / 1000) - 7;   // N�hodn� hodnota
    morph4.points[i].z := (Random(14000) / 1000) - 7;   // N�hodn� hodnota
    end;
  if not ObjLoad('Data/Sphere.txt',helper) then     // Na�ten� koule do pomocn�ho objektu
    begin
    Result := false;                                // Konec
    exit;
    end;
  sour := morph1;                                   // Inicializace ukazatel� na objekty
  dest := morph1;                                   // Inicializace ukazatel� na objekty
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;

function DrawGLScene():bool;                            // Vykreslov�n�
var
  tx, ty, tz: GLfloat;                                  // Pomocn� sou�adnice
  q: Vertex;                                            // Pomocn� bod pro v�po�ty
  i: integer;                                           // Cyklus
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);  // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity();	                                    // Reset matice
  glTranslatef(cx,cy,cz);                               // P�esun na pozici
  glRotatef(xrot,1.0,0.0,0.0);                          // Rotace na ose x
  glRotatef(yrot,0.0,1.0,0.0);                          // Rotace na ose y
  glRotatef(zrot,0.0,0.0,1.0);                          // Rotace na ose z
  xrot := xrot + xspeed;                                // Zv�t�� �hly rotace
  yrot := yrot + yspeed;
  zrot := zrot + zspeed;
  glBegin(GL_POINTS);                                   // Za��tek kreslen� bod�
    for i := 0 to morph1.verts - 1 do                   // Cyklus proch�z� vertexy
      begin
      if morph then q := Calculate(i)                   // Pokud zrovna morfujeme, Spo��t�me hodnotu posunut�
        else                                            // Jinak
        begin
        q.x := 0;                                       // Budeme ode��tat nulu, ale t�m neposouv�me
        q.y := 0;
        q.z := 0;
        end;
      helper.points[i].x := helper.points[i].x - q.x;   // Posunut� na ose x
      helper.points[i].y := helper.points[i].y - q.y;   // Posunut� na ose y
      helper.points[i].z := helper.points[i].z - q.z;   // Posunut� na ose z
      tx := helper.points[i].x;                         // Zp�ehledn�n� + efekt
      ty := helper.points[i].y;                         // Zp�ehledn�n� + efekt
      tz := helper.points[i].z;                         // Zp�ehledn�n� + efekt
      glColor3f(0.0,1.0,1.0);                           // Zelenomodr� barva
      glVertex3f(tx,ty,tz);                             // Vykresl� prvn� bod
      glColor3f(0.0,0.5,1.0);                           // Mod�ej�� zelenomodr� barva
      tx := tx - 2 * q.x;                               // Spo��t�n� nov�ch pozic
      ty := ty - 2 * q.y;
      tz := tz - 2 * q.z;
      glVertex3f(tx,ty,tz);                             // Vykresl� druh� bod v nov� pozici
      glColor3f(0.0,0.0,1.0);                           // Modr� barva
      tx := tx - 2 * q.x;                               // Spo��t�n� nov�ch pozic
      ty := ty - 2 * q.y;
      tz := tz - 2 * q.z;
      glVertex3f(tx,ty,tz);                             // Vykresl� t�et� bod v nov� pozici
      end;
  glEnd;                                                // Ukon�� kreslen�
  if morph and (step <= steps) then                     // Morfujeme a krok je men�� ne� maximum
    step := step + 1                                    // P��t� pokra�uj n�sleduj�c�m krokem
    else                                                // Nemorfujeme nebo byl pr�v� ukon�en
    begin
    morph := false;                                     // Konec morfingu
    sour := dest;                                       // C�lov� objekt je nyn� zdrojov�
    step := 0;                                          // Prvn� (nulov�) krok morfingu
    end;
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
  ObjFree(morph1);                                      // Uvoln� alokovanou pam�
  ObjFree(morph2);                                      // Uvoln� alokovanou pam�
  ObjFree(morph3);                                      // Uvoln� alokovanou pam�
  ObjFree(morph4);                                      // Uvoln� alokovanou pam�
  ObjFree(helper);                                      // Uvoln� alokovanou pam�
  ObjFree(sour);                                        // Uvoln� alokovanou pam�
  ObjFree(dest);                                        // Uvoln� alokovanou pam�
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
          if keys[VK_PRIOR] then                      // PageUp?
            zspeed := zspeed + 0.01;                  
          if keys[VK_NEXT] then                       // PageDown?
            zspeed := zspeed - 0.01;                  
          if keys[VK_DOWN] then                       // �ipka dolu?
            xspeed := xspeed + 0.01;                  
          if keys[VK_UP] then                         // �ipka nahoru?
            xspeed := xspeed - 0.01;                  
          if keys[VK_RIGHT] then                      // �ipka doprava?
            yspeed := yspeed + 0.01;                  
          if keys[VK_LEFT] then                       // �ipka doleva?
            yspeed := yspeed - 0.01;                  
          if keys[Ord('Q')] then                      // Q?
            cz := cz + 0.01;                          // D�le
          if keys[Ord('Z')] then                      // Z?
            cz := cz - 0.01;                          // Bl�e
          if keys[Ord('W')] then                      // W?
            cy := cy + 0.01;                          // Nahoru
          if keys[Ord('S')] then                      // S?
            cy := cy - 0.01;                          // Dolu
          if keys[Ord('D')] then                      // D?
            cx := cx + 0.01;                          // Doprava
          if keys[Ord('A')] then                      // A?
            cx := cx - 0.01;                          // Doleva
          if (keys[Ord('1')]) and (key <> 1) and not morph then   // Kl�vesa 1?
            begin
            key := 1;                                             // Proti dvojn�sobn�mu stisku
            morph := true;                                        // Za�ne morfovac� proces
            dest := morph1;                                       // Nastav� c�lov� objekt
            end;
          if (keys[Ord('2')]) and (key <> 2) and not morph then   // Kl�vesa 2?
            begin
            key := 2;                                             // Proti dvojn�sobn�mu stisku
            morph := true;                                        // Za�ne morfovac� proces
            dest := morph2;                                       // Nastav� c�lov� objekt
            end;
          if (keys[Ord('3')]) and (key <> 3) and not morph then   // Kl�vesa 3?
            begin
            key := 3;                                             // Proti dvojn�sobn�mu stisku
            morph := true;                                        // Za�ne morfovac� proces
            dest := morph3;                                       // Nastav� c�lov� objekt
            end;
          if (keys[Ord('4')]) and (key <> 4) and not morph then   // Kl�vesa 4?
            begin
            key := 4;                                             // Proti dvojn�sobn�mu stisku
            morph := true;                                        // Za�ne morfovac� proces
            dest := morph4;                                       // Nastav� c�lov� objekt
            end;
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  DecimalSeparator := '.';                             // Odd�lova� desetinn�ch m�st podle anglick�ch zvyklost�
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
  DecimalSeparator := ',';                             // Odd�lova� desetinn�ch m�st zp�t podle �esk�ch zvyklost�
end.

