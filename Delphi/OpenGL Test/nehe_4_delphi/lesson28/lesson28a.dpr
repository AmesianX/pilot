program lesson28a;

{   k�d pro Delphi 7}

//******************************************************************************
// Pozn�mka:
// Do k�du jsem neimplementoval "fullscreen fix", proto�e na m� grafick� kart�
// GeForce4 Ti4800 to ��dn� probl�m ned�l� :-))) Pokud by s t�m m�l n�kdo probl�m
// nech� si zkus� �pravu k�du jako dom�c� cvi�en�. Nen� to nic te�k�ho ...
//******************************************************************************

uses
  Windows,
  Messages,
  OpenGL,sysutils,math,
  GLaux;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

type
  Point_3D = record                                   // Struktura bodu
    x, y, z: Double;
    end;

  Bezier_patch = record                               // Struktura Bezierova povrchu
    anchors: array [0..3,0..3] of Point_3D;           // M��ka ��d�c�ch bod� (4x4)
    dlBPatch: GLuint;                                 // Display list
    texture: GLuint;                                  // Textura
    end;

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  mybezier: Bezier_patch;           // Bezier�v povrch
  rotz: GLfloat = 0.0;              // Rotace na ose z
  showCPoints: boolean = true;      // Flag pro zobrazen� m��ky mezi kontroln�mi body
  divs: integer = 7;                // Po�et interpolac� (mno�stv� vykreslovan�ch polygon�)

function pointAdd(p, q:  Point_3D):Point_3D;                // S��t�n� dvou bod�
begin
  p.x := p.x + q.x;
  p.y := p.y + q.y;
  p.z := p.z + q.z;
  Result := p;
end;

function pointTimes(c: Double; p: Point_3D):Point_3D;       // N�soben� bodu konstantou
begin
  p.x := p.x * c;
  p.y := p.y * c;
  p.z := p.z * c;
  Result := p;
end;

function makePoint(a, b, c: Double):Point_3D;               // Vytvo�en� bodu ze t�� ��sel
var p: Point_3D;
begin
  p.x := a;
  p.y := b;
  p.z := c;
  Result := p;
end;

function Bernstein(u: GLfloat; p: array of Point_3D):Point_3D;  // Spo��t� sou�adnice bodu le��c�ho na k�ivce
var a, b, c, d, r: Point_3D;                                    // Pomocn� prom�nn�
begin
  a := pointTimes(Power(u,3),p[0]);                             // V�po�et podle vzorce
  b := pointTimes(3*Power(u,2)*(1-u),p[1]);
  c := pointTimes(3*Power((1-u),2)*u,p[2]);
  d := pointTimes(Power((1-u),3),p[3]);
  r := pointAdd(pointAdd(a,b),pointAdd(c,d));                   // Se�ten� n�sobk� a, b, c, d
  Result := r;                                                  // Vr�cen� v�sledn�ho bodu
end;

function genBezier(patch: Bezier_patch; divs: integer): GLuint; // Generuje display list Bezierova povrchu
var
  u, v: integer;                                                // ��d�c� prom�nn�
  px, py, pyold: GLfloat;                                       // Procentu�ln� hodnoty
  drawList: GLuint;                                             // Display list
  temp: array [0..3] of Point_3D;                               // ��d�c� body pomocn� k�ivky
  last: array of Point_3D;                                      // Posledn� �ada polygon�
begin
  drawList := glGenLists(1);                                    // Display list
  SetLength(last,divs+1);                                       // Prvn� �ada polygon�
  if patch.dlBPatch <> 0 then glDeleteLists(patch.dlBPatch,1);  // Pokud existuje star� display list Sma�eme ho
  temp[0] := patch.anchors[0,3];                                // Prvn� odvozen� k�ivka (osa x)
  temp[1] := patch.anchors[1,3];
  temp[2] := patch.anchors[2,3];
  temp[3] := patch.anchors[3,3];
  for v := 0 to divs do                                         // Vytvo�� prvn� ��dek bod�
    begin
    px := v / divs;                                             // Px je procentu�ln� hodnota v
    last[v] := Bernstein(px,temp);                              // Spo��t� bod na k�ivce ve vzd�lenosti px
    end;
  glNewList(drawList,GL_COMPILE);                               // Nov� display list
  glBindTexture(GL_TEXTURE_2D,patch.texture);                   // Zvol� texturu
  for u := 1 to divs do                                         // Proch�z� body na k�ivce
    begin
    py := u / divs;                                             // Py je procentu�ln� hodnota u
    pyold := (u - 1) / divs;                                    // Pyold m� hodnotu py p�i minul�m pr�chodu cyklem
    temp[0] := Bernstein(py, patch.anchors[0]);                 // Spo��t� Bezierovy body pro k�ivku
    temp[1] := Bernstein(py, patch.anchors[1]);
    temp[2] := Bernstein(py, patch.anchors[2]);
    temp[3] := Bernstein(py, patch.anchors[3]);
    glBegin(GL_TRIANGLE_STRIP);                                 // Za��tek kreslen� triangle stripu
      for v := 0 to divs do                                     // Proch�z� body na k�ivce
        begin
        px := v / divs;                                         // Px je procentu�ln� hodnota v
        glTexCoord2f(pyold,px);                                 // Texturovac� koordin�ty z minul�ho pr�chodu
        glVertex3d(last[v].x,last[v].y,last[v].z);              // Bod z minul�ho pr�chodu
        last[v] := Bernstein(px,temp);                          // Generuje nov� bod
        glTexCoord2f(py,px);                                    // Nov� texturov� koordin�ty
        glVertex3d(last[v].x,last[v].y,last[v].z);              // Nov� bod
        end;
    glEnd;                                                      // Konec triangle stripu
    end;
  glEndList;                                                    // Konec display listu
  last := nil;                                                  // Uvoln� dynamick� pole vertex�
  Result := drawList;                                           // Vr�t� pr�v� vytvo�en� display list
end;

procedure initBezier;                                       // Po��te�n� nastaven� kontroln�ch bod�
begin
  mybezier.anchors[0,0] := makePoint(-0.75,-0.75,-0.5);
  mybezier.anchors[0,1] := makePoint(-0.25,-0.75, 0.0);
  mybezier.anchors[0,2] := makePoint( 0.25,-0.75, 0.0);
  mybezier.anchors[0,3] := makePoint( 0.75,-0.75,-0.5);
  mybezier.anchors[1,0] := makePoint(-0.75,-0.25,-0.75);
  mybezier.anchors[1,1] := makePoint(-0.25,-0.25, 0.5);
  mybezier.anchors[1,2] := makePoint( 0.25,-0.25, 0.5);
  mybezier.anchors[1,3] := makePoint( 0.75,-0.25,-0.75);
  mybezier.anchors[2,0] := makePoint(-0.75, 0.25, 0.0);
  mybezier.anchors[2,1] := makePoint(-0.25, 0.25,-0.5);
  mybezier.anchors[2,2] := makePoint( 0.25, 0.25,-0.5);
  mybezier.anchors[2,3] := makePoint( 0.75, 0.25, 0.0);
  mybezier.anchors[3,0] := makePoint(-0.75, 0.75,-0.5);
  mybezier.anchors[3,1] := makePoint(-0.25, 0.75,-1.0);
  mybezier.anchors[3,2] := makePoint( 0.25, 0.75,-1.0);
  mybezier.anchors[3,3] := makePoint( 0.75, 0.75,-0.5);
  mybezier.dlBPatch := 0;                                   // Display list je�t� neexistuje
end;

function LoadBMP(FileName: pchar):PTAUX_RGBImageRec;        // Nahraje bitmapu
begin
  if Filename = '' then                                     // Byla p�ed�na cesta k souboru?
    begin
    Result := nil;                                          // Pokud ne, konec
    exit;
    end;
  if not FileExists(Filename) then                          // Existuje soubor?
    begin
    Result := nil;                                          // Pokud ne, konec
    exit;
    end;
  Result := auxDIBImageLoadA(FileName);                     // Na�te bitmapu a vr�t� na ni ukazatel
end;

function LoadGLTextures: Bool;                              // Loading bitmapy a konverze na texturu
var TextureImage: array [0..0] of PTAUX_RGBImageRec;        // Ukl�d� bitmapu
    Status: Bool;                                           // Indikuje chyby
begin
  Status := false;
  ZeroMemory(@TextureImage,sizeof(TextureImage));           // Vynuluje pam�
  TextureImage[0] := LoadBMP('Data/NeHe.bmp');              // Nahraje bitmapu
  if Assigned(TextureImage[0]) then                         // V�e je bez probl�m�?
    begin
    Status := true;                                         // V�e je bez probl�m�
    glGenTextures(1,mybezier.texture);                      // Generuje texturu
    glBindTexture(GL_TEXTURE_2D,mybezier.texture);          // Typick� vytv��en� textury z bitmapy
    glTexImage2D(GL_TEXTURE_2D,0,3,TextureImage[0].sizeX,TextureImage[0].sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[0].data);    // Vlastn� vytv��en� textury
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);   // Filtrov�n� p�i zv�t�en�
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);   // Filtrov�n� p�i zmen�en�
    end;
  Result := Status;                                         // Ozn�m� p��padn� chyby
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
begin
  if not LoadGLTextures then                        // Nahraje texturu
    begin
    Result := false;
    exit;
    end;
  glEnable(GL_TEXTURE_2D);                          // Zapne mapov�n� textur
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glClearColor(0.0, 0.0, 0.0, 0.5);	  	            // �ern� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glEnable(GL_DEPTH_TEST);			                    // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LEQUAL);				                    // Typ hloubkov�ho testov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST); // Nejlep�� perspektivn� korekce
  initBezier;                                       // Inicializace kontroln�ch bod�
  mybezier.dlBPatch := genBezier(mybezier,divs);    // Generuje display list Bezierova povrchu
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;

function DrawGLScene():bool;                            // Vykreslov�n�
var
  i, j: integer;                                        // ��d�c� prom�nn� cykl�
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);  // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity();	                                    // Reset matice
  glTranslatef(0.0,0.0,-4.0);                           // P�esun do hloubky
  glRotatef(-75.0,1.0,0.0,0.0);                         // Rotace na ose x
  glRotatef(rotz,0.0,0.0,1.0);                          // Rotace na ose z
  glCallList(mybezier.dlBPatch);                        // Vykresl� display list Bezierova povrchu
  if showCPoints then                                   // Pokud je zapnut� vykreslov�n� m��ky
    begin
    glDisable(GL_TEXTURE_2D);                           // Vypne texturov�n�
    glColor3f(1.0,0.0,0.0);                             // �erven� barva
    for i := 0 to 3 do                                  // Horizont�ln� linky
      begin
      glBegin(GL_LINE_STRIP);                           // Kreslen� linek
      for j := 0 to 3 do                                // �ty�i linky
        glVertex3d(mybezier.anchors[i,j].x,mybezier.anchors[i,j].y,mybezier.anchors[i,j].z);
      glEnd;                                            // Konec kreslen�
      end;
    for i := 0 to 3 do                                  // Vertik�ln� linky
      begin
      glBegin(GL_LINE_STRIP);                           // Kreslen� linek
      for j := 0 to 3 do                                // �ty�i linky
        glVertex3d(mybezier.anchors[j,i].x,mybezier.anchors[j,i].y,mybezier.anchors[j,i].z);
      glEnd;                                            // Konec kreslen�
      end;
    glColor3f(1.0,1.0,1.0);                             // B�l� barva
    glEnable(GL_TEXTURE_2D);                            // Zapne texturov�n�
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
          if keys[VK_LEFT] then rotz := rotz - 0.8;             // Rotace doleva
          if keys[VK_RIGHT] then rotz := rotz + 0.8;            // Rotace doprava
          if keys[VK_UP] then                                   // �ipka nahoru
            begin
            divs := divs + 1;                                   // Men�� hranatost povrchu
            mybezier.dlBPatch := genBezier(mybezier,divs);      // Aktualizace display listu
            keys[VK_UP] := false;
            end;
          if keys[VK_DOWN] and (divs > 1) then                  // �ipka dol�
            begin
            divs := divs - 1;                                   // V�t�� hranatost povrchu
            mybezier.dlBPatch := genBezier(mybezier,divs);      // Aktualizace display listu
            keys[VK_DOWN] := false;
            end;
          if keys[VK_SPACE] then                                // Mezern�k
            begin
            showCPoints := not showCPoints;                     // Zobraz�/skryje linky mezi ��d�c�mi body
            keys[VK_SPACE] := false;
            end;
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

