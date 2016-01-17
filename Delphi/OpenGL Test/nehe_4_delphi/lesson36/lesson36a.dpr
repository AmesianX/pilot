program lesson36a;

{   k�d pro Delphi 7}

uses
  Windows,
  SysUtils,
  Messages,
  OpenGL,
  NeHeGL in 'NeHeGL.pas';

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); stdcall; external 'opengl32';
procedure glCopyTexImage2D(target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); stdcall; external 'opengl32';

type
  TPoints = array [0..3,0..2] of GLfloat;               // Pole bod�

const                                                   // Sv�tla
  global_ambient: array [0..3] of GLfloat = (0.2,0.2,0.2,1.0);
  light0pos: array [0..3] of GLfloat = (0.0,5.0,10.0,1.0);
	light0ambient: array [0..3] of GLfloat = (0.2,0.2,0.2,1.0);
	light0diffuse: array [0..3] of GLfloat = (0.3,0.3,0.3,1.0);
	light0specular: array [0..3] of GLfloat = (0.8,0.8,0.8,1.0);
	lmodel_ambient: array [0..3] of GLfloat = (0.2,0.2,0.2,1.0);
  glfMaterialColor: array [0..3] of GLfloat = (0.4,0.2,0.8,1.0);
  specular: array [0..3] of GLfloat = (1.0,1.0,1.0,1.0);

var
  g_window: PGL_Window;                                 // Okno
  g_keys: PKeys;                                        // Kl�vesy
  angle: GLfloat;                                       // �hel rotace spir�ly
  vertexes: array [0..3,0..2] of GLfloat;               // �ty�i body o t�ech sou�adnic�ch
  normal: array [0..2] of GLfloat;                      // Data norm�lov�ho vektoru
  BlurTexture: GLuint;                                  // Textura


function EmptyTexture: GLuint;                                                  // Vytvo�� pr�zdnou texturu
var
  txtnumber: GLuint;                                                            // ID textury
  data: PGLuint;                                                                // Ukazatel na data obr�zku
begin
  data := AllocMem(128*128*4*sizeof(GLuint));                                   // Alokace pam�ti
  glGenTextures(1,txtnumber);                                                   // Jedna textura
  glBindTexture(GL_TEXTURE_2D,txtnumber);                                       // Zvol� texturu
  glTexImage2D(GL_TEXTURE_2D,0,4,128,128,0,GL_RGBA,GL_UNSIGNED_BYTE,data);      // Vytvo�en� textury
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);               // Line�rn� filtrov�n� pro zmen�en� i zv�t�en�
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  FreeMem(data);                                                                // Uvoln�n� pam�ti
  Result := txtnumber;                                                          // Vr�t� ID textury
end;

procedure ReduceToUnit(var vector: array of GLfloat);                           // V�po�et normalizovan�ho vektoru (jednotkov� d�lka)
var
  length: GLfloat;                                                              // D�lka vektoru
begin
  length := sqrt(sqr(vector[0]) + sqr(vector[1]) + sqr(vector[2]));             // V�po�et sou�asn� d�lky vektoru
  if length = 0 then length := 1;                                               // Prevence d�len� nulou
  vector[0] := vector[0] / length;                                              // Vyd�len� jednotliv�ch slo�ek d�lkou
  vector[1] := vector[1] / length;
  vector[2] := vector[2] / length;
end;

procedure calcNormal(v: TPoints; var output: array of GLfloat);                 // V�po�et norm�lov�ho vektoru polygonu
const
  x = 0;                                                                        // Pomocn� indexy do pole
  y = 1;
  z = 2;
var
  v1, v2: array [0..2] of GLfloat;                                              // Vektor 1 a vektor 2 (x,y,z)
begin
  v1[x] := v[0,x] - v[1,x];                                                     // V�po�et vektoru z 1. bodu do 0. bodu
  v1[y] := v[0,y] - v[1,y];
  v1[z] := v[0,z] - v[1,z];
  v2[x] := v[1,x] - v[2,x];                                                     // V�po�et vektoru z 2. bodu do 1. bodu
  v2[y] := v[1,y] - v[2,y];
  v2[z] := v[1,z] - v[2,z];
  // V�sledkem vektorov�ho sou�inu dvou vektor� je t�et� vektor, kter� je k nim kolm�
  output[x] := v1[y]*v2[z] - v1[z]*v2[y];
  output[y] := v1[z]*v2[x] - v1[x]*v2[z];
  output[z] := v1[x]*v2[y] - v1[y]*v2[x];
  ReduceToUnit(output);                                                         // Normalizace v�sledn�ho vektoru
end;

procedure ProcessHelix;                                                         // Vykresl� spir�lu
var
  x, y, z: GLfloat;                                                             // Sou�adnice x, y, z
  phi, theta, u, v: GLfloat;                                                    // �hly
  r: GLfloat;                                                                   // Polom�r z�vitu
  twists: integer;                                                              // Z�vity
begin
  twists := 5;                                                                  // P�t z�vit�
  glLoadIdentity;                                                               // Reset matice
  gluLookAt(0,5,50,0,0,0,0,1,0);                                                // Pozice o�� (0,5,50), st�ed sc�ny (0,0,0), UP vektor na ose y
  glPushMatrix;                                                                 // Ulo�en� matice
  glTranslatef(0,0,-50);                                                        // Pades�t jednotek do sc�ny
  glRotatef(angle/2.0,1,0,0);                                                   // Rotace na ose x
  glRotatef(angle/3.0,0,1,0);                                                   // Rotace na ose y
  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE,@glfMaterialColor);     // Nastaven� materi�l�
  glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,@specular);
  r := 1.5;                                                                     // Polom�r
  phi := 0;
  theta := 0;
  glBegin(GL_QUADS);                                                            // Kreslen� obd�ln�k�
  while phi <= 360 do                                                           // 360 stup�� v kroku po 20 stupn�ch
    begin
    while theta <= 360*twists do                                                // 360 stup��* po�et z�vit� po 20 stupn�ch
      begin
      v := phi / 180.0 * 3.142;                                                 // �hel prvn�ho bodu (0)
      u := theta / 180.0 * 3.142;                                               // �hel prvn�ho bodu (0)
      x := (cos(u) * (2.0 + cos(v))) * r;                                       // Pozice x, y, z prvn�ho bodu
      y := (sin(u) * (2.0 + cos(v))) * r;
      z := ((u - (2.0 * 3.142)) + sin(v)) * r;
      vertexes[0,0] := x;                                                       // Kop�rov�n� prvn�ho bodu do pole
      vertexes[0,1] := y;
      vertexes[0,2] := z;
      v := phi / 180.0 * 3.142;                                                 // �hel druh�ho bodu (0)
      u := (theta + 20) / 180.0 * 3.142;                                        // �hel druh�ho bodu (20)
      x := (cos(u) * (2.0 + cos(v))) * r;                                       // Pozice x, y, z druh�ho bodu
      y := (sin(u) * (2.0 + cos(v))) * r;
      z := ((u - (2.0 * 3.142)) + sin(v)) * r;
      vertexes[1,0] := x;                                                       // Kop�rov�n� druh�ho bodu do pole
      vertexes[1,1] := y;
      vertexes[1,2] := z;
      v := (phi + 20) / 180.0 * 3.142;                                          // �hel t�et�ho bodu (20)
      u := (theta + 20) / 180.0 * 3.142;                                        // �hel t�et�ho bodu (20)
      x := (cos(u) * (2.0 + cos(v))) * r;                                       // Pozice x, y, z t�et�ho bodu
      y := (sin(u) * (2.0 + cos(v))) * r;
      z := ((u - (2.0 * 3.142)) + sin(v)) * r;
      vertexes[2,0] := x;                                                       // Kop�rov�n� t�et�ho bodu do pole
      vertexes[2,1] := y;
      vertexes[2,2] := z;
      v := (phi + 20) / 180.0 * 3.142;                                          // �hel �tvrt�ho bodu (20)
      u := theta / 180.0 * 3.142;                                               // �hel �tvrt�ho bodu (0)
      x := (cos(u) * (2.0 + cos(v))) * r;                                       // Pozice x, y, z �tvrt�ho bodu
      y := (sin(u) * (2.0 + cos(v))) * r;
      z := ((u - (2.0 * 3.142)) + sin(v)) * r;
      vertexes[3,0] := x;                                                       // Kop�rov�n� �tvrt�ho bodu do pole
      vertexes[3,1] := y;
      vertexes[3,2] := z;
      calcNormal(TPoints(vertexes),normal);                                     // V�po�et norm�ly obd�ln�ku
      glNormal3f(normal[0],normal[1],normal[2]);                                // Posl�n� norm�ly OpenGL
      // Rendering obd�ln�ku
      glVertex3f(vertexes[0,0],vertexes[0,1],vertexes[0,2]);
      glVertex3f(vertexes[1,0],vertexes[1,1],vertexes[1,2]);
      glVertex3f(vertexes[2,0],vertexes[2,1],vertexes[2,2]);
      glVertex3f(vertexes[3,0],vertexes[3,1],vertexes[3,2]);
      theta := theta + 20.0;
      end;
    theta := 0;
    phi := phi + 20.0;
    end;
  glEnd;                                                                        // Konec kreslen�
  glPopMatrix;                                                                  // Obnoven� matice
end;

procedure ViewOrtho;                          // Nastavuje pravo�hlou projekci
begin
  glMatrixMode(GL_PROJECTION);                // Projek�n� matice
  glPushMatrix;                               // Ulo�en� matice
  glLoadIdentity;                             // Reset matice
  glOrtho(0,640,480,0,-1,1);                  // Nastaven� pravo�hl� projekce
  glMatrixMode(GL_MODELVIEW);                 // Modelview matice
  glPushMatrix;                               // Ulo�en� matice
  glLoadIdentity;                             // Reset matice
end;

procedure ViewPerspective;                    // Obnoven� perspektivn�ho m�du
begin
  glMatrixMode(GL_PROJECTION);                // Projek�n� matice
  glPopMatrix;                                // Obnoven� matice
  glMatrixMode(GL_MODELVIEW);                 // Modelview matice
  glPopMatrix;                                // Obnoven� matice
end;

procedure RenderToTexture;                                                      // Rendering do textury
begin
  glViewport(0,0,128,128);                                                      // Nastaven� viewportu (odpov�d� velikosti textury)
  ProcessHelix;                                                                 // Rendering spir�ly
  glBindTexture(GL_TEXTURE_2D,BlurTexture);                                     // Zvol� texturu
  glCopyTexImage2D(GL_TEXTURE_2D,0,GL_LUMINANCE,0,0,128,128,0);                 // Zkop�ruje viewport do textury (od 0, 0 do 128, 128, bez okraje)
  glClearColor(0.0,0.0,0.5,0.5);                                                // St�edn� modr� barva pozad�
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                          // Sma�e obrazovku a hloubkov� buffer
  glViewport(0,0,640,480);                                                      // Obnoven� viewportu
end;

procedure DrawBlur(times: integer; inc: GLfloat);                     // Vykresl� rozmazan� obr�zek
var
  spost: GLfloat;                                                     // Offset
  alphainc: GLfloat;                                                  // Rychlost blednut� pro alfa blending
  alpha: GLfloat;                                                     // Alpha
  num: integer;                                                       // Cyklus
begin
  spost := 0.0;                                                       // Po��te�n� offset sou�adnic na textu�e
  alpha := 0.2;                                                       // Po��te�n� hodnota alfy
  glDisable(GL_TEXTURE_GEN_S);                                        // Vypne automatick� generov�n� texturov�ch koordin�t�
  glDisable(GL_TEXTURE_GEN_T);
  glEnable(GL_TEXTURE_2D);                                            // Zapne mapov�n� textur
  glDisable(GL_DEPTH_TEST);                                           // Vypne testov�n� hloubky
  glBlendFunc(GL_SRC_ALPHA,GL_ONE);                                   // M�d blendingu
  glEnable(GL_BLEND);                                                 // Zapne blending
  glBindTexture(GL_TEXTURE_2D,BlurTexture);                           // Zvol� texturu
  ViewOrtho;                                                          // P�epne do pravo�hl� projekce
  alphainc := alpha / times;                                          // Hodnota zm�ny alfy p�i jednom kroku
  glBegin(GL_QUADS);                                                  // Kreslen� obd�ln�k�
  for num := 0 to times - 1 do                                        // Po�et krok� renderov�n� skvrn
    begin
    glColor4f(1.0,1.0,1.0,alpha);                                     // Nastaven� hodnoty alfy
    glTexCoord2f(0 + spost,1 - spost);                                // Texturov� koordin�ty (0, 1)
    glVertex2f(0,0);                                                  // Prvn� vertex (0, 0)
    glTexCoord2f(0 + spost,0 + spost);                                // Texturov� koordin�ty (0, 0)
    glVertex2f(0,480);                                                // Druh� vertex (0, 480)
    glTexCoord2f(1 - spost,0 + spost);                                // Texturov� koordin�ty (1, 0)
    glVertex2f(640,480);                                              // T�et� vertex (640, 480)
    glTexCoord2f(1 - spost,1 - spost);                                // Texturov� koordin�ty (1, 1)
    glVertex2f(640,0);                                                // �tvrt� vertex (640, 0)
    spost := spost + inc;                                             // Postupn� zvy�ov�n� skvrn (zoomov�n� do st�edu textury)
    alpha := alpha - alphainc;                                        // Postupn� sni�ov�n� alfy (blednut� obr�zku)
    end;
  glEnd;                                                              // Konec kreslen�
  ViewPerspective;                                                    // Obnoven� perspektivy
  glEnable(GL_DEPTH_TEST);                                            // Zapne testov�n� hloubky
  glDisable(GL_TEXTURE_2D);                                           // Vypne mapov�n� textur
  glDisable(GL_BLEND);                                                // Vypne blending
  glBindTexture(GL_TEXTURE_2D,0);                                     // Zru�en� vybran� textury
end;

function Initialize(window: PGL_Window; key: PKeys): boolean;	        // Inicializace OpenGL
begin
  g_window := window;
  g_keys := key;
  angle := 0.0;
  BlurTexture := EmptyTexture;                                        // Pr�zdn� textura
  glViewport(0,0,window.init.width,window.init.height);               // Okno
  glMatrixMode(GL_PROJECTION);                                        // Projek�n� matice
  glLoadIdentity;                                                     // Reset
  gluPerspective(50,window.init.width / window.init.height,5,2000);   // Nastaven� perspektivy
  glMatrixMode(GL_MODELVIEW);                                         // Modelov� matice
	glLoadIdentity;                                                     // Reset
  glEnable(GL_DEPTH_TEST);			                                      // Povol� hloubkov� testov�n�
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT,@lmodel_ambient);		          // Nastaven� sv�tel
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT,@global_ambient);
	glLightfv(GL_LIGHT0,GL_POSITION,@light0pos);
	glLightfv(GL_LIGHT0,GL_AMBIENT,@light0ambient);
	glLightfv(GL_LIGHT0,GL_DIFFUSE,@light0diffuse);
	glLightfv(GL_LIGHT0,GL_SPECULAR,@light0specular);
	glEnable(GL_LIGHTING);										                          // Zapne sv�tla
	glEnable(GL_LIGHT0);
  glShadeModel(GL_SMOOTH);
  glMateriali(GL_FRONT,GL_SHININESS,128);
  glClearColor(0.0, 0.0, 0.0, 0.5);	  	                              // �ern� pozad�
  Result:=true;                                                       // Inicializace prob�hla v po��dku
end;

procedure Deinitialize;                                               // Uvoln�n� prost�edk�
begin
  glDeleteTextures(1,@BlurTexture);                                   // Smaz�n� textur
end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohyb� ve sc�n� a stisk kl�ves
begin
  if g_keys.keyDown[VK_ESCAPE] then                                   // Kl�vesa ESC?
    TerminateApplication(g_window^);                                  // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                       // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                      // P�epnut� fullscreen/okno
  angle := angle + milliseconds / 5.0;
end;

procedure Draw;                                                       // Vykreslen� sc�ny
begin
  glClearColor(0.0,0.0,0.0,0.5);                                      // �ern� pozad�
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity;	                                                    // Reset matice
  RenderToTexture;                                                    // Rendering do textury
  ProcessHelix;                                                       // Rendering spir�ly
  DrawBlur(25,0.02);                                                  // Rendering blur efektu
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
    init.title := 'rIO And NeHe''s RadialBlur Tutorial.';
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
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );                  // Start programu
end.

