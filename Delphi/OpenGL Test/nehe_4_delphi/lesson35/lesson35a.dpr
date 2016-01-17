program lesson35a;

{   k�d pro Delphi 7}

uses
  Windows,
  SysUtils,
  Messages,
  OpenGL,
  mplayer,
  vfw,
  NeHeGL in 'NeHeGL.pas';

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); stdcall; external 'opengl32';
procedure glCopyTexImage2D(target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); stdcall; external 'opengl32';
procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); stdcall; external 'opengl32';

var
  g_window: PGL_Window;                                 // Okno
  g_keys: PKeys;                                        // Kl�vesy
  angle: GLfloat;                                       // �hel rotace objektu
  next: integer;                                        // Pro animaci
  frame: integer = 0;                                   // Aktu�ln� sn�mek videa
  effect: integer;                                      // Zobrazen� objekt
  env: boolean = true;                                  // Automaticky generovat texturov� koordin�ty?
  bg: boolean = true;                                   // Zobrazovat pozad�?
  sp: boolean;                                          // Stisknut mezern�k?
  ep: boolean;                                          // Stisknuto E?
  bp: boolean;                                          // Stisknuto B?
  psi: TAVIStreamInfo;                                  // Informace o datov�m proudu videa
  pavi: IAVIStream;                                     // Handle proudu
  pgf: IGetFrame;                                       // Ukazatel na objekt GetFrame
  bmih: BITMAPINFOHEADER;                               // Hlavi�ka pro DrawDibDraw dek�dov�n�
  lastframe: integer;                                   // Posledn� sn�mek proudu
  width: integer;                                       // ���ka videa
  height: integer;                                      // V��ka videa
  pdata: PGLubyte;                                      // Ukazatel na data textury
  mpf: integer;                                         // Doba zobrazen� jednoho sn�mku (Milliseconds Per Frame)
  quadratic: GLUquadricObj;                             // Objekt quadraticu
  hdd: HDRAWDIB;                                        // Handle DIBu
  h_bitmap: HBITMAP;                                    // Handle bitmapy z�visl� na za��zen�
  h_dc: HDC;                                            // Kontext za��zen�
  data: Pointer = nil;                                  // Ukazatel na bitmapu o zm�n�n� velikosti

procedure flipIt(buffer: Pointer);                                              // Prohod� �ervenou a modrou slo�ku pixel� v obr�zku
{asm                                                                            // Asm k�d mi nechod� korektn�, okno se p�i animaci neust�le zav�r� a znovu otv�r�...
  mov ecx, 256*256                                                              // Zjistil jsem, �e mi n�jak�m zp�sobem zm�n� hodnotu lok�ln� prom�nn� isMessagePumpActive v procedu�e WinMain
  mov ebx, buffer                                                               // Kdyby n�kdo v�d�l pro�, dejte mi pros�m v�d�t. Pro m� je to z�hadou.
@@loop :
  mov al,[ebx+0]
  mov ah,[ebx+2]
  mov [ebx+2],al
  mov [ebx+0],ah
  add ebx,3
  dec ecx
  jnz @@loop  }
var                                                                             // Klasika - jako p�i na��t�n� TGA textur
  i: integer;
  B, R: PGLubyte;
  temp: GLubyte;
begin
  for i := 0 to 256 * 256 - 1 do                                                // Proch�z� data obr�zku
    begin
    B := Pointer(Integer(buffer) + i * 3);                                      // Ukazatel na B
    R := Pointer(Integer(buffer) + i * 3+2);                                    // Ukazatel na R
    temp := B^;                                                                 // B ulo��me do pomocn� prom�nn�
    B^ := R^;                                                                   // R je na spr�vn�m m�st�
    R^ := temp;                                                                 // B je na spr�vn�m m�st�
    end;
end;

procedure OpenAVI(szFile: LPCSTR);                                              // Otev�e AVI soubor
var
  title: PAnsiChar;                                                             // Pro vyps�n� textu do titulku okna
  bmi: BITMAPINFO;
begin
  AVIFileInit;                                                                  // P�iprav� knihovnu AVIFile na pou�it�
  if AVIStreamOpenFromFile(pavi,szFile,streamtypeVIDEO,0,OF_READ,nil) <> 0 then // Otev�e AVI proud
    MessageBox(HWND_DESKTOP,'Failed To Open The AVI Stream','Error',MB_OK or MB_ICONEXCLAMATION); // Chybov� zpr�va
  AVIStreamInfo(pavi,psi,sizeof(psi));                                          // Na�te informace o proudu
  width := psi.rcFrame.Right - psi.rcFrame.Left;                                // V�po�et ���ky
  height := psi.rcFrame.Bottom - psi.rcFrame.Top;                               // V�po�et v��ky
  lastframe := AVIStreamLength(pavi);                                           // Posledn� sn�mek proudu
  mpf := AVIStreamSampleToTime(pavi,lastframe) div lastframe;                   // Po�et milisekund na jeden sn�mek
  with bmih do
    begin
    biSize := sizeof(BITMAPINFOHEADER);                                         // Velikost struktury
    biPlanes := 1;                                                              // BiPlanes
    biBitCount := 24;                                                           // Po�et bit� na pixel
    biWidth := 256;                                                             // ���ka bitmapy
    biHeight := 256;                                                            // V��ka bitmapy
    biCompression := BI_RGB;                                                    // RGB m�d
    end;
  bmi.bmiHeader := bmih;
  h_bitmap := CreateDIBSection(h_dc,bmi,DIB_RGB_COLORS,data,0,0);
  SelectObject(h_dc,h_bitmap);                                                  // Zvol� bitmapu do kontextu za��zen�
  pgf := AVIStreamGetFrameOpen(pavi,nil);                                       // Vytvo�� PGETFRAME pou�it�m po�adovan�ho m�du
  if pgf = nil then                                                             // Ne�sp�ch?
    MessageBox(HWND_DESKTOP,'Failed To Open The AVI Frame','Error',MB_OK or MB_ICONEXCLAMATION);
  title := PAnsiChar(Format('NeHe''s AVI Player: Width: %d, Height: %d, Frames: %d',[width,height,lastframe])); // Informace o videu (���ka, v��ka, po�et sn�mk�)
  SetWindowText(g_window.hWnd,title);                                           // Modifikace titulku okna
end;

procedure GrabAVIFrame(frame: integer);                                               // Grabuje po�adovan� sn�mek z proudu
var
  lpbi: PBitmapInfoHeader;                                                            // Hlavi�ka bitmapy
begin
  lpbi := AVIStreamGetFrame(pgf,frame);                                               // Grabuje data z AVI proudu
  pdata := Pointer(Integer(lpbi) + lpbi.biSize + lpbi.biClrUsed * sizeof(RGBQUAD));   // Ukazatel na data
  DrawDibDraw(hdd,h_dc,0,0,256,256,lpbi,pdata,0,0,width,height,0);                    // Konvertov�n� obr�zku na po�adovan� form�t
  flipIt(data);                                                                       // Prohod� R a B slo�ku pixel�
  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 256, 256, GL_RGB, GL_UNSIGNED_BYTE, data);  // Aktualizace textury
end;
 
procedure CloseAVI;                                                             // Zav�en� AVI souboru
begin
  DeleteObject(h_bitmap);                                                       // Sma�e bitmapu
  DrawDibClose(hdd);                                                            // Zav�e DIB
  //AVIStreamGetFrameClose(pgf);                                                  // Dealokace GetFrame zdroje - p�i pou�it� hod� chybu, nev�m pro�
  //AVIStreamRelease(pavi);                                                       // Uvoln�n� proudu - p�i pou�it� hod� chybu, nev�m pro�
  AVIFileExit;                                                                  // Uvoln�n� souboru
end;

function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
begin
  g_window := window;
  g_keys := key;
  h_dc := CreateCompatibleDC(0);                                                // Kontext za��zen�
  angle := 0.0;                                                                 // Na po��tku nulov� �hel
  hdd := DrawDibOpen;                                                           // Kontext za��zen� DIBu
  glClearColor(0.0, 0.0, 0.0, 0.5);	  	                                        // �ern� pozad�
  glClearDepth(1.0);                                                            // Nastaven� hloubkov�ho bufferu
  glDepthFunc(GL_LEQUAL);                                                       // Typ test� hloubky
  glEnable(GL_DEPTH_TEST);			                                                // Povol� hloubkov� testov�n�
  glShadeModel(GL_SMOOTH);                                                      // Jemn� st�nov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);                             // Perspektivn� korekce
  quadratic := gluNewQuadric;                                                   // Vytvo�� objekt quadraticu
  gluQuadricNormals(quadratic,GLU_SMOOTH);                                      // Norm�ly
  gluQuadricTexture(quadratic,GL_TRUE);                                         // Texturov� koordin�ty
  glEnable(GL_TEXTURE_2D);                                                      // Zapne texturov�n�
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);              // Filtry textur
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
  glTexGeni(GL_S,GL_TEXTURE_GEN_MODE,GL_SPHERE_MAP);                            // Automatick� generov�n� koordin�t�
  glTexGeni(GL_T,GL_TEXTURE_GEN_MODE,GL_SPHERE_MAP);
  OpenAVI('data/face2.avi');                                                    // Otev�en� AVI souboru
  glTexImage2D(GL_TEXTURE_2D,0,GL_RGB,256,256,0,GL_RGB,GL_UNSIGNED_BYTE,data);  // Vytvo�en� textury
  Result:=true;                                                                 // Inicializace prob�hla v po��dku
end;

procedure Deinitialize;                                                         // Uvoln�n� prost�edk�
begin
  CloseAVI;                                                                     // Zav�e AVI
end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohyb� ve sc�n� a stisk kl�ves
begin
  if g_keys.keyDown[VK_ESCAPE] then                                   // Kl�vesa ESC?
    TerminateApplication(g_window^);                                  // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                       // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                      // P�epnut� fullscreen/okno
  if g_keys.keyDown[Ord(' ')] and not sp then                         // Mezern�k
    begin
    sp := true;
    Inc(effect);                                                      // N�sleduj�c� objekt v �ad�
    if effect > 3 then effect := 0;                                   // P�ete�en�?
    end;
  if  not g_keys.keyDown[Ord(' ')] then                               // Uvoln�n� mezern�ku
    sp := false;
  if g_keys.keyDown[Ord('B')] and not bp then                         // Kl�vesa B
    begin
    bp := true;
    bg := not bg;                                                     // Nastav� flag pro zobrazov�n� pozad�
    end;
  if not g_keys.keyDown[Ord('B')] then                                // Uvoln�n� B
    bp := false;
  if g_keys.keyDown[Ord('E')] and not ep then                         // Kl�vesa E
    begin
    ep := true;
    env := not env;                                                   // Nastav� flag pro automatick� generov�n� texturov�ch koordin�t�
    end;
  if not g_keys.keyDown[Ord('E')] then                                // Uvoln�n� E
    ep := false;
  angle := angle + milliseconds / 60.0;                               // Aktualizace �hlu nato�en�
  Inc(next,milliseconds);                                             // Zv�t�en� next o uplynul� �as
  frame := next div mpf;                                              // V�po�et aktu�ln�ho sn�mku
  if frame >= lastframe then                                          // P�ete�en� sn�mk�?
    begin
    frame := 0;                                                       // P�eto�� video na za��tek
    next := 0;                                                        // Nulov�n� �asu
    end;
end;

procedure Draw;                                                       // Vykreslen� sc�ny
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                // Sma�e obrazovku a hloubkov� buffer
  GrabAVIFrame(frame);                                                // Nagrabuje po�adovan� sn�mek videa
  if bg then                                                          // Zobrazuje se pozad�?
    begin
    glLoadIdentity;	                                                  // Reset matice
    glBegin(GL_QUADS);                                                // Vykreslov�n� obd�ln�k�
      glTexCoord2f(1.0,1.0); glVertex3f( 11.0, 8.3,-20.0);
      glTexCoord2f(0.0,1.0); glVertex3f(-11.0, 8.3,-20.0);
      glTexCoord2f(0.0,0.0); glVertex3f(-11.0,-8.3,-20.0);
      glTexCoord2f(1.0,0.0); glVertex3f( 11.0,-8.3,-20.0);
    glEnd;
    end;
  glLoadIdentity;                                                     // Reset matice
  glTranslatef(0.0,0.0,-10.0);                                        // Posun do sc�ny
  if env then                                                         // Zapnuto generov�n� sou�adnic textur?
    begin
    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_T);
    end;
  glRotatef(angle*2.3,1.0,0.0,0.0);                                   // Rotace
  glRotatef(angle*1.8,0.0,1.0,0.0);
  glTranslatef(0.0,0.0,2.0);                                          // P�esun na novou pozici
  case effect of                                                      // V�tven� podle efektu
    0: begin                                                          // Krychle
       glRotatef(angle*1.3,1.0,0.0,0.0);                              // Rotace
       glRotatef(angle*1.1,0.0,1.0,0.0);
       glRotatef(angle*1.2,0.0,0.0,1.0);
       glBegin(GL_QUADS);                                             // Kreslen� obd�ln�k�
        // �eln� st�na
        glNormal3f(0.0,0.0,0.5);
        glTexCoord2f(0.0,0.0); glVertex3f(-1.0,-1.0,1.0);
        glTexCoord2f(1.0,0.0); glVertex3f( 1.0,-1.0,1.0);
        glTexCoord2f(1.0,1.0); glVertex3f( 1.0, 1.0,1.0);
        glTexCoord2f(0.0,1.0); glVertex3f(-1.0, 1.0,1.0);
        // Zadn� st�na
        glNormal3f(0.0,0.0,-0.5);
        glTexCoord2f(1.0,0.0); glVertex3f(-1.0,-1.0,-1.0);
        glTexCoord2f(1.0,1.0); glVertex3f(-1.0, 1.0,-1.0);
        glTexCoord2f(0.0,1.0); glVertex3f( 1.0, 1.0,-1.0);
        glTexCoord2f(0.0,0.0); glVertex3f( 1.0,-1.0,-1.0);
        // Horn� st�na
        glNormal3f(0.0,0.5,0.0);
        glTexCoord2f(0.0,1.0); glVertex3f(-1.0,1.0,-1.0);
        glTexCoord2f(0.0,0.0); glVertex3f(-1.0,1.0, 1.0);
        glTexCoord2f(1.0,0.0); glVertex3f( 1.0,1.0, 1.0);
        glTexCoord2f(1.0,1.0); glVertex3f( 1.0,1.0,-1.0);
        // Spodn� st�na
        glNormal3f(0.0,-0.5,0.0);
        glTexCoord2f(1.0,1.0); glVertex3f(-1.0,-1.0,-1.0);
        glTexCoord2f(0.0,1.0); glVertex3f( 1.0,-1.0,-1.0);
        glTexCoord2f(0.0,0.0); glVertex3f( 1.0,-1.0, 1.0);
        glTexCoord2f(1.0,0.0); glVertex3f(-1.0,-1.0, 1.0);
        // Prav� st�na
        glNormal3f(0.5,0.0,0.0);
        glTexCoord2f(1.0,0.0); glVertex3f(1.0,-1.0,-1.0);
        glTexCoord2f(1.0,1.0); glVertex3f(1.0, 1.0,-1.0);
        glTexCoord2f(0.0,1.0); glVertex3f(1.0, 1.0, 1.0);
        glTexCoord2f(0.0,0.0); glVertex3f(1.0,-1.0, 1.0);
        // Lev� st�na
        glNormal3f(-0.5,0.0,0.0);
        glTexCoord2f(0.0,0.0); glVertex3f(-1.0,-1.0,-1.0);
        glTexCoord2f(1.0,0.0); glVertex3f(-1.0,-1.0, 1.0);
        glTexCoord2f(1.0,1.0); glVertex3f(-1.0, 1.0, 1.0);
        glTexCoord2f(0.0,1.0); glVertex3f(-1.0, 1.0,-1.0);
       glEnd();
       end;
    1: begin                                                          // Koule
       glRotatef(angle*1.3,1.0,0.0,0.0);                              // Rotace
       glRotatef(angle*1.1,0.0,1.0,0.0);
       glRotatef(angle*1.2,0.0,0.0,1.0);
       gluSphere(quadratic,1.3,20,20);                                // Vykreslen� koule
       end;
    2: begin                                                          // V�lec
       glRotatef(angle*1.3,1.0,0.0,0.0);                              // Rotace
       glRotatef(angle*1.1,0.0,1.0,0.0);
       glRotatef(angle*1.2,0.0,0.0,1.0);
       glTranslatef(0.0,0.0,-1.5);                                    // Vycentrov�n�
       gluCylinder(quadratic,1.0,1.0,3.0,32,32);                      // Vykreslen� v�lce
       end;
    end;
  if env then                                                         // Zapnuto generov�n� sou�adnic textur?
    begin
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    end;
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
    init.title := 'NeHe''s New GL Framework';
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


