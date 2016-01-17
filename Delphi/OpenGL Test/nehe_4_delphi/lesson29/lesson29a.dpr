program lesson29a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages, sysutils,
  OpenGL;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

type
  TEXTURE_IMAGE = record            // Struktura obr�zku
    width: integer;                 // ���ka v pixelech
    height: integer;                // V��ka v pixelech
    format: integer;                // Barevn� hloubka v bytech na pixel
    data: PGLubyte;                 // Data obr�zku
    end;

  P_TEXTURE_IMAGE = ^TEXTURE_IMAGE; // Datov� typ ukazatele na obr�zek

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  t1, t2: P_TEXTURE_IMAGE;          // Dva obr�zky
  texture: GLuint;                  // Jedna textura
  xrot: GLfloat;                    // X rotace
  yrot: GLfloat;                    // Y rotace
  zrot: GLfloat;                    // Z rotace

function AllocateTextureBuffer(w, h, f: GLint): P_TEXTURE_IMAGE;  // Alokuje pam� pro obr�zek
var
  ti: P_TEXTURE_IMAGE;                                            // Ukazatel na strukturu obr�zku
  c: PGLubyte;                                                    // Ukazatel na data obr�zku
begin
  ti := nil;
  c := nil;
  ti := AllocMem(SizeOf(TEXTURE_IMAGE));                          // Alokace pam�ti pro strukturu
  if ti <> nil then                                               // Poda�ila se alokace pam�ti?
    begin
    ti.width := w;                                                // Nastav� atribut ���ky
    ti.height := h;                                               // Nastav� atribut v��ky
    ti.format := f;                                               // Nastav� atribut barevn� hloubky
    c := AllocMem(w*h*f);                                         // Alokace pam�ti pro strukturu
    if c <> nil then                                              // Poda�ila se alokace pam�ti?
      ti.data := c                                                // Nastav� ukazatel na data
      else                                                        // Alokace pam�ti pro data se nepoda�ila
      begin
      MessageBox(h_Wnd,'Could Not Allocate Memory For A Texture Buffer','BUFFER ERROR',MB_OK or MB_ICONINFORMATION);
      FreeMem(ti);                                                // Uvoln�n� pam�ti struktury
      ti := nil;
      Result := nil;
      exit;
      end;
    end
    else                                                          // Alokace pam�ti pro strukturu se nepoda�ila
    begin
    MessageBox(h_Wnd,'Could Not Allocate An Image Structure','IMAGE STRUCTURE ERROR',MB_OK or MB_ICONINFORMATION);
    Result := nil;
    exit;
    end;
  Result := ti;                                                   // Vr�t� ukazatel na dynamickou pam�
end;

procedure DeallocateTexture(t: P_TEXTURE_IMAGE);                  // Uvoln� dynamicky alokovanou pam� obr�zku
begin
  if t <> nil then                                                // Pokud struktura obr�zku existuje
    begin
    if t.data <> nil then                                         // Pokud existuj� data obr�zku
      FreeMem(t.data);                                            // Uvoln� data obr�zku
    FreeMem(t);                                                   // Uvoln� strukturu obr�zku
    end;
end;

function ReadTextureData(filename: string; buffer: P_TEXTURE_IMAGE): integer;   // Na�te data obr�zku
var
  f: file;                                                                      // Handle souboru
  i, j, k: integer;                                                             // ��d�c� prom�nn� cykl�
  done: integer;                                                                // Po�et na�ten�ch byt� ze souboru (n�vratov� hodnota)
  stride: integer;                                                              // Velikost ��dku
  p: PGLubyte;                                                                  // Ukazatel na aktu�ln� byte pam�ti
begin
  done := 0;
  stride := buffer.width * buffer.format;                                       // Velikost ��dku
  p := nil;
  AssignFile(f,filename);                                                       // Otev�e soubor
  {$I-}
  Reset(f,1);
  {$I+}
  if IOResult = 0 then                                                          // Poda�ilo se ho otev��t?
    begin
    for i := buffer.height-1 downto 0 do                                        // Od zdola nahoru po ��dc�ch
      begin
      p := Pointer(Integer(buffer.data) + i * stride);                          // P ukazuje na po�adovan� ��dek
      for j := 0 to buffer.width-1 do                                           // Zleva doprava po pixelech
        begin
        for k := 0 to buffer.format-2 do                                        // Jednotliv� byty v pixelu
          begin
          BlockRead(f,p^,1);                                                    // Na�te R, G a B slo�ku barvy
          Inc(p);
          Inc(done);
          end;
        p^ := 255;                                                              // Alfa nepr�hledn� (ru�n� nastaven�)
        Inc(p);                                                                 // Ukazatel na dal�� byte
        end;
      end;
    CloseFile(f);                                                               // Zav�e soubor
    end
    else                                                                        // Soubor se nepoda�ilo otev��t
    begin
    MessageBox(h_Wnd,'Unable To Open Image File','IMAGE ERROR',MB_OK or MB_ICONINFORMATION);
    end;
  Result := done;                                                               // Vr�t� po�et na�ten�ch byt�
end;

procedure BuildTexture(tex: P_TEXTURE_IMAGE);                                   // Vytvo�� texturu
begin
  glGenTextures(1,texture);                                                     // Generuje texturu
  glBindTexture(GL_TEXTURE_2D,texture);                                         // Vybere texturu za aktu�ln�
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);               // Line�rn� filtrov�n�
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  gluBuild2DMipmaps(GL_TEXTURE_2D,GL_RGB,tex.width,tex.height,GL_RGBA,GL_UNSIGNED_BYTE,tex.data); // Mipmapovan� textura
end;
                                                                                // Blitting obr�zk�
procedure Blit(src, dst: P_TEXTURE_IMAGE;                                       // Zdrojov� obr�zek, C�lov� obr�zek
                src_xstart, src_ystart,                                         // Lev� horn� bod kop�rovan� oblasti
                src_width, src_height,                                          // ���ka a v��ka kop�rovan� oblasti
                dst_xstart, dst_ystart,                                         // Kam kop�rovat (lev� horn� bod)
                blend, alpha: integer);                                         // Pou��t blending? Hodnota alfy p�i blendingu
var
  i, j, k: integer;                                                             // ��d�c� prom�nn� cykl�
  s, d: PGLubyte;                                                               // Pomocn� ukazatele na data zdroje a c�le
begin
  if alpha > 255 then alpha := 255;                                             // Je alfa mimo rozsah?
  if alpha < 0 then alpha := 0;
  if blend < 0 then blend := 0;                                                 // Je blending mimo rozsah?
  if blend > 1 then blend := 1;
  d := Pointer(Integer(dst.data) + dst_ystart * dst.width * dst.format);        // Ukazatele na prvn� kop�rovan� ��dek
  s := Pointer(Integer(src.data) + src_ystart * src.width * src.format);
  for i := 0 to src_height-1 do                                                 // ��dky, ve kter�ch se kop�ruj� data
    begin
    s := Pointer(Integer(s) + src_xstart * src.format);                         // Posun na prvn� kop�rovan� pixel v ��dku
    d := Pointer(Integer(d) + dst_xstart * dst.format);
    for j := 0 to src_width-1 do                                                // Pixely v ��dku, kter� se maj� kop�rovat
      for k := 0 to src.format-1 do                                             // Byty v kop�rovan�m pixelu
        begin
        if blend <> 0 then                                                      // Je po�adov�n blending?
          d^ := (s^ * alpha + d^ * (255 - alpha)) shr 8                         // Slou�en� dvou pixel� do jednoho
          else                                                                  // Bez blendingu
          d^ := s^;                                                             // Oby�ejn� kop�rov�n�
        Inc(d);
        Inc(s);
        end;
    d := Pointer(Integer(d) + (dst.width - (src_width + dst_xstart)) * dst.format); // Sko�� ukazatelem na konec ��dku
    s := Pointer(Integer(s) + (src.width - (src_width + src_xstart)) * src.format);                         // Posun na prvn� kop�rovan� pixel v ��dku
    end;
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
  t1 := AllocateTextureBuffer(256,256,4);           // Alokace pam�ti pro prvn� obr�zek
  if ReadTextureData('Data/Monitor.raw',t1) = 0 then    // Nahraje data obr�zku
    begin                                               // Nic se nenahr�lo
    MessageBox(h_Wnd,'Could Not Read ''Monitor.raw'' Image Data','TEXTURE ERROR',MB_OK or MB_ICONINFORMATION);
    Result := false;
    exit;
    end;
  t2 := AllocateTextureBuffer(256,256,4);           // Alokace pam�ti pro druh� obr�zek
  if ReadTextureData('Data/GL.raw',t2) = 0 then     // Nahraje data obr�zku
    begin                                           // Nic se nenahr�lo
    MessageBox(h_Wnd,'Could Not Read ''GL.raw'' Image Data','TEXTURE ERROR',MB_OK or MB_ICONINFORMATION);
    Result := false;
    exit;
    end;                                            // Blitting obr�zk�
  Blit(t2,                                          // Zdrojov� obr�zek
        t1,                                         // C�lov� obr�zek
        127,                                        // Lev� horn� bod kop�rovan� oblasti
        127,                                        // Lev� horn� bod kop�rovan� oblasti
        128,                                        // ���ka kop�rovan� oblasti
        128,                                        // V��ka kop�rovan� oblasti
        64,                                         // Kam kop�rovat (lev� horn� bod)
        64,                                         // Kam kop�rovat (lev� horn� bod)
        1,                                          // Pou��t blending?
        128);                                       // Hodnota alfy p�i blendingu
  BuildTexture(t1);                                 // Vytvo�� texturu
  DeallocateTexture(t1);                            // Uvoln� pam� obr�zk�
  DeallocateTexture(t2);                            
  glEnable(GL_TEXTURE_2D);                          // Zapne texturov�n�
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glClearColor(0.0, 0.0, 0.0, 0.0);	  	            // �ern� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glEnable(GL_DEPTH_TEST);			                    // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LESS); 				                    // Typ hloubkov�ho testov�n�
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;


function DrawGLScene():bool;                            // Vykreslov�n�
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);  // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity();	                                    // Reset matice
  glTranslatef(0.0,0.0,-10.0);                          // P�esun do hloubky
  glRotatef(xrot,1.0,0.0,0.0);                          // Rotace
  glRotatef(yrot,0.0,1.0,0.0);
  glRotatef(zrot,0.0,0.0,1.0);
  glBindTexture(GL_TEXTURE_2D,texture);                 // Zvol� texturu
  glBegin(GL_QUADS);                                    // Za��tek kreslen� obd�ln�k�
    // �eln� st�na
    glNormal3f(0.0,0.0,1.0);
    glTexCoord2f(1.0,1.0); glVertex3f( 1.0, 1.0, 1.0);
    glTexCoord2f(0.0,1.0); glVertex3f(-1.0, 1.0, 1.0);
    glTexCoord2f(0.0,0.0); glVertex3f(-1.0,-1.0, 1.0);
    glTexCoord2f(1.0,0.0); glVertex3f( 1.0,-1.0, 1.0);
    // Zadn� st�na
    glNormal3f(0.0,0.0,-1.0);
    glTexCoord2f(1.0,1.0); glVertex3f(-1.0, 1.0,-1.0);
    glTexCoord2f(0.0,1.0); glVertex3f( 1.0, 1.0,-1.0);
    glTexCoord2f(0.0,0.0); glVertex3f( 1.0,-1.0,-1.0);
    glTexCoord2f(1.0,0.0); glVertex3f(-1.0,-1.0,-1.0);
    // Horn� st�na
    glNormal3f(0.0,1.0,0.0);
    glTexCoord2f(1.0,1.0); glVertex3f( 1.0, 1.0,-1.0);
    glTexCoord2f(0.0,1.0); glVertex3f(-1.0, 1.0,-1.0);
    glTexCoord2f(0.0,0.0); glVertex3f(-1.0, 1.0, 1.0);
    glTexCoord2f(1.0,0.0); glVertex3f( 1.0, 1.0, 1.0);
    // Doln� st�na
    glNormal3f(0.0,-1.0,0.0);
    glTexCoord2f(0.0,0.0); glVertex3f( 1.0,-1.0, 1.0);
    glTexCoord2f(1.0,0.0); glVertex3f(-1.0,-1.0, 1.0);
    glTexCoord2f(1.0,1.0); glVertex3f(-1.0,-1.0,-1.0);
    glTexCoord2f(0.0,1.0); glVertex3f( 1.0,-1.0,-1.0);
    // Prav� st�na
    glNormal3f(1.0,0.0,0.0);
    glTexCoord2f(1.0,0.0); glVertex3f( 1.0,-1.0,-1.0);
    glTexCoord2f(1.0,1.0); glVertex3f( 1.0, 1.0,-1.0);
    glTexCoord2f(0.0,1.0); glVertex3f( 1.0, 1.0, 1.0);
    glTexCoord2f(0.0,0.0); glVertex3f( 1.0,-1.0, 1.0);
    // Lev� st�na
    glNormal3f(-1.0,0.0,0.0);
    glTexCoord2f(0.0,0.0); glVertex3f(-1.0,-1.0,-1.0);
    glTexCoord2f(1.0,0.0); glVertex3f(-1.0,-1.0, 1.0);
    glTexCoord2f(1.0,1.0); glVertex3f(-1.0, 1.0, 1.0);
    glTexCoord2f(0.0,1.0); glVertex3f(-1.0, 1.0,-1.0);
  glEnd();                                              // Konec kreslen�
  xrot := xrot + 0.3;                                   // Zv�t�� �hly rotace
  yrot := yrot + 0.2;
  zrot := zrot + 0.4;
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
          if not Active then WaitMessage;             // Je program neaktivn�? // �ekej na zpr�vu a zat�m nic ned�lej
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
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

