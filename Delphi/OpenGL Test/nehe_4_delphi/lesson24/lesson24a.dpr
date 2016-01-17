program lesson24a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages, sysutils,
  OpenGL;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

type
  TextureImage = record             // Struktura textury
    imageData: ^GLubyte;            // Data obr�zku
    bpp: GLuint;                    // Barevn� hloubka obr�zku
    width: GLuint;                  // ���ka obr�zku
    height: GLuint;                 // V��ka obr�zku
    textID: GLuint;                 // Vytvo�en� textura
    end;

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  Scroll: integer;                  // Pro rolov�n� okna
  MaxTokens: integer;               // Po�et podporovan�ch roz���en�
  Base: GLuint;                     // Z�kladn� display list fontu
  swidth, sheight: integer;         // ���ka, V��ka o�ezan� oblasti
  textures: array [0..0] of TextureImage; // Jedna textura

function LoadTGA(var texture: TextureImage; filename: string): bool;
type
  TData = array [0..255] of GLubyte;
var
  TGAheader: array [0..11] of GLbyte;             // Nekomprimovan� TGA hlavi�ka { 0,0,2,0,0,0,0,0,0,0,0,0 }
  TGAcompare: array [0..11] of GLbyte;            // Pro porovn�n� TGA hlavi�ky
  Header: array [0..5] of GLbyte;                 // Prvn�ch 6 u�ite�n�ch byt� z hlavi�ky
  BytesPerPixel: GLuint;                          // Po�et byt� na pixel pou�it� v TGA souboru
  ImageSize: GLuint;                              // Ukl�d� velikost obr�zku p�i alokov�n� RAM
  temp: GLuint;                                   // Pomocn� prom�nn�
  TypeMode: GLuint;                               // GL m�d
  f: file;                                        // Soubor TGA
  precteno: Gluint;                               // Po�et p�e�ten�ch bytu
  i: integer;                                     // Cyklus
  B, R: PGluint;                                  // Ukazatel na prohazovan� slo�ky barev
begin
  ZeroMemory(@TGAheader,sizeof(TGAheader));       // Nulov�n� prvk� pole
  TGAheader[2] := 2;                              // T�et� prvek hlavi�ky je 2 - viz deklarace
  TypeMode := GL_RGBA;                            // Implicitn�m GL m�dem je RGBA (32 BPP)
  if not FileExists(filename) then                // Existuje soubor?
    begin
    Result := false;                                                  // Konec funkce
    exit;
    end;
  AssignFile(f,filename);                                             // P�i�azen� souboru
  Reset(f,1);                                                         // Otev�e TGA soubor
  BlockRead(f,TGAcompare,Sizeof(TGAcompare),precteno);                // Na�te hlavi�ku
  if (precteno <> Sizeof(TGAcompare)) or                              // Poda�ilo se na��st 12 byt�?
    (not CompareMem(@TGAcompare,@TGAHeader,sizeof(TGAheader))) then   // Maj� pot�ebn� hodnoty?
    begin
    Result := false;                                                  // Konec funkce
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  BlockRead(f,Header,Sizeof(Header),precteno);                        // Pokud ano, na�te dal��ch �est byt�
  if precteno <> sizeof(Header) then                                  // Poda�ilo se na��st 6 byt�?
    begin
    Result := false;                                                  // Konec funkce
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  texture.width := Header[1] * 256 + Header[0];                       // Z�sk� ���ku obr�zku
  texture.height := Header[3] * 256 + Header[2];                      // Z�sk� v��ku obr�zku
  if (texture.width <= 0) or (texture.height <= 0)                    // Platn� ���ka? Platn� v��ka?
    or ((Header[4] <> 24) and (Header[4] <> 32)) then                 // Platn� barevn� hloubka?
    begin
    Result := false;                                                  // Konec funkce
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  texture.bpp := Header[4];                                           // Bity na pixel (24 nebo 32)
  BytesPerPixel := texture.bpp div 8;                                 // Byty na pixel
  ImageSize := texture.width * texture.height * BytesPerPixel;        // Velikost pam�ti pro data obr�zku
  texture.imageData := AllocMem(ImageSize);                           // Alokace pam�ti pro data obr�zku
  if texture.imageData = nil then                                     // Poda�ilo se pam� alokovat?
    begin
    Result := false;                                                  // Konec funkce
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  BlockRead(f,texture.imageData^,ImageSize,precteno);                 // Kop�rov�n� dat
  if precteno <> ImageSize then                                       // Poda�ilo se kop�rov�n� dat?
    begin
    Result := false;                                                  // Konec funkce
    FreeMem(texture.imageData);                                       // Uvoln� pam�
    CloseFile(f);                                                     // Zav�e soubor
    exit;
    end;
  for i := 0 to (texture.width * texture.height) - 1 do               // Proch�z� data obr�zku
    begin
    B := Pointer(Integer(texture.imageData) + i * BytesPerPixel);     // Ukazatel na B
    R := Pointer(Integer(texture.imageData) + i * BytesPerPixel+2);   // Ukazatel na R
    temp := B^;                                                       // B ulo��me do pomocn� prom�nn�
    B^ := R^;                                                         // R je na spr�vn�m m�st�
    R^ := temp;                                                       // B je na spr�vn�m m�st�
    end;
  CloseFile(f);                                                       // Zav�e soubor
  glGenTextures(1,texture.textID);                                    // Generuje texturu
  glBindTexture(GL_TEXTURE_2D, texture.textID);                       // Zvol� texturu
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);   // Line�rn� filtrov�n�
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);   // Line�rn� filtrov�n�
  if texture.bpp = 24 then TypeMode := GL_RGB;                        // Je obr�zek 24 bitov�? Nastav� typ na GL_RGB
  glTexImage2D(GL_TEXTURE_2D, 0, TypeMode, texture.width, texture.height, 0, TypeMode, GL_UNSIGNED_BYTE, texture.imageData);// Vytvo�� texturu
  FreeMem(texture.imageData);                                         // Uvoln� pam�
  Result := true;
end;

procedure ReSizeGLScene(Width: GLsizei; Height: GLsizei); // Zm�na velikosti a inicializace OpenGL okna
begin
  swidth := Width;                                        // ���ka okna
  sheight := Height;                                      // V��ka okna
  if (Height=0) then		                                  // Zabezpe�en� proti d�len� nulou
     Height:=1;                                           // Nastav� v��ku na jedna
  glViewport(0, 0, Width, Height);                        // Resetuje aktu�ln� nastaven�
  glMatrixMode(GL_PROJECTION);                            // Zvol� projek�n� matici
  glLoadIdentity();                                       // Reset matice
  glOrtho(0.0,640,480,0.0,-1.0,1.0);                      // Pravo�hl� projekce 640x480, [0; 0] vlevo naho�e
  glMatrixMode(GL_MODELVIEW);                             // Zvol� matici Modelview
  glLoadIdentity;                                         // Reset matice
end;

procedure BuildFont;                                      // Vytvo�en� display list� fontu
var
  cx, cy: GLfloat;                                        // Koordin�ty x, y
  loop: integer;
begin
  base := glGenLists(256);                                // 256 display list�
  glBindTexture(GL_TEXTURE_2D,textures[0].textID);        // V�b�r textury
  for loop:=0 to 255 do                                   // Vytv��� 256 display list�
    begin
    cx := (loop mod 16) / 16;                             // X pozice aktu�ln�ho znaku
    cy := (loop div 16) /16;                              // Y pozice aktu�ln�ho znaku
    glNewList(base + loop,GL_COMPILE);                    // Vytvo�en� display listu
      glBegin(GL_QUADS);                                  // Pro ka�d� znak jeden obd�ln�k
        glTexCoord2f(cx,1-cy-0.0625);glVertex2i(0,16);
        glTexCoord2f(cx+0.0625,1-cy-0.0625);glVertex2i(16,16);
        glTexCoord2f(cx+0.0625,1-cy-0.001);glVertex2i(16,0);
        glTexCoord2f(cx,1-cy-0.001);glVertex2i(0,0);
      glEnd;
      glTranslated(14,0,0);                               // P�esun na pravou stranu znaku
    glEndList;                                            // Konec kreslen� display listu
    end;
end;

procedure KillFont;                                       // Uvoln� pam� fontu
begin
  glDeleteLists(base,256);                                // Sma�e 256 display list�
end;

procedure glPrint(x,y: GLint;text: string;sada: integer); // Prov�d� v�pis textu
begin
  if text = '' then exit;
  if sada>1 then sada:=1;
  glEnable(GL_TEXTURE_2D);                                // Zapne texturov� mapov�n�
  glLoadIdentity;                                         // Reset matice
  glTranslated(x,y,0);                                    // P�esun na po�adovanou pozici
  glListBase(base-32+(128*sada));                         // Zvol� znakovou sadu
  glScalef(1.0,2.0,1.0);                                  // Zm�na m���tka
  glCallLists(length(text),GL_UNSIGNED_BYTE,Pchar(text)); // V�pis textu na monitor
  glDisable(GL_TEXTURE_2D);                               // Vypne texturov� mapov�n�
end;

function InitGL:bool;	                              // V�echno nastaven� OpenGL
begin
  if not LoadTGA(textures[0],'Data/Font.TGA') then  // Nahraje texturu fontu z TGA obr�zku
    begin
    Result := false;
    exit;
    end;
  BuildFont;                                        // Sestav� font
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glClearColor(0.0, 0.0, 0.0, 0.5);	  	            // �ern� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glBindTexture(GL_TEXTURE_2D,textures[0].textID);  // Zvol� texturu
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;


function DrawGLScene():bool;                            // Vykreslov�n�
var
  token: string;                                        // Ukl�d� jedno roz���en�
  cnt: integer;                                         // ��ta� roz���en�
  texts: string;                                        // V�echna roz���en�
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);  // Sma�e obrazovku a hloubkov� buffer
  glColor3f(1.0,0.5,0.5);                               // �erven� barva
  glPrint(50,16,'RENDERER',1);                          // V�pis nadpisu pro grafickou kartu
  glPrint(80,48,'VENDOR',1);                            // V�pis nadpisu pro v�robce
  glPrint(66,80,'VERSION',1);                           // V�pis nadpisu pro verzi
  glColor3f(1.0,0.7,0.4);                               // Oran�ov� barva
  glPrint(200,16,glGetString(GL_RENDERER),1);           // V�pis typu grafick� karty
  glPrint(200,48,glGetString(GL_VENDOR),1);             // V�pis v�robce
  glPrint(200,80,glGetString(GL_VERSION),1);            // V�pis verze
  glColor3f(0.5,0.5,1.0);                               // Modr� barva
  glPrint(192,432,'NeHe Productions',1);                // V�pis NeHe Productions
  glLoadIdentity();	                                    // Reset matice
  glColor3f(1.0,1.0,1.0);                               // B�l� barva
  glBegin(GL_LINE_STRIP);                               // Za��tek kreslen� linek
    glVertex2d(639,417);                                // 1
    glVertex2d(0,417);                                  // 2
    glVertex2d(0,480);                                  // 3
    glVertex2d(639,480);                                // 4
    glVertex2d(639,128);                                // 5
  glEnd();                                              // Konec kreslen�
  glBegin(GL_LINE_STRIP);                               // Za��tek kreslen� linek
    glVertex2d(0,128);                                  // 6
    glVertex2d(639,128);                                // 7
    glVertex2d(639,1);                                  // 8
    glVertex2d(0,1);                                    // 9
    glVertex2d(0,417);                                  // 10
  glEnd();                                              // Konec kreslen�
  glScissor(1,Round(0.135416*sheight),swidth-2,Round(0.597916*sheight));  // Definov�n� o�ez�vac� oblasti
  glEnable(GL_SCISSOR_TEST);                            // Povol� o�ez�vac� testy
  texts := glGetString(GL_EXTENSIONS);                  // Zkop�ruje seznam roz���en� do text
  token := copy(texts,1,Pos(' ',texts));                // Z�sk� prvn� pod�et�zec
  Delete(texts,1,Pos(' ',texts));                       // Sma�e prvn� pod�et�zec
  while token <> '' do                                  // Proch�z� podporovan� roz���en�
    begin
    Inc(cnt);                                           // Inkrementuje ��ta�
    if cnt > MaxTokens then MaxTokens := cnt;           // Je maximum men�� ne� hodnota ��ta�e? // Aktualizace maxima
    glColor3f(0.5,1.0,0.5);                             // Zelen� barva
    glPrint(0,96+(cnt*32)-Scroll,IntToStr(cnt),0);      // Po�ad� aktu�ln�ho roz���en�
    glColor3f(1.0,1.0,0.5);                             // �lut� barva
    glPrint(50,96+(cnt*32)-Scroll,token,0);             // Vyp�e jedno roz���en�
    token := copy(texts,1,Pos(' ',texts));              // Najde dal�� roz���en�
    Delete(texts,1,Pos(' ',texts));                     // Sma�e dal�� roz���en�
    end;
  glDisable(GL_SCISSOR_TEST);                           // Vypne o�ez�vac� testy
  glFlush;                                              // Vypr�zdn� renderovac� pipeline
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
  KillFont;                                             // Sma�e font
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
          if (keys[VK_UP] and (scroll > 0)) then      // �ipka nahoru?
            scroll := scroll - 2;                     // Posune text nahoru
          if (keys[VK_DOWN] and (scroll < 32*(maxtokens-9))) then   // �ipka dol�?
            scroll := scroll + 2;                                   // Posune text dol�
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

