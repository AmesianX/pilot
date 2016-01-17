program lesson10a;

{   k�d pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL, sysutils,
  GLaux;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

type
  vertex = record                       // Struktura bodu
    x,y,z: GLfloat;                     // Sou�adnice v prostoru
    u,v: GLfloat;                       // Texturov� koordin�ty
    end;
  triangle = record                     // Struktura troj�heln�ku
    vertex: array [0..3] of vertex;     // Pole t�� bod�
    end;
  sector = record                       // Struktura sektoru
    numtriangles: integer;              // Po�et troj�heln�k� v sektoru
    triangle: array of triangle;        // Ukazatel na dynamick� pole troj�heln�k�
    end;

const
  piover180 = 0.0174532925;             // Zjednodu�� p�evod mezi stupni a radi�ny
  worldfile = 'world.txt';              // Soubor s informacemi o sv�t�

var
  h_Rc: HGLRC;		                  // Trval� Rendering Context
  h_Dc: HDC;                        // Priv�tn� GDI Device Context
  h_Wnd: HWND;                      // Obsahuje Handle na�eho okna
  keys: array [0..255] of BOOL;	    // Pole pro ukl�d�n� vstupu z kl�vesnice
  Active: bool = true;              // Ponese informaci o tom, zda je okno aktivn�
  FullScreen:bool = true;           // Ponese informaci o tom, zda je program ve fullscreenu
  blend: bool;                      // Blending ON/OFF
  bp: bool;                         // B stisknuto? (blending)
  fp: bool;                         // F stisknuto? (texturov� filtry)
  heading: GLfloat;                 // Pomocn� pro p�epo��t�v�n� xpos a zpos p�i pohybu
  xpos: GLfloat;                    // Ur�uje x-ov� sou�adnice na podlaze
  zpos: GLfloat;                    // Ur�uje z-ov� sou�adnice na podlaze
  yrot: GLfloat;                    // Y rotace (nato�en� sc�ny doleva/doprava - sm�r pohledu)
  walkbias: GLfloat = 0;            // Houp�n� sc�ny p�i pohybu (simulace krok�)
  walkbiasangle: GLfloat = 0;       // Pomocn� pro vypo��t�n� walkbias
  lookupdown: GLfloat = 0;          // Ur�uje �hel nato�en� pohledu nahoru/dol�
  filter: GLuint;                   // Pou�it� texturov� filtr
  texture: array [0..2] of GLuint;  // Ukl�d� textury
  sector1: sector;                  // Bude obsahovat v�echna data 3D sv�ta

procedure ReadStr(var f: textfile; var s: string);        // Na�te jeden pou�iteln� ��dek ze souboru
begin
  readln(f,s);                                            // Na�ti ��dek
  while ((copy(s, 1, 1) = '/') or (length(s) = 0)) do     // Pokud nen� pou�iteln� na�ti dal��
    readln(f,s);
end;

procedure SetupWorld;                                     // Na�ti 3D sv�t ze souboru
var
  x,y,z,u,v: GLfloat;                                     // body v prostoru a koordin�ty textur
  numtriangles: integer;                                  // Po�et troj�heln�k�
  filein: textfile;                                       // Ukazatel na soubor
  oneline: string;                                        // Znakov� buffer
  loop,vert: integer;                                     // cykly
begin
  //*************************************************************************************************
  //                                 UPOZORN�N�!!!!!!!!
  // Soubor world.txt nen� stejn� jako u origin�ln�ho tutori�lu. Upravil jsem jeho strukturu
  // z d�vodu snaz��ho na��t�n� dat.
  //*************************************************************************************************
  AssignFile(filein,worldfile);
  Reset(filein);                                          // Otev�en� souboru pro �ten�
  Readstr(filein,oneline);                                // Na�ten� prvn�ho pou�iteln�ho ��dku
  Delete(oneline,1,11);
  numtriangles := StrToIntDef(oneline,0);                 // Vyjmeme po�et troj�heln�k�
  SetLength(sector1.triangle,numtriangles);               // Alokace pot�ebn� pam�ti
  sector1.numtriangles := numtriangles;                   // Ulo�en� po�tu troj�heln�k�
  for loop := 0 to numtriangles - 1 do                    // Proch�z� troj�heln�ky
    for vert := 0 to 2 do                                 // Proch�z� vrcholy troj�heln�k�
      begin
      Readstr(filein,oneline);                            // Na�te ��dek
      x := StrToFloat(Copy(oneline,1,4));                 // Na�ten� do pomocn�ch prom�nn�ch
      y := StrToFloat(Copy(oneline,5,4));
      z := StrToFloat(Copy(oneline,9,4));
      u := StrToFloat(Copy(oneline,13,4));
      v := StrToFloat(Copy(oneline,17,4));
      sector1.triangle[loop].vertex[vert].x := x;         // Inicializuje jednotliv� polo�ky struktury
      sector1.triangle[loop].vertex[vert].y := y;
      sector1.triangle[loop].vertex[vert].z := z;
      sector1.triangle[loop].vertex[vert].u := u;
      sector1.triangle[loop].vertex[vert].v := v;
      end;
  CloseFile(filein);                                      // Zav�e soubor
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

function LoadGLTextures: Bool;                                        // Loading bitmapy a konverze na texturu
var TextureImage: PTAUX_RGBImageRec;                                  // Ukl�d� bitmapu
    Status: Bool;                                                     // Indikuje chyby
begin
  Status := false;
  ZeroMemory(TextureImage,sizeof(TextureImage));                      // Vynuluje pam�
  TextureImage := LoadBMP('Data/mud.bmp');                            // Nahraje bitmapu
  if Assigned(TextureImage) then
    begin
    Status := true;                                                   // V�e je bez probl�m�
    glGenTextures(3,texture[0]);                                      // Generuje t�i textury
    // Vytvo�� neline�rn� filtrovanou texturu
    glBindTexture(GL_TEXTURE_2D,texture[0]);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D,0,3,TextureImage.sizeX,TextureImage.sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage.data);
    // Vytvo�� line�rn� filtrovanou texturu
    glBindTexture(GL_TEXTURE_2D,texture[1]);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D,0,3,TextureImage.sizeX,TextureImage.sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,TextureImage.data);
    // Vytvo�� mipmapovanou texturu
    glBindTexture(GL_TEXTURE_2D,texture[2]);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
    gluBuild2DMipmaps(GL_TEXTURE_2D,3,TextureImage.sizeX,TextureImage.sizeY,GL_RGB,GL_UNSIGNED_BYTE,TextureImage.data);
    end;
  Result := Status;                                                   // Ozn�m� p��padn� chyby
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
  decimalseparator := '.';                          // Nastav� odd�lova� desetinn�ch m�st podle anglick�ch zvyklost� - nutn� pro spr�vn� na��t�n� koordin�t� ze souboru
  if not LoadGLTextures then                        // Nahraje texturu
    begin
    Result := false;
    exit;
    end;
  glEnable(GL_TEXTURE_2D);                          // Zapne mapov�n� textur
  glShadeModel(GL_SMOOTH);			                    // Povol� jemn� st�nov�n�
  glBlendFunc(GL_SRC_ALPHA,GL_ONE);                 // Nastaven� blendingu pro pr�hlednost
  glClearColor(0.0, 0.0, 0.0, 0.0);	  	            // �ern� pozad�
  glClearDepth(1.0);				                        // Nastaven� hloubkov�ho bufferu
  glEnable(GL_DEPTH_TEST);			                    // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LESS);				                      // Typ hloubkov�ho testov�n�
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST); // Nejlep�� perspektivn� korekce
  SetupWorld;                                       // Loading 3D sv�ta
  Result:=true;                                     // Inicializace prob�hla v po��dku
end;


function DrawGLScene():bool;                            // Vykreslov�n�
var
  x_m, y_m, z_m, u_m, v_m: GLfloat;                     // Pomocn� sou�adnice a koordin�ty textury
  xtrans, ztrans, ytrans: GLfloat;                      // Pro pohyb na ose
  sceneroty: GLfloat;                                   // �hel sm�ru pohledu
  numtriangles: integer;                                // Po�et troj�heln�k�
  loop_m: integer;                                      // Cyklus
begin
  xtrans := -xpos;
  ztrans := -zpos;
  ytrans := -walkbias-0.25;
  sceneroty := 360 - yrot;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);  // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity();	                                    // Reset matice
  glRotatef(lookupdown, 1.0,0.0,0.0);                   // Rotace na ose x - pohled nahoru/dol�
  glRotatef(sceneroty, 0.0,1.0,0.0);                    // Rotace na ose y - oto�en� doleva/doprava
  glTranslatef(xtrans, ytrans, ztrans);                 // Posun na pozici ve sc�n�
  glBindTexture(GL_TEXTURE_2D, texture[filter]);        // V�b�r textury podle filtru
  numtriangles := sector1.numtriangles;                 // Po�et troj�heln�k� - pro p�ehlednost
  for loop_m := 0 to numtriangles - 1 do                // Projde a vykresl� v�echny troj�heln�ky
  begin
    glBegin(GL_TRIANGLES);                              // Za��tek kreslen� troj�heln�k�
      glNormal3f( 0.0, 0.0, 1.0);                       // Norm�la ukazuje dop�edu - sv�tlo
      x_m := sector1.triangle[loop_m].vertex[0].x;      // Prvn� vrchol
      y_m := sector1.triangle[loop_m].vertex[0].y;
      z_m := sector1.triangle[loop_m].vertex[0].z;
      u_m := sector1.triangle[loop_m].vertex[0].u;
      v_m := sector1.triangle[loop_m].vertex[0].v;
      glTexCoord2f(u_m,v_m); glVertex3f(x_m,y_m,z_m);   // Vykreslen�

      x_m := sector1.triangle[loop_m].vertex[1].x;      // Druh� vrchol
      y_m := sector1.triangle[loop_m].vertex[1].y;
      z_m := sector1.triangle[loop_m].vertex[1].z;
      u_m := sector1.triangle[loop_m].vertex[1].u;
      v_m := sector1.triangle[loop_m].vertex[1].v;
      glTexCoord2f(u_m,v_m); glVertex3f(x_m,y_m,z_m);   // Vykreslen�

      x_m := sector1.triangle[loop_m].vertex[2].x;      // T�et� vrchol
      y_m := sector1.triangle[loop_m].vertex[2].y;
      z_m := sector1.triangle[loop_m].vertex[2].z;
      u_m := sector1.triangle[loop_m].vertex[2].u;
      v_m := sector1.triangle[loop_m].vertex[2].v;
      glTexCoord2f(u_m,v_m); glVertex3f(x_m,y_m,z_m);   // Vykreslen�
    glEnd();                                            // Konec kreslen� troj�heln�k�
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
  decimalseparator := ',';                              // Nastav� zp�t odd�lova� desetinn�ch m�st podle na�ich zvyklost�
  sector1.triangle := nil;                              // Uvoln�n� pole troj�heln�k�
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
          if (keys[ord('B')] and not bp) then
            begin
            bp := TRUE;
            blend := not(blend);
            if (blend) then
              begin
              glEnable(GL_BLEND);			                // Kl�vesa B - zapne/vypne blending
              glBlendFunc(GL_SRC_ALPHA,GL_ONE);
              glDisable(GL_DEPTH_TEST);
              end
              else
              begin
              glDisable(GL_BLEND);
              glEnable(GL_DEPTH_TEST);
              end;
            end;
          if (not keys[ord('B')]) then bp := False;
          if (keys[ord('F')] and not fp) then         // Kl�vesa F - cyklov�n� mezi texturov�mi filtry
            begin
            fp := True;
            Filter := Filter + 1;
            if (Filter > 2) then Filter := 0;
            end;
          if (not keys[ord('F')]) then fp:=FALSE;
          if (keys[VK_UP]) then                                     // �ipka nahoru - pohyb dop�edu
            begin
            xpos := xpos - sin(heading*piover180) * 0.05;           // Pohyb na ose x
            zpos := zpos - cos(heading*piover180) * 0.05;           // Pohyb na ose z
            if (walkbiasangle >= 359.0) then walkbiasangle := 0.0
              else walkbiasangle := walkbiasangle + 10;
            walkbias := sin(walkbiasangle * piover180)/20.0;        // Simulace krok�
            end;
          if (keys[VK_DOWN]) then                                   // �ipka dol� - pohyb dozadu
            begin
            xpos := xpos + sin(heading*piover180) * 0.05;           // Pohyb na ose x
            zpos := zpos + cos(heading*piover180) * 0.05;           // Pohyb na ose z
            if (walkbiasangle <= 1.0) then walkbiasangle := 359.0
              else walkbiasangle := walkbiasangle - 10;
            walkbias := sin(walkbiasangle * piover180)/20.0;        // Simulace krok�
            end;
          if (keys[VK_RIGHT]) then                                  // �ipka doprava
            begin
            heading := heading - 1.0;                               // Nato�en� sc�ny
            yrot := heading;
            end;
          if (keys[VK_LEFT]) then                                   // �ipka doleva
            begin
            heading := heading + 1.0;                               // Nato�en� sc�ny
            yrot := heading;
            end;
          if (keys[VK_PRIOR]) then lookupdown := lookupdown - 1.0;  // Page Up
          if (keys[VK_NEXT]) then lookupdown := lookupdown + 1.0;   // Page Down
        end;
    end;                                              // Konec smy�ky while
  killGLwindow();                                     // Zav�e okno
  result:=msg.wParam;                                 // Ukon�en� programu
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );   // Start programu
end.

