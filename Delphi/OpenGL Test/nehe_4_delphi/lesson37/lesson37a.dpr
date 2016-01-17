program lesson37a;

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

type
  MATRIX = record                                           // Ukl�d� OpenGL matici
    Data: array [0..15] of GLfloat;                         // Matice ve form�tu OpenGL
    end;

  VECTOR = record                                           // Struktura vektoru
    X, Y, Z: GLfloat;                                       // Slo�ky vektoru
    end;

  VERTEX = record                                           // Struktura vertexu
    Nor: VECTOR;                                            // Norm�la vertexu
    Pos: VECTOR;                                            // Pozice vertexu
    end;

  POLYGON = record                                          // Struktura polygonu
    Verts: array [0..2] of VERTEX;                          // Pole t�� vertex�
    end;

var
  g_window: PGL_Window;                                     // Okno
  g_keys: PKeys;                                            // Kl�vesy
  outlineDraw: boolean = true;                              // Flag pro vykreslov�n� obrysu
  outlineSmooth: boolean = false;                           // Flag pro vyhlazov�n� �ar
  outlineColor: array [0..2] of GLfloat = (0.0,0.0,0.0);    // Barva �ar
  outlineWidth: GLfloat = 3.0;                              // Tlou��ka �ar
  lightAngle: VECTOR;                                       // Sm�r sv�tla
  lightRotate: boolean = false;                             // Flag oznamuj�c� zda rotujeme sv�tlem
  modelAngle: GLfloat = 0.0;                                // �hel nato�en� objektu na ose y
  modelRotate: boolean = false;                             // Flag na ot��en� modelem
  polyData: array of POLYGON;                               // Data polygon�
  polyNum: integer = 0;                                     // Po�et polygon�
  shaderTexture: GLuint;                                    // M�sto pro jednu texturu


function ReadMesh: boolean;                                                     // Na�te obsah souboru model.txt
var
  vstup: file;
  precteno: integer;
begin
  AssignFile(vstup,'Data\model.txt');
  {$I-}
  Reset(vstup,1);                                                               // Otev�e soubor
  {$I+}
  if IOResult <> 0 then                                                         // Kontrola chyby otev�en�
    begin
    Result := false;
    exit;
    end;
  BlockRead(vstup,polyNum,sizeof(integer),precteno);                            // Na�te hlavi�ku souboru (po�et vertex�)
  SetLength(polyData,polyNum);                                                  // Alokace pam�ti
  BlockRead(vstup,polyData[0],polyNum * sizeof(POLYGON),precteno);              // Na�te v�echna data
  CloseFile(vstup);                                                             // Zav�e soubor
  Result := true;                                                               // Loading objektu �sp�n�
end;

function DotProduct(V1, V2: VECTOR): GLfloat;                                   // Spo��t� odchylku dvou vektor�
begin
  Result := V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z;                            // Vr�t� �hel
end;

function Magnitude(V: VECTOR): GLfloat;                                         // Spo��t� d�lku vektoru
begin
  Result := sqrt(sqr(V.X) + sqr(V.Y) + sqr(V.Z));                               // Vr�t� d�lku vektoru
end;

procedure Normalize(var V: VECTOR);                                             // Vytvo�� jednotkov� vektor
var
  M: GLfloat;                                                                   // D�lka vektoru
begin
  M := Magnitude(V);                                                            // Spo��t� aktu�ln� d�lku vektoru
  if M <> 0 then                                                                // Proti d�len� nulou
    begin
    V.X := V.X / M;                                                             // Normalizov�n� jednotliv�ch slo�ek
    V.Y := V.Y / M;
    V.Z := V.Z / M;
    end;
end;

procedure RotateVector(M: MATRIX; V: VECTOR; var D: VECTOR);                    // Rotace vektoru podle zadan� matice
begin
  D.X := (M.Data[0] * V.X) + (M.Data[4] * V.Y) + (M.Data[8] * V.Z);             // Oto�en� na x
  D.Y := (M.Data[1] * V.X) + (M.Data[5] * V.Y) + (M.Data[9] * V.Z);             // Oto�en� na y
  D.Z := (M.Data[2] * V.X) + (M.Data[6] * V.Y) + (M.Data[10] * V.Z);            // Oto�en� na z
end;

function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
var
  i: integer;                                                                   // ��d�c� prom�nn� cykl�
  line: string;                                                                 // Pole znak�
  shaderData: array [0..31,0..2] of GLfloat;                                    // Pole 96 shader hodnot
  vstup: Textfile;                                                              // Ukazatel na soubor
begin
  g_window := window;
  g_keys := key;
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);                             // Perspektivn� korekce
  glClearColor(0.7,0.7,0.7,0.0);                                                // Sv�tle �ed� pozad�
  glClearDepth(1.0);                                                            // Nastaven� hloubkov�ho bufferu
  glEnable(GL_DEPTH_TEST);			                                                // Povol� hloubkov� testov�n�
  glDepthFunc(GL_LESS);                                                         // Typ testov�n� hloubky
  glShadeModel(GL_SMOOTH);                                                      // Jemn� st�nov�n�
  glDisable(GL_LINE_SMOOTH);                                                    // Vypne vyhlazov�n� �ar
  glEnable(GL_CULL_FACE);                                                       // Zapne face culling (o�ez�v�n� st�n)
  glDisable(GL_LIGHTING);                                                       // Vypne sv�tla
  AssignFile(vstup,'Data\shader.txt');
  {$I-}
  Reset(vstup);                                                                 // Otev�en� shader souboru
  {$I+}
  if IOResult = 0 then                                                          // Kontrola, zda je soubor otev�en
    begin
    for i := 0 to 31 do                                                         // Projde v�ech 32 hodnot ve stupn�ch �edi
      begin
      if Eof(vstup) then break;                                                 // Kontrola konce souboru
      Readln(vstup,line);                                                       // Z�sk�n� aktu�ln�ho ��dku
      shaderData[i,0] := StrToFloat(line);                                      // Zkop�ruje danou hodnotu do v�ech slo�ek barvy
      shaderData[i,1] := shaderData[i,0];
      shaderData[i,2] := shaderData[i,0];
      end;
    CloseFile(vstup);                                                           // Zav�e soubor
    end
    else
    begin
    Result := false;                                                            // Ne�sp�ch
    exit;
    end;
  glGenTextures(1,shaderTexture);                                               // Z�sk�n� ID textury
  glBindTexture(GL_TEXTURE_1D,shaderTexture);                                   // P�i�azen� textury; od te� je 1D texturou
  // Nikdy nepou��vejte bi-/trilinearn� filtrov�n�!
  glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
  glTexImage1D(GL_TEXTURE_1D,0,GL_RGB,32,0,GL_RGB,GL_FLOAT,@shaderData);        // Upload dat
  lightAngle.X := 0.0;                                                          // Nastaven� sm�ru x
  lightAngle.Y := 0.0;                                                          // Nastaven� sm�ru y
  lightAngle.Z := 1.0;                                                          // Nastaven� sm�ru z
  Normalize(lightAngle);                                                        // Normalizov�n� vektoru sv�tla
  Result := ReadMesh;                                                           // Vr�t� n�vratovou hodnotu funkce ReadMesh()
end;

procedure Deinitialize;                                                         // Deinicializace
begin
  glDeleteTextures(1,@shaderTexture);                                           // Sma�e shader texturu
  SetLength(polyData,0);                                                        // Uvoln� data polygon�
end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohyb� ve sc�n� a stisk kl�ves
begin
  if g_keys.keyDown[VK_ESCAPE] then                                   // Kl�vesa ESC?
    TerminateApplication(g_window^);                                  // Ukon�en� programu
  if g_keys.keyDown[VK_F1] then                                       // Kl�vesa F1?
    ToggleFullscreen(g_window^);                                      // P�epnut� fullscreen/okno
  if g_keys.keyDown[Ord(' ')] then                                    // Mezern�k
    begin
    modelRotate := not modelRotate;                                   // Zapne/vypne rotaci objektu
    g_keys.keyDown[Ord(' ')] := FALSE;
    end;
  if g_keys.keyDown [Ord('1')] then                                   // Kl�vesa ��sla 1
    begin
    outlineDraw := not outlineDraw;                                   // Zapne/vypne vykreslov�n� obrysu
    g_keys.keyDown[Ord('1')] := FALSE;
    end;
  if g_keys.keyDown[Ord('2')] then                                    // Kl�vesa ��slo 2
    begin
    outlineSmooth := not outlineSmooth;                               // Zapne/vypne anti-aliasing
    g_keys.keyDown[Ord('2')] := FALSE;
    end;
  if g_keys.keyDown[VK_UP] then                                       // �ipka nahoru
    begin
    outlineWidth := outlineWidth + 1.0;                               // Zv�t�� tlou��ku ��ry
    g_keys.keyDown[VK_UP] := FALSE;
    end;
  if g_keys.keyDown[VK_DOWN] then                                     // �ipka dol�
    begin
    outlineWidth := outlineWidth - 1.0;                               // Zmen�� tlou��ku ��ry
    g_keys.keyDown[VK_DOWN] := FALSE;
    end;
  if modelRotate then                                                 // Je rotace zapnut�
    modelAngle := modelAngle + milliseconds / 10.0;                   // Aktualizace �hlu nato�en� v z�vislosti na FPS
end;

procedure Draw;                                                       // Vykreslen� sc�ny
var
  i, j: integer;                                                      // ��d�c� prom�nn� cykl�
  TmpShade: GLfloat;                                                  // Do�asn� hodnota st�nu
  TmpMatrix: MATRIX;                                                  // Do�asn� MATRIX struktura
  TmpVector, TmpNormal: VECTOR;                                       // Do�asn� VECTOR struktury
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                // Sma�e obrazovku a hloubkov� buffer
  glLoadIdentity;	                                                    // Reset matice
  if outlineSmooth then                                               // Chce u�ivatel vyhlazen� ��ry?
    begin
    glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);                            // Pou�ije nejkvalitn�j�� v�po�ty
    glEnable(GL_LINE_SMOOTH);                                         // Zapne anti-aliasing
    end
    else
    glDisable(GL_LINE_SMOOTH);                                        // Vypne anti-aliasing
  glTranslatef(0.0,0.0,-2.0);                                         // Posun do hloubky
  glRotatef(modelAngle,0.0,1.0,0.0);                                  // Rotace objektem na ose y
  glGetFloatv(GL_MODELVIEW_MATRIX,@TmpMatrix.Data);                   // Z�sk�n� matice
  // K�d Cel-Shadingu
  glEnable(GL_TEXTURE_1D);                                            // Zapne 1D texturov�n�
  glBindTexture(GL_TEXTURE_1D,shaderTexture);                         // Zvol� texturu
  glColor3f(1.0,1.0,1.0);                                             // Nastaven� barvy modelu (b�l�)
  glBegin(GL_TRIANGLES);                                              // Za��tek kreslen� troj�heln�k�
    for i := 0 to polyNum - 1 do                                      // Proch�z� jednotliv� polygony
      for j := 0 to 2 do                                              // Proch�z� jednotliv� vertexy
        begin
        // Zkop�rov�n� aktu�ln� norm�ly do do�asn� struktury
        TmpNormal.X := polyData[i].Verts[j].Nor.X;
        TmpNormal.Y := polyData[i].Verts[j].Nor.Y;
        TmpNormal.Z := polyData[i].Verts[j].Nor.Z;
        RotateVector(TmpMatrix,TmpNormal,TmpVector);                  // Oto�� vektor podle matice
        Normalize(TmpVector);                                         // Normalizace norm�ly
        TmpShade := DotProduct(TmpVector,lightAngle);                 // Spo��t�n� hodnoty st�nu
        if TmpShade < 0.0 then TmpShade := 0.0;                       // Pokud je TmpShade men�� ne� nula bude se rovnat nule
        glTexCoord1f(TmpShade);                                       // Nastaven� texturovac� sou�adnice na hodnotu st�nu
        glVertex3fv(@polyData[i].Verts[j].Pos.X);                     // Po�le pozici vertexu
        end;
  glEnd;                                                              // Konec kreslen�
  glDisable(GL_TEXTURE_1D);                                           // Vypne 1D texturov�n�
  // K�d pro vykreslen� obrys�
  if outlineDraw then                                                 // Chceme v�bec kreslit obrys?
    begin
    glEnable(GL_BLEND);                                               // Zapne blending
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);                 // M�d blendingu
    glPolygonMode(GL_BACK,GL_LINE);                                   // Odvr�cen� polygony se stanout pouze obrysov�mi �arami
    glLineWidth(outlineWidth);                                        // Nastaven� ���ky ��ry
    glCullFace(GL_FRONT);                                             // Nerenderovat p�ivr�cen� polygony
    glDepthFunc(GL_LEQUAL);                                           // M�d testov�n� hloubky
    glColor3fv(@outlineColor[0]);                                     // Barva obrysu (�ern�)
    glBegin(GL_TRIANGLES);                                            // Za��tek kreslen� troj�heln�k�
      for i := 0 to polyNum - 1 do                                    // Proch�z� jednotliv� polygony
        for j := 0 to 2 do                                            // Proch�z� jednotliv� vertexy
          glVertex3fv(@polyData[i].Verts[j].Pos.X);                   // Po�le pozici vertexu
    glEnd;                                                            // Konec kreslen�
    glDepthFunc(GL_LESS);                                             // Testov�n� hloubky na p�vodn� nastaven�
    glCullFace(GL_BACK);                                              // Nastaven� o�ez�v�n� na p�vodn� hodnotu
    glPolygonMode(GL_BACK,GL_FILL);                                   // Norm�ln� vykreslov�n�
    glDisable(GL_BLEND);                                              // Vypne blending
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
    init.title := 'NeHe''s Cel-Shading Tutorial';
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
  DecimalSeparator := '.';
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );                  // Start programu
  DecimalSeparator := ',';
end.

