unit Font;

interface

uses OpenGL;

procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external 'opengl32';
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); stdcall; external 'opengl32';

type
  glFont = class
    public
      constructor Create;
      destructor Destroy; override;
      function GetListBase: GLuint;
      function GetTexture: GLuint;
      procedure SetWindowSize(width, height: GLint);
      procedure glPrintf(x, y: GLint; sada: GLint; text: string);
      procedure BuildFont(Scale: GLfloat = 1.0);
      procedure SetFontTexture(tex: GLuint);
    protected
      m_WindowWidth: GLdouble;
	    m_WindowHeight: GLdouble;
	    m_ListBase: GLuint;
	    m_FontTexture: GLuint;
    end;

implementation

{ glFont }

procedure glFont.BuildFont(Scale: GLfloat);                                     // Vytvo�� font
var
  cx, cy: GLfloat;
  loop: integer;
begin
  m_ListBase := glGenLists(256);                                                // Generov�n� display list�
  if m_FontTexture <> 0 then
    begin
    glBindTexture(GL_TEXTURE_2D,m_FontTexture);                                 // Vybere texturu
    for loop := 0 to 255 do                                                     // 256 znak�
      begin
      cx := (loop mod 16) / 16.0;                                               // P��slu�n� ��st textury
      cy := (loop div 16) / 16.0;
      glNewList(m_ListBase + loop,GL_COMPILE);                                  // Za��tek tvorby display listu
        glBegin(GL_QUADS);
					glTexCoord2f(cx,1 - cy - 0.0625);
					glVertex2f(0,0);
					glTexCoord2f(cx + 0.0625,1 - cy - 0.0625);
					glVertex2f(16 * Scale,0);
					glTexCoord2f(cx + 0.0625,1 - cy);
					glVertex2f(16 * Scale,16 * Scale);
					glTexCoord2f(cx,1 - cy);
					glVertex2f(0,16 * Scale);
				glEnd;                                                                  
				glTranslated(10 * Scale,0,0);
      glEndList;                                                                // Konec display listu
      end;
    end;
end;

constructor glFont.Create;                                                      // Konstruktor
begin
  inherited;
  m_FontTexture := 0;
  m_ListBase := 0;
end;

destructor glFont.Destroy;                                                      // Destruktor
begin
  if m_FontTexture <> 0 then
    glDeleteTextures(1,@m_FontTexture);
  if m_ListBase <> 0 then
    glDeleteLists(m_ListBase,256);
  inherited;
end;

function glFont.GetListBase: GLuint;                                            // Vrac� aktu�ln� sadu znak�
begin
  Result := m_ListBase;
end;

function glFont.GetTexture: GLuint;                                             // Vrac� aktu�ln� texturu fontu
begin
  Result := m_FontTexture;
end;

procedure glFont.glPrintf(x, y, sada: GLint; text: string);                     // V�pis textu
begin
  if text = '' then exit;                                                       // Byl p�ed�n text?
  if sada > 1 then sada := 1;
  glEnable(GL_TEXTURE_2D);                                                      // Povol� textury
  glEnable(GL_BLEND);                                                           // Povol� blending
  glBlendFunc(GL_SRC_COLOR,GL_ONE_MINUS_SRC_COLOR);                             // Typ blendingu
  glBindTexture(GL_TEXTURE_2D,m_FontTexture);                                   // V�b�r textury
  glDisable(GL_DEPTH_TEST);                                                     // Vypne hloubkov� testov�n�
  glMatrixMode(GL_PROJECTION);                                                  // Vybere projek�n� matici
  glPushMatrix;                                                                 // Ulo�� projek�n� matici
  glLoadIdentity;                                                               // Reset matice
  glOrtho(0,m_WindowWidth,0,m_WindowHeight,-1,1);                               // Nastaven� kolm� projekce
  glMatrixMode(GL_MODELVIEW);                                                   // V�b�r matice
  glPushMatrix;                                                                 // Ulo�en� matice
  glLoadIdentity;                                                               // Reset matice
  glTranslated(x,y,0);                                                          // Pozice textu (0,0 - lev� doln�)
  glListBase(m_ListBase-32+(128*sada));                                         // Zvol� znakovou sadu (0 nebo 1)
  glCallLists(length(text),GL_BYTE,Pchar(text));                                // Vykreslen� textu na obrazovku
  glMatrixMode(GL_PROJECTION);                                                  // V�b�r projek�n� matice
  glPopMatrix;                                                                  // Obnoven� ulo�en� projek�n� matice
  glMatrixMode(GL_MODELVIEW);                                                   // V�b�r matice modelview
  glPopMatrix;                                                                  // Obnoven� ulo�en� modelview matice
  glEnable(GL_DEPTH_TEST);                                                      // Zapne hloubkov� testov�n�
  glDisable(GL_BLEND);                                                          // Vypne blending
  glDisable(GL_TEXTURE_2D);                                                     // Vypne textury
end;

procedure glFont.SetFontTexture(tex: GLuint);                                   // Nastav� texturu fontu
begin
  if tex <> 0 then
    m_FontTexture := tex;
end;

procedure glFont.SetWindowSize(width, height: GLint);                           // Velikost okna
begin
  m_WindowWidth := width;
  m_WindowHeight := height;
end;

end.
