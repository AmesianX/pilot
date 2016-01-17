unit Camera;

interface

uses Vector, Point, OpenGL;

procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external 'opengl32';
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); stdcall; external 'opengl32';

type
  glCamera = class                                                              // T��da kamery
    public
      vLightSourceToCamera, vLightSourceToIntersect: glVector;                  // Vektory sv�tlo - kamera a sv�tlo - pr�se��k
	    ptIntersect, pt: glPoint;                                                 // Bod pr�se��ku
	    m_WindowHeight: GLsizei;                                                  // Velikost okna
	    m_WindowWidth: GLsizei;
	    m_StreakTexture: GLuint;                                                  // ID textur
	    m_HaloTexture: GLuint;
	    m_GlowTexture: GLuint;
	    m_BigGlowTexture: GLuint;
	    m_MaxPointSize: GLfloat;
	    m_Frustum: array [0..5,0..3] of GLfloat;                                  // O�ezov� roviny
	    m_LightSourcePos: glPoint;                                                // Pozice sv�tla
	    m_MaxPitchRate: GLfloat;
	    m_MaxHeadingRate: GLfloat;
	    m_HeadingDegrees: GLfloat;
	    m_PitchDegrees: GLfloat;
	    m_MaxForwardVelocity: GLfloat;
	    m_ForwardVelocity: GLfloat;
	    m_Position: glPoint;
	    m_DirectionVector: glVector;
      constructor Create;
      destructor Destroy; override;
      function SphereInFrustum(p: glPoint; Radius: GLfloat): boolean; overload;
	    function SphereInFrustum(x, y, z, Radius: GLfloat): boolean; overload;
	    function PointInFrustum(x, y, z: GLfloat): boolean; overload;
	    function PointInFrustum(p: glPoint): boolean; overload;
      procedure RenderLensFlare;
	    procedure RenderStreaks(r, g, b, a: GLfloat; p: glPoint; scale: GLfloat);
	    procedure RenderBigGlow(r, g, b, a: GLfloat; p: glPoint; scale: GLfloat);
	    procedure RenderGlow(r, g, b, a: GLfloat; p: glPoint; scale: GLfloat);
	    procedure RenderHalo(r, g, b, a: GLfloat; p: glPoint; scale: GLfloat);
      procedure UpdateFrustumFaster;
	    procedure UpdateFrustum;
      procedure ChangeVelocity(vel: GLfloat);
	    procedure ChangeHeading(degrees: GLfloat);
	    procedure ChangePitch(degrees: GLfloat);
	    procedure SetPrespective;
      function IsOccluded(p :glPoint): boolean;
    end;

implementation

{ glCamera }

constructor glCamera.Create;                                                    // konstruktor - inicializace t��dy
begin
  m_MaxPitchRate := 0.0;
	m_MaxHeadingRate := 0.0;
	m_HeadingDegrees := 0.0;
	m_PitchDegrees := 0.0;
	m_MaxForwardVelocity := 0.0;
	m_ForwardVelocity	:= 0.0;
  m_LightSourcePos := glPoint.Create;
	m_GlowTexture := 0;
	m_HaloTexture	:= 0;
	m_StreakTexture	:= 0;
  m_BigGlowTexture := 0;
	m_MaxPointSize := 0.0;
  m_WindowHeight := 0;
  m_WindowWidth := 0;
  m_Position := glPoint.Create;
  m_DirectionVector := glVector.Create;
  vLightSourceToCamera := glVector.Create;
  vLightSourceToIntersect := glVector.Create;
  ptIntersect := glPoint.Create;
  pt := glPoint.Create;
end;

destructor glCamera.Destroy;                                                    // Destruktor - uvoln�n� prost�edk�
begin
  if m_GlowTexture <> 0 then
    glDeleteTextures(1,@m_GlowTexture);
  if m_HaloTexture <> 0 then
    glDeleteTextures(1,@m_HaloTexture);
  if m_BigGlowTexture <> 0 then
    glDeleteTextures(1,@m_BigGlowTexture);
  if m_StreakTexture <> 0 then
    glDeleteTextures(1,@m_StreakTexture);
  inherited;
end;

procedure glCamera.ChangeHeading(degrees: GLfloat);
begin
  if Abs(degrees) < Abs(m_MaxHeadingRate) then
    if ((m_PitchDegrees > 90) and (m_PitchDegrees < 270)) or
        ((m_PitchDegrees < -90) and (m_PitchDegrees > -270)) then
			m_HeadingDegrees := m_HeadingDegrees - degrees
		  else
			m_HeadingDegrees := m_HeadingDegrees + degrees
    else
    if degrees < 0 then
      if ((m_PitchDegrees > 90) and (m_PitchDegrees < 270)) or
          ((m_PitchDegrees < -90) and (m_PitchDegrees > -270)) then
				m_HeadingDegrees := m_HeadingDegrees + m_MaxHeadingRate
			  else
				m_HeadingDegrees := m_HeadingDegrees - m_MaxHeadingRate
      else
      if ((m_PitchDegrees > 90) and (m_PitchDegrees < 270)) or
          ((m_PitchDegrees < -90) and (m_PitchDegrees > -270)) then
				m_HeadingDegrees := m_HeadingDegrees - m_MaxHeadingRate
			  else
				m_HeadingDegrees := m_HeadingDegrees + m_MaxHeadingRate;
  if m_HeadingDegrees > 360.0 then
    m_HeadingDegrees := m_HeadingDegrees - 360.0;
  if m_HeadingDegrees < -360 then
    m_HeadingDegrees := m_HeadingDegrees + 360.0;
end;

procedure glCamera.ChangePitch(degrees: GLfloat);
begin
  if Abs(degrees) < Abs(m_MaxPitchRate) then
    m_PitchDegrees := m_PitchDegrees + degrees
    else
    if degrees < 0 then
      m_PitchDegrees := m_PitchDegrees - m_MaxPitchRate
      else
      m_PitchDegrees := m_PitchDegrees + m_MaxPitchRate;
  if m_PitchDegrees > 360.0 then
    m_PitchDegrees := m_PitchDegrees - 360.0;
  if m_PitchDegrees < 360.0 then
    m_PitchDegrees := m_PitchDegrees + 360.0;
end;

procedure glCamera.ChangeVelocity(vel: GLfloat);
begin
  if Abs(vel) < Abs(m_MaxForwardVelocity) then
    m_ForwardVelocity := m_ForwardVelocity + vel
    else
    if vel < 0 then
      m_ForwardVelocity := m_ForwardVelocity - (-m_MaxForwardVelocity)
      else
      m_ForwardVelocity := m_ForwardVelocity + m_MaxForwardVelocity;
end;

function glCamera.IsOccluded(p: glPoint): boolean;                              // Je p�ed bodem n�co vykresleno?
var
  viewport: array [0..3] of GLint;                                              // Data viewportu
  mvmatrix, projmatrix: array [0..15] of GLdouble;                              // Transforma�n� matice
  winx, winy, winz: GLdouble;                                                   // V�sledn� sou�adnice
  flareZ: GLdouble;                                                             // Hloubka z��e v obrazovce
  bufferZ: GLfloat;                                                             // Hloubka z bufferu
begin
  glGetIntegerv(GL_VIEWPORT,@viewport);                                         // Z�sk�n� viewportu
  glGetDoublev(GL_MODELVIEW_MATRIX,@mvmatrix);                                  // Z�sk�n� modelview matice
  glGetDoublev(GL_PROJECTION_MATRIX,@projmatrix);                               // Z�sk�n� projek�n� matice
  // Kam do viewportu (2D) se vykresl� bod (3D)
  gluProject(p.x,p.y,p.z,@mvmatrix,@projmatrix,@viewport,winx,winy,winz);
  flareZ := winz;
  glReadPixels(Trunc(winx),Trunc(winy),1,1,GL_DEPTH_COMPONENT,GL_FLOAT,@bufferZ); // Hloubka v depth bufferu
  if bufferZ < flareZ then                                                      // P�ed bodem se nach�z� objekt
    Result := true
    else                                                                        // Nic p�ed bodem nen�
    Result := false;
end;

function glCamera.PointInFrustum(x, y, z: GLfloat): boolean;                    // Bude bod vid�t na sc�n�?
var
  i: integer;
begin
  for i := 0 to 5 do                                                            // Bod se mus� nach�zet mezi v�emi �esti o�ez�vac�mi rovinami
    if (m_Frustum[i,0] * x + m_Frustum[i,1] * y + m_Frustum[i,2] * z + m_Frustum[i,3]) <= 0 then
      begin
      Result := false;
      exit;
      end;
  Result := true;
end;

function glCamera.PointInFrustum(p: glPoint): boolean;                          // Bude bod vid�t na sc�n�?
var
  i: integer;
begin
  for i := 0 to 5 do                                                            // Bod se mus� nach�zet mezi v�emi �esti o�ez�vac�mi rovinami
    if (m_Frustum[i,0] * p.x + m_Frustum[i,1] * p.y + m_Frustum[i,2] * p.z + m_Frustum[i,3]) <= 0 then
      begin
      Result := false;
      exit;
      end;
  Result := true;
end;

procedure glCamera.RenderBigGlow(r, g, b, a: GLfloat; p: glPoint;               // Vykreslen� z��e
  scale: GLfloat);
var
  q: array [0..3] of glPoint;                                                   // Pomocn� bod
  i: integer;
begin
  for i := 0 to 3 do q[i] := glPoint.Create;
  q[0].x := p.x - scale;                                                        // V�po�et pozice
  q[0].y := p.y - scale;
  q[1].x := p.x - scale;
  q[1].y := p.y + scale;
  q[2].x := p.x + scale;
  q[2].y := p.y - scale;
  q[3].x := p.x + scale;
  q[3].y := p.y + scale;
  glPushMatrix;                                                                 // Ulo�en� matice
  glTranslatef(p.x,p.y,p.z);                                                    // P�esun na pozici
  glRotatef(-m_HeadingDegrees,0.0,1.0,0.0);                                     // Odstran�n� rotac�
  glRotatef(-m_PitchDegrees,1.0,0.0,0.0);
  glBindTexture(GL_TEXTURE_2D,m_BigGlowTexture);                                // Textura
  glColor4f(r,g,b,a);                                                           // Nastaven� barvy
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0.0,0.0); glVertex2f(q[0].x,q[0].y);
    glTexCoord2f(0.0,1.0); glVertex2f(q[1].x,q[1].y);
    glTexCoord2f(1.0,0.0); glVertex2f(q[2].x,q[2].y);
    glTexCoord2f(1.0,1.0); glVertex2f(q[3].x,q[3].y);
  glEnd;
  glPopMatrix;                                                                  // Obnoven� matice
  for i := 0 to 3 do q[i].Free;
end;

procedure glCamera.RenderGlow(r, g, b, a: GLfloat; p: glPoint;                  // Vykreslen� z��e
  scale: GLfloat);
var
  q: array [0..3] of glPoint;                                                   // Pomocn� bod
  i: integer;
begin
  for i := 0 to 3 do q[i] := glPoint.Create;
  q[0].x := p.x - scale;                                                        // V�po�et pozice
  q[0].y := p.y - scale;
  q[1].x := p.x - scale;
  q[1].y := p.y + scale;
  q[2].x := p.x + scale;
  q[2].y := p.y - scale;
  q[3].x := p.x + scale;
  q[3].y := p.y + scale;
  glPushMatrix;                                                                 // Ulo�en� matice
  glTranslatef(p.x,p.y,p.z);                                                    // P�esun na pozici
  glRotatef(-m_HeadingDegrees,0.0,1.0,0.0);                                     // Odstran�n� rotac�
  glRotatef(-m_PitchDegrees,1.0,0.0,0.0);
  glBindTexture(GL_TEXTURE_2D,m_GlowTexture);                                   // Textura
  glColor4f(r,g,b,a);                                                           // Nastaven� barvy
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0.0,0.0); glVertex2f(q[0].x,q[0].y);
    glTexCoord2f(0.0,1.0); glVertex2f(q[1].x,q[1].y);
    glTexCoord2f(1.0,0.0); glVertex2f(q[2].x,q[2].y);
    glTexCoord2f(1.0,1.0); glVertex2f(q[3].x,q[3].y);
  glEnd;
  glPopMatrix;                                                                  // Obnoven� matice
  for i := 0 to 3 do q[i].Free;
end;

procedure glCamera.RenderHalo(r, g, b, a: GLfloat; p: glPoint;                  // Vykreslen� z��e
  scale: GLfloat);
var
  q: array [0..3] of glPoint;                                                   // Pomocn� bod
  i: integer;
begin
  for i := 0 to 3 do q[i] := glPoint.Create;
  q[0].x := p.x - scale;                                                        // V�po�et pozice
  q[0].y := p.y - scale;
  q[1].x := p.x - scale;
  q[1].y := p.y + scale;
  q[2].x := p.x + scale;
  q[2].y := p.y - scale;
  q[3].x := p.x + scale;
  q[3].y := p.y + scale;
  glPushMatrix;                                                                 // Ulo�en� matice
  glTranslatef(p.x,p.y,p.z);                                                    // P�esun na pozici
  glRotatef(-m_HeadingDegrees,0.0,1.0,0.0);                                     // Odstran�n� rotac�
  glRotatef(-m_PitchDegrees,1.0,0.0,0.0);
  glBindTexture(GL_TEXTURE_2D,m_HaloTexture);                                   // Textura
  glColor4f(r,g,b,a);                                                           // Nastaven� barvy
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0.0,0.0); glVertex2f(q[0].x,q[0].y);
    glTexCoord2f(0.0,1.0); glVertex2f(q[1].x,q[1].y);
    glTexCoord2f(1.0,0.0); glVertex2f(q[2].x,q[2].y);
    glTexCoord2f(1.0,1.0); glVertex2f(q[3].x,q[3].y);
  glEnd;
  glPopMatrix;                                                                  // Obnoven� matice
  for i := 0 to 3 do q[i].Free;
end;

procedure glCamera.RenderLensFlare;                                             // Vykreslen� �o�kov�ch objekt�
var
  Length: GLfloat;
begin
  Length := 0.0;
  if SphereInFrustum(m_LightSourcePos,1.0) then                                 // Pouze pokud kamera sm��uje ke sv�tlu
    begin
    vLightSourceToCamera.Assign(m_Position.Subtract(m_LightSourcePos));              // Vektor od kamery ke sv�tlu
    Length := vLightSourceToCamera.Magnitude;                                   // Vzd�lenost kamery od sv�tla
    ptIntersect.Assign(m_DirectionVector.Multiply(Length));                     // Bod pr�se��ku
    ptIntersect.AddSelf(m_Position);
    vLightSourceToIntersect.Assign(ptIntersect.Subtract(m_LightSourcePos));          // Vektor mezi sv�tlem a pr�se��kem
    Length := vLightSourceToIntersect.Magnitude;                                // Vzd�lenost sv�tla a pr�se��ku
    vLightSourceToIntersect.Normalize;                                          // Normalizace vektoru
    // Nastaven� OpenGL
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);
    if not IsOccluded(m_LightSourcePos) then                                    // P�ed st�edem z��e nesm� b�t ��dn� objekt
      begin
      // Vykreslen� z��e
      RenderBigGlow(0.60,0.60,0.8,1.0,m_LightSourcePos,16.0);
      RenderStreaks(0.60,0.60,0.8,1.0,m_LightSourcePos,16.0);
      RenderGlow(0.8,0.8,1.0,0.5,m_LightSourcePos,3.5);
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.1)));              // Bod ve 20% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderGlow(0.9,0.6,0.4,0.5,pt,0.6);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.15)));             // Bod ve 30% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderHalo(0.8,0.5,0.6,0.5,pt,1.7);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.175)));            // Bod ve 35% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderHalo(0.9,0.2,0.1,0.5,pt,0.83);                                      // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.285)));            // Bod ve 57% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderHalo(0.7,0.7,0.4,0.5,pt,1.6);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.2755)));           // Bod ve 55.1% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderGlow(0.9,0.9,0.2,0.5,pt,0.8);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.4775)));           // Bod ve 95.5% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderGlow(0.93,0.82,0.73,0.5,pt,1.0);                                    // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.49)));             // Bod ve 98% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderHalo(0.7,0.6,0.5,0.5,pt,1.4);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.65)));             // Bod ve 130% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderGlow(0.7,0.8,0.3,0.5,pt,1.8);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.63)));             // Bod ve 126% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderGlow(0.4,0.3,0.2,0.5,pt,1.4);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.8)));              // Bod ve 160% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderHalo(0.7,0.5,0.5,0.5,pt,1.4);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.7825)));           // Bod ve 156.5% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderGlow(0.8,0.5,0.1,0.5,pt,0.6);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 1.0)));              // Bod ve 200% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderHalo(0.5,0.5,0.7,0.5,pt,1.7);                                       // Vykreslen� z��e
      pt.Assign(vLightSourceToIntersect.Multiply((Length * 0.975)));            // Bod ve 195% vzd�lenosti od sv�tla ve sm�ru pr�se��ku
      pt.AddSelf(m_LightSourcePos);
      RenderGlow(0.4,0.1,0.9,0.5,pt,2.0);                                       // Vykreslen� z��e
      end;
    // Obnoven� nastaven� OpenGL
    glDisable(GL_BLEND);
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_TEXTURE_2D);
    end;
end;

procedure glCamera.RenderStreaks(r, g, b, a: GLfloat; p: glPoint;               // Vykreslen� z��e
  scale: GLfloat);
var
  q: array [0..3] of glPoint;                                                   // Pomocn� bod
  i: integer;
begin
  for i := 0 to 3 do q[i] := glPoint.Create;
  q[0].x := p.x - scale;                                                        // V�po�et pozice
  q[0].y := p.y - scale;
  q[1].x := p.x - scale;
  q[1].y := p.y + scale;
  q[2].x := p.x + scale;
  q[2].y := p.y - scale;
  q[3].x := p.x + scale;
  q[3].y := p.y + scale;
  glPushMatrix;                                                                 // Ulo�en� matice
  glTranslatef(p.x,p.y,p.z);                                                    // P�esun na pozici
  glRotatef(-m_HeadingDegrees,0.0,1.0,0.0);                                     // Odstran�n� rotac�
  glRotatef(-m_PitchDegrees,1.0,0.0,0.0);
  glBindTexture(GL_TEXTURE_2D,m_StreakTexture);                                 // Textura
  glColor4f(r,g,b,a);                                                           // Nastaven� barvy
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0.0,0.0); glVertex2f(q[0].x,q[0].y);
    glTexCoord2f(0.0,1.0); glVertex2f(q[1].x,q[1].y);
    glTexCoord2f(1.0,0.0); glVertex2f(q[2].x,q[2].y);
    glTexCoord2f(1.0,1.0); glVertex2f(q[3].x,q[3].y);
  glEnd;
  glPopMatrix;                                                                  // Obnoven� matice
  for i := 0 to 3 do q[i].Free;
end;

procedure glCamera.SetPrespective;
var
  Matrix: array [0..15] of GLfloat;                                             // Pole pro modelview matici
  v: glVector;                                                                  // Sm�r a rychlost kamery
begin
  v := glVector.Create;
  glRotatef(m_HeadingDegrees,0.0,1.0,0.0);                                      // V�po�et sm�rov�ho vektoru
  glRotatef(m_PitchDegrees,1.0,0.0,0.0);
  glGetFloatv(GL_MODELVIEW_MATRIX,@Matrix);                                     // Z�sk�n� matice
  m_DirectionVector.i := Matrix[8];                                             // Sm�rov� vektor
  m_DirectionVector.j := Matrix[9];
  m_DirectionVector.k := -Matrix[10];                                           // Mus� b�t invertov�n
  glLoadIdentity;                                                               // Reset matice
  glRotatef(m_PitchDegrees,1.0,0.0,0.0);                                        // Spr�vn� orientace sc�ny
  glRotatef(m_HeadingDegrees,0.0,1.0,0.0);
  v.Assign(m_DirectionVector);                                                  // Aktualizovat sm�r podle rychlosti
  v.MultiplySelf(m_ForwardVelocity);
  m_Position.x := m_Position.x + v.i;                                           // Inkrementace pozice vektorem
  m_Position.y := m_Position.y + v.j;
  m_Position.z := m_Position.z + v.k;
  glTranslatef(-m_Position.x,-m_Position.y,-m_Position.z);                      // P�esun na novou pozici
  v.Free;
end;

function glCamera.SphereInFrustum(p: glPoint; Radius: GLfloat): boolean;        // Bude koule vid�t na sc�n�?
var
  i: integer;
begin
  for i := 0 to 5 do                                                            // Koule se mus� nach�zet mezi v�emi �esti o�ez�vac�mi rovinami
    if (m_Frustum[i,0] * p.x + m_Frustum[i,1] * p.y + m_Frustum[i,2] * p.z + m_Frustum[i,3]) <= -Radius then
      begin
      Result := false;
      exit;
      end;
  Result := true;
end;

function glCamera.SphereInFrustum(x, y, z, Radius: GLfloat): boolean;           // Bude koule vid�t na sc�n�?
var
  i: integer;
begin
  for i := 0 to 5 do                                                            // Koule se mus� nach�zet mezi v�emi �esti o�ez�vac�mi rovinami
    if (m_Frustum[i,0] * x + m_Frustum[i,1] * y + m_Frustum[i,2] * z + m_Frustum[i,3]) <= -Radius then
      begin
      Result := false;
      exit;
      end;
  Result := true;
end;

procedure glCamera.UpdateFrustum;                                               // Z�sk�n� o�ez�vac�ch rovin
var
  clip: array [0..15] of GLfloat;                                               // Pomocn� matice
  proj: array [0..15] of GLfloat;                                               // Projek�n� matice
  modl: array [0..15] of GLfloat;                                               // Modelview matice
  t: GLfloat;                                                                   // Pomocn�
begin
  glGetFloatv(GL_PROJECTION_MATRIX,@proj);                                      // Z�sk�n� projek�n� matice
  glGetFloatv(GL_MODELVIEW_MATRIX,@modl);                                       // Z�sk�n� modelview matice
  // Vyn�sob� projek�n� matici pomoc� modelview
  clip[0] := modl[0] * proj[0] + modl[1] * proj[4] + modl[2] * proj[8] + modl[3] * proj[12];
  clip[1] := modl[0] * proj[1] + modl[1] * proj[5] + modl[2] * proj[9] + modl[3] * proj[13];
  clip[2] := modl[0] * proj[2] + modl[1] * proj[6] + modl[2] * proj[10] + modl[3] * proj[14];
  clip[3] := modl[0] * proj[3] + modl[1] * proj[7] + modl[2] * proj[11] + modl[3] * proj[15];
  clip[4] := modl[4] * proj[0] + modl[5] * proj[4] + modl[6] * proj[8] + modl[7] * proj[12];
  clip[5] := modl[4] * proj[1] + modl[5] * proj[5] + modl[6] * proj[9] + modl[7] * proj[13];
  clip[6] := modl[4] * proj[2] + modl[5] * proj[6] + modl[6] * proj[10] + modl[7] * proj[14];
  clip[7] := modl[4] * proj[3] + modl[5] * proj[7] + modl[6] * proj[11] + modl[7] * proj[15];
  clip[8] := modl[8] * proj[0] + modl[9] * proj[4] + modl[10] * proj[8] + modl[11] * proj[12];
  clip[9] := modl[8] * proj[1] + modl[9] * proj[5] + modl[10] * proj[9] + modl[11] * proj[13];
  clip[10] := modl[8] * proj[2] + modl[9] * proj[6] + modl[10] * proj[10] + modl[11] * proj[14];
  clip[11] := modl[8] * proj[3] + modl[9] * proj[7] + modl[10] * proj[11] + modl[11] * proj[15];
  clip[12] := modl[12] * proj[0] + modl[13] * proj[4] + modl[14] * proj[8] + modl[15] * proj[12];
  clip[13] := modl[12] * proj[1] + modl[13] * proj[5] + modl[14] * proj[9] + modl[15] * proj[13];
  clip[14] := modl[12] * proj[2] + modl[13] * proj[6] + modl[14] * proj[10] + modl[15] * proj[14];
  clip[15] := modl[12] * proj[3] + modl[13] * proj[7] + modl[14] * proj[11] + modl[15] * proj[15];
  m_Frustum[0,0] := clip[3] - clip[0];                                          // Z�sk�n� prav� roviny
  m_Frustum[0,1] := clip[7] - clip[4];
  m_Frustum[0,2] := clip[11] - clip[8];
  m_Frustum[0,3] := clip[15] - clip[12];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[0,0]) + sqr(m_Frustum[0,1]) + sqr(m_Frustum[0,2]));
  m_Frustum[0,0] := m_Frustum[0,0] / t;
  m_Frustum[0,1] := m_Frustum[0,1] / t;
  m_Frustum[0,2] := m_Frustum[0,2] / t;
  m_Frustum[0,3] := m_Frustum[0,3] / t;
  m_Frustum[1,0] := clip[3] + clip[0];                                          // Z�sk�n� lev� roviny
  m_Frustum[1,1] := clip[7] + clip[4];
  m_Frustum[1,2] := clip[11] + clip[8];
  m_Frustum[1,3] := clip[15] + clip[12];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[1,0]) + sqr(m_Frustum[1,1]) + sqr(m_Frustum[1,2]));
  m_Frustum[1,0] := m_Frustum[1,0] / t;
  m_Frustum[1,1] := m_Frustum[1,1] / t;
  m_Frustum[1,2] := m_Frustum[1,2] / t;
  m_Frustum[1,3] := m_Frustum[1,3] / t;
  m_Frustum[2,0] := clip[3] + clip[1];                                          // Z�sk�n� doln� roviny
  m_Frustum[2,1] := clip[7] + clip[5];
  m_Frustum[2,2] := clip[11] + clip[9];
  m_Frustum[2,3] := clip[15] + clip[13];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[2,0]) + sqr(m_Frustum[2,1]) + sqr(m_Frustum[2,2]));
  m_Frustum[2,0] := m_Frustum[2,0] / t;
  m_Frustum[2,1] := m_Frustum[2,1] / t;
  m_Frustum[2,2] := m_Frustum[2,2] / t;
  m_Frustum[2,3] := m_Frustum[2,3] / t;
  m_Frustum[3,0] := clip[3] - clip[1];                                          // Z�sk�n� horn� roviny
  m_Frustum[3,1] := clip[7] - clip[5];
  m_Frustum[3,2] := clip[11] - clip[9];
  m_Frustum[3,3] := clip[15] - clip[13];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[3,0]) + sqr(m_Frustum[3,1]) + sqr(m_Frustum[3,2]));
  m_Frustum[3,0] := m_Frustum[3,0] / t;
  m_Frustum[3,1] := m_Frustum[3,1] / t;
  m_Frustum[3,2] := m_Frustum[3,2] / t;
  m_Frustum[3,3] := m_Frustum[3,3] / t;
  m_Frustum[4,0] := clip[3] - clip[2];                                          // Z�sk�n� zadn� roviny
  m_Frustum[4,1] := clip[7] - clip[6];
  m_Frustum[4,2] := clip[11] - clip[10];
  m_Frustum[4,3] := clip[15] - clip[14];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[4,0]) + sqr(m_Frustum[4,1]) + sqr(m_Frustum[4,2]));
  m_Frustum[4,0] := m_Frustum[4,0] / t;
  m_Frustum[4,1] := m_Frustum[4,1] / t;
  m_Frustum[4,2] := m_Frustum[4,2] / t;
  m_Frustum[4,3] := m_Frustum[4,3] / t;
  m_Frustum[5,0] := clip[3] + clip[2];                                          // Z�sk�n� p�edn� roviny
  m_Frustum[5,1] := clip[7] + clip[6];
  m_Frustum[5,2] := clip[11] + clip[10];
  m_Frustum[5,3] := clip[15] + clip[14];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[5,0]) + sqr(m_Frustum[5,1]) + sqr(m_Frustum[5,2]));
  m_Frustum[5,0] := m_Frustum[5,0] / t;
  m_Frustum[5,1] := m_Frustum[5,1] / t;
  m_Frustum[5,2] := m_Frustum[5,2] / t;
  m_Frustum[5,3] := m_Frustum[5,3] / t;
end;

procedure glCamera.UpdateFrustumFaster;                                         // Z�sk�n� o�ez�vac�ch rovin (optimalizovan� funkce)
var
  clip: array [0..15] of GLfloat;                                               // Pomocn� matice
  proj: array [0..15] of GLfloat;                                               // Projek�n� matice
  modl: array [0..15] of GLfloat;                                               // Modelview matice
  t: GLfloat;                                                                   // Pomocn�
begin
  glGetFloatv(GL_PROJECTION_MATRIX,@proj);                                      // Z�sk�n� projek�n� matice
  glGetFloatv(GL_MODELVIEW_MATRIX,@modl);                                       // Z�sk�n� modelview matice
  // Vyn�sob� projek�n� matici pomoc� modelview (nesm� b�t p�ed t�m pou�ita rotace ani translace)
  clip[0] := modl[0] * proj[0];
  clip[1] := modl[1] * proj[5];
  clip[2] := modl[2] * proj[10] + modl[3] * proj[14];
  clip[3] := modl[2] * proj[11];
  clip[4] := modl[4] * proj[0];
  clip[5] := modl[5] * proj[5];
  clip[6] := modl[6] * proj[10] + modl[7] * proj[14];
  clip[7] := modl[6] * proj[11];
  clip[8] := modl[8] * proj[0];
  clip[9] := modl[9] * proj[5];
  clip[10] := modl[10] * proj[10] + modl[11] * proj[14];
  clip[11] := modl[10] * proj[11];
  clip[12] := modl[12] * proj[0];
  clip[13] := modl[13] * proj[5];
  clip[14] := modl[14] * proj[10] + modl[15] * proj[14];
  clip[15] := modl[14] * proj[11];
  m_Frustum[0,0] := clip[3] - clip[0];                                          // Z�sk�n� prav� roviny
  m_Frustum[0,1] := clip[7] - clip[4];
  m_Frustum[0,2] := clip[11] - clip[8];
  m_Frustum[0,3] := clip[15] - clip[12];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[0,0]) + sqr(m_Frustum[0,1]) + sqr(m_Frustum[0,2]));
  m_Frustum[0,0] := m_Frustum[0,0] / t;
  m_Frustum[0,1] := m_Frustum[0,1] / t;
  m_Frustum[0,2] := m_Frustum[0,2] / t;
  m_Frustum[0,3] := m_Frustum[0,3] / t;
  m_Frustum[1,0] := clip[3] + clip[0];                                          // Z�sk�n� lev� roviny
  m_Frustum[1,1] := clip[7] + clip[4];
  m_Frustum[1,2] := clip[11] + clip[8];
  m_Frustum[1,3] := clip[15] + clip[12];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[1,0]) + sqr(m_Frustum[1,1]) + sqr(m_Frustum[1,2]));
  m_Frustum[1,0] := m_Frustum[1,0] / t;
  m_Frustum[1,1] := m_Frustum[1,1] / t;
  m_Frustum[1,2] := m_Frustum[1,2] / t;
  m_Frustum[1,3] := m_Frustum[1,3] / t;
  m_Frustum[2,0] := clip[3] + clip[1];                                          // Z�sk�n� spodn� roviny
  m_Frustum[2,1] := clip[7] + clip[5];
  m_Frustum[2,2] := clip[11] + clip[9];
  m_Frustum[2,3] := clip[15] + clip[13];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[2,0]) + sqr(m_Frustum[2,1]) + sqr(m_Frustum[2,2]));
  m_Frustum[2,0] := m_Frustum[2,0] / t;
  m_Frustum[2,1] := m_Frustum[2,1] / t;
  m_Frustum[2,2] := m_Frustum[2,2] / t;
  m_Frustum[2,3] := m_Frustum[2,3] / t;
  m_Frustum[3,0] := clip[3] - clip[1];                                          // Z�sk�n� horn� roviny
  m_Frustum[3,1] := clip[7] - clip[5];
  m_Frustum[3,2] := clip[11] - clip[9];
  m_Frustum[3,3] := clip[15] - clip[13];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[3,0]) + sqr(m_Frustum[3,1]) + sqr(m_Frustum[3,2]));
  m_Frustum[3,0] := m_Frustum[3,0] / t;
  m_Frustum[3,1] := m_Frustum[3,1] / t;
  m_Frustum[3,2] := m_Frustum[3,2] / t;
  m_Frustum[3,3] := m_Frustum[3,3] / t;
  m_Frustum[4,0] := clip[3] - clip[2];                                          // Z�sk�n� zadn� roviny
  m_Frustum[4,1] := clip[7] - clip[6];
  m_Frustum[4,2] := clip[11] - clip[10];
  m_Frustum[4,3] := clip[15] - clip[14];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[4,0]) + sqr(m_Frustum[4,1]) + sqr(m_Frustum[4,2]));
  m_Frustum[4,0] := m_Frustum[4,0] / t;
  m_Frustum[4,1] := m_Frustum[4,1] / t;
  m_Frustum[4,2] := m_Frustum[4,2] / t;
  m_Frustum[4,3] := m_Frustum[4,3] / t;
  m_Frustum[5,0] := clip[3] + clip[2];                                          // Z�sk�n� p�edn� roviny
  m_Frustum[5,1] := clip[7] + clip[6];
  m_Frustum[5,2] := clip[11] + clip[10];
  m_Frustum[5,3] := clip[15] + clip[14];
  // Normalizace v�sledku
  t := sqrt(sqr(m_Frustum[5,0]) + sqr(m_Frustum[5,1]) + sqr(m_Frustum[5,2]));
  m_Frustum[5,0] := m_Frustum[5,0] / t;
  m_Frustum[5,1] := m_Frustum[5,1] / t;
  m_Frustum[5,2] := m_Frustum[5,2] / t;
  m_Frustum[5,3] := m_Frustum[5,3] / t;
end;

end.
