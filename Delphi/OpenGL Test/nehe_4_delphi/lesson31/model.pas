unit model;

interface

uses GLaux, sysutils, windows, opengl;

procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

type
  Vertex = record                                           // Struktura vertexu
    m_location: array [0..2] of GLfloat;                    // X, y, z sou�adnice
    m_boneID: Char;                                         // Pro skelet�ln� animaci
    end;

  Triangle = record                                         // Struktura troj�heln�ku
    m_vertexIndices: array [0..2] of integer;               // T�i indexy do pole vertex�
    m_s, m_t: array [0..2] of GLfloat;                      // Texturov� koordin�ty
    m_vertexNormals: array [0..2,0..2] of GLfloat;          // T�i norm�lov� vektory
    end;

  Mesh = record                                             // Mesh modelu
    m_pTriangleIndices: array of integer;                   // Indexy do troj�heln�k�
    m_numTriangles: integer;                                // Po�et troj�heln�k�
    m_materialIndex: integer;                               // Index do materi�l�
    end;

  Material = record                                         // Vlastnosti materi�l�
    m_ambient, m_diffuse,                                   // Reakce materi�lu na sv�tlo
    m_specular, m_emissive: array [0..3] of GLfloat;
    m_shininess: GLfloat;                                   // Lesk materi�lu
    m_texture: GLuint;                                      // Textura
    m_pTextureFilename: string;                             // Souborov� cesta k textu�e
    end;

  TModel = class                                                                // Obecn� �lo�i�t� dat (abstraktn� t��da)
    protected
      m_numVertices: integer;                                                   // Po�et vertex�
      m_pVertices: array of Vertex;                                             // Dynamick� pole vertex�
      m_numTriangles: integer;                                                  // Po�et troj�heln�k�
      m_pTriangles: array of Triangle;                                          // Dynamick� pole troj�heln�k�
      m_numMeshes: integer;                                                     // Po�et mesh�
      m_pMeshes: array of Mesh;                                                 // Dynamick� pole mesh�
      m_numMaterials: integer;                                                  // Po�et materi�l�
      m_pMaterials: array of Material;                                          // Dynamick� pole materi�l�
    public
      constructor Create;                                                       // Konstruktor
      destructor Destroy; override;                                             // Destruktor
      function loadModelData(filename: string): boolean; virtual; abstract;     // Loading objektu ze souboru
      procedure reloadTexture;                                                  // Znovunahr�n� textur
      procedure Draw;                                                           // Vykreslen� objektu
    end;

implementation

{ TModel }

constructor TModel.Create;                                                      // Konstruktor
begin
  m_numMeshes := 0;                                                             // Nulov�n� struktur
  m_pMeshes := nil;
  m_numMaterials := 0;
  m_pMaterials := nil;
  m_numTriangles := 0;
  m_pTriangles := nil;
  m_numVertices := 0;
  m_pVertices := nil;
end;

destructor TModel.Destroy;                                                      // Destruktor
var
  i: integer;
begin
  for i := 0 to m_numMeshes - 1 do                                              // Uvoln�n� v�ech struktur
    SetLength(m_pMeshes[i].m_pTriangleIndices,0);
  for i := 0 to m_numMaterials - 1 do
    m_pMaterials[i].m_pTextureFilename := '';
  m_numMeshes := 0;
  if m_pMeshes <> nil then
    begin
    SetLength(m_pMeshes,0);
    m_pMeshes := nil;
    end;
  m_numMaterials := 0;
  if m_pMaterials <> nil then
    begin
    SetLength(m_pMaterials,0);
    m_pMaterials := nil;
    end;
  m_numTriangles := 0;
  if m_pTriangles <> nil then
    begin
    SetLength(m_pTriangles,0);
    m_pTriangles := nil;
    end;
  m_numVertices := 0;
  if m_pVertices <> nil then
    begin
    SetLength(m_pVertices,0);
    m_pVertices := nil;
    end;
  inherited;
end;

procedure TModel.Draw;
var
  texEnabled: GLboolean;
  i, j, k: integer;
  materialIndex, triangleIndex, index: integer;
  pTri: ^Triangle;
begin
  texEnabled := glIsEnabled(GL_TEXTURE_2D);                                     // Ulo�� atribut
  for i := 0 to m_numMeshes - 1 do                                              // Meshe
    begin
    materialIndex := m_pMeshes[i].m_materialIndex;                              // Index
    if materialIndex >= 0 then                                                  // Obsahuje mesh index materi�lu?
      begin                                                                     // Nastav� OpenGL
      glMaterialfv(GL_FRONT,GL_AMBIENT,@m_pMaterials[materialIndex].m_ambient);
      glMaterialfv(GL_FRONT,GL_DIFFUSE,@m_pMaterials[materialIndex].m_diffuse);
      glMaterialfv(GL_FRONT,GL_SPECULAR,@m_pMaterials[materialIndex].m_specular);
      glMaterialfv(GL_FRONT,GL_EMISSION,@m_pMaterials[materialIndex].m_emissive);
      glMaterialf(GL_FRONT,GL_SHININESS,m_pMaterials[materialIndex].m_shininess);
      if m_pMaterials[materialIndex].m_texture > 0 then                         // Obsahuje materi�l texturu?
        begin
        glBindTexture(GL_TEXTURE_2D,m_pMaterials[materialIndex].m_texture);
        glEnable(GL_TEXTURE_2D);
        end
        else                                                                    // Bez textury
        glDisable(GL_TEXTURE_2D);
      end
      else                                                                      // Bez materi�lu nem��e b�t ani textura
      glDisable(GL_TEXTURE_2D);
    glBegin(GL_TRIANGLES);                                                      // Za��tek troj�heln�k�
      for j := 0 to m_pMeshes[i].m_numTriangles - 1 do                          // Troj�heln�ky v meshi
        begin
        triangleIndex := m_pMeshes[i].m_pTriangleIndices[j];                    // Index
        pTri := @m_pTriangles[triangleIndex];                                   // Troj�heln�k
        for k := 0 to 2 do                                                      // Vertexy v troj�heln�ku
          begin
          index := pTri.m_vertexIndices[k];                                     // Index vertexu
          glNormal3fv(@pTri.m_vertexNormals[k]);                                // Norm�la
          glTexCoord2f(pTri.m_s[k],pTri.m_t[k]);                                // Texturovac� sou�adnice
          glVertex3fv(@m_pVertices[index].m_location);                          // Sou�adnice vertexu
          end;
        end;
    glEnd;                                                                      // Konec kreslen�
    end;
  if texEnabled then                                                            // Obnoven� nastaven� OpenGL
    glEnable(GL_TEXTURE_2D)
    else
    glDisable(GL_TEXTURE_2D);
end;

function LoadBMP(FileName: pchar):PTAUX_RGBImageRec;                            // Nahraje bitmapu
begin
  if Filename = '' then                                                         // Byla p�ed�na cesta k souboru?
    begin
    Result := nil;                                                              // Pokud ne, konec
    exit;
    end;
  if not FileExists(Filename) then                                              // Existuje soubor?
    begin
    Result := nil;                                                              // Pokud ne, konec
    exit;
    end;
  Result := auxDIBImageLoadA(FileName);                                         // Na�te bitmapu a vr�t� na ni ukazatel
end;

function LoadGLTextures(FileName: pchar): GLuint;                               // Loading bitmapy a konverze na texturu
var pImage: PTAUX_RGBImageRec;                                                  // Ukl�d� bitmapu
    texture: GLuint;
begin
  texture := 0;
  ZeroMemory(@pImage,sizeof(pImage));                                           // Vynuluje pam�
  pImage := LoadBMP(FileName);                                                  // Nahraje bitmapu
  if Assigned(FileName) then                                                    // V�e je bez probl�m�?
    begin
    glGenTextures(1,texture);                                                   // Generuje texturu
    glBindTexture(GL_TEXTURE_2D,texture);                                       // Typick� vytv��en� textury z bitmapy
    glTexImage2D(GL_TEXTURE_2D,0,3,pImage.sizeX,pImage.sizeY,0,GL_RGB,GL_UNSIGNED_BYTE,pImage.data);    // Vlastn� vytv��en� textury
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);             // Filtrov�n� p�i zv�t�en�
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);             // Filtrov�n� p�i zmen�en�
    end;
  Result := texture;                                                            // Vrac� texturu
end;

procedure TModel.reloadTexture;                                                 // Nahr�n� textur
var
  i: integer;                                                                   // Cyklus
begin
  for i := 0 to m_numMaterials - 1 do                                           // Jednotliv� materi�ly
    if m_pMaterials[i].m_pTextureFilename <> '' then                            // Existuje �et�zec s cestou
      m_pMaterials[i].m_texture := LoadGLTextures(Pchar(m_pMaterials[i].m_pTextureFilename))  // Nahraje texturu
      else
      m_pMaterials[i].m_texture := 0;                                           // Nulou indikuje, �e materi�l nem� texturu
end;

end.
