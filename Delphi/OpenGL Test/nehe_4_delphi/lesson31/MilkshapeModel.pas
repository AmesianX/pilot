unit MilkshapeModel;

interface

uses model, classes, sysutils, windows;

type
  {MS3D STRUKTURA}

  MS3DHeader = packed record                                          // Hlavi�ka souboru
    m_ID: array [0..9] of char;
    m_version: integer;
    end;

  MS3DVertex = packed record                                          // Informace o vertexu
    m_flags: byte;
    m_vertex: array [0..2] of single;
    m_boneID: char;
    m_refCount: byte;
    end;

  MS3DTriangle = packed record                                        // Informace o troj�heln�ku
    m_flags: word;
    m_vertexIndices: array [0..2] of word;
    m_vertexNormals: array [0..2,0..2] of single;
    m_s, m_t: array [0..2] of single;
    m_smoothingGroup: byte;
    m_groupIndex: byte;
    end;

  MS3DMaterial = packed record                                        // Informace o materi�lu
    m_name: array [0..31] of char;
    m_ambient: array [0..3] of single;
    m_diffuse: array [0..3] of single;
    m_specular: array [0..3] of single;
    m_emissive: array [0..3] of single;
    m_shininess: single;                                              // 0.0 - 128.0
    m_transparency: single;                                           // 0.0 - 1.0
    m_mode: byte;                                                     // 0, 1, 2
    m_texture: array [0..127] of char;
    m_alphamap: array [0..127] of char;
    end;

  MS3DJoint = packed record                                           // Joint informace
    m_flags: byte;
    m_name: array [0..31] of char;
    m_parentName: array [0..31] of char;
    m_rotation: array [0..2] of single;
    m_translation: array [0..2] of single;
    m_numRotationKeyframes: word;
    m_numTranslationKeyframes: word;
    end;

  MS3DKeyframe = packed record                                        // Keyframe informace
    m_time: single;
    m_parameter: array [0..2] of single;
    end;

  {Konec MS3D STRUKTURY}

  TMilkshapeModel = class(TModel)
    public
      constructor Create;                                             // Konstruktor
      destructor Destroy; override;                                   // Destruktor
      function loadModelData(filename: string): boolean; override;    // Loading objektu ze souboru
    end;

implementation

{ TMilkshapeModel }

constructor TMilkshapeModel.Create;
begin
end;

destructor TMilkshapeModel.Destroy;
begin
  inherited;
end;

function TMilkshapeModel.loadModelData(filename: string): boolean;
var
  inputFile: TFileStream;                                                       // Vstupn� soubor
  buffer: ^byte;                                                                // Obraz souboru v pam�ti
  pPtr: pointer;                                                                // Ukazatel do obraze souboru v pam�ti
  size, precteno: integer;                                                      // Velikost souboru
  pHeader: MS3DHeader;                                                          // Hlavi�ka
  nVertices, nTriangles, nGroups, nMaterials: integer;                          // Po�ty jednotliv�ch struktur
  i, j: integer;                                                                // Cykly
  pVertex: MS3DVertex;                                                          // Vertexy
  pTriangle: MS3DTriangle;                                                      // Troj�heln�ky
  vertexIndices: array [0..2] of integer;
  t: array [0..2] of single;
  materialIndex: char;                                                          // Index materi�lu
  pMaterial: MS3DMaterial;                                                      // Materi�l
begin
  try
  inputFile := TFileStream.Create(filename,fmOpenRead);                         // Otev�en� souboru
  if not Assigned(inputFile) then                                               // Poda�ilo se ho otev��t?
    begin
    Result := false;                                                            // Pokud ne, konec
    exit;
    end;
  size := inputFile.Seek(0,soFromEnd);                                          // Velikost souboru
  inputFile.Seek(0,soFromBeginning);
  buffer := AllocMem(size);                                                     // Alokace pam�ti pro kopii souboru
  precteno := inputFile.Read(buffer^,size);                                     // Vytvo�en� pam�ov� kopie souboru
  inputFile.Free;                                                               // Zav�en� souboru
  if precteno <> size then                                                      // Na�etl se cel� soubor?
    begin                                                                       // Pokud ne ...
    FreeMem(buffer,size);                                                       // Uvolnit alokovanou pam�
    Result := false;                                                            // Konec
    exit;
    end;
  pPtr := buffer;                                                               // Pomocn� ukazatel na kopii souboru
  pHeader := MS3DHeader(pPtr^);                                                 // Ukazatel na hlavi�ku
  pPtr := Pointer(Integer(pPtr) + Sizeof(MS3DHeader));                          // Posun za hlavi�ku
  if CompareStr(pHeader.m_ID,'MS3D000000') <> 0 then                            // Nen� Milkshape3D souborem
    begin
    FreeMem(buffer,size);                                                       // Uvolnit alokovanou pam�
    Result := false;                                                            // Konec
    exit;
    end;
  if (pHeader.m_version < 3) or (pHeader.m_version > 4) then                    // �patn� verze souboru, t��da podporuje pouze verze 1.3 a 1.4
    begin
    FreeMem(buffer,size);                                                       // Uvolnit alokovanou pam�
    Result := false;                                                            // Konec
    exit;
    end;
  nVertices := Word(pPtr^);                                                     // Po�et vertex�
  m_numVertices := nVertices;                                                   // Nastav� atribut t��dy
  SetLength(m_pVertices,nVertices);                                             // Alokace pam�ti pro vertexy
  pPtr := Pointer(Integer(pPtr) + Sizeof(Word));                                // Posun za po�et vertex�
  for i := 0 to nVertices - 1 do                                                // Nahr�v� vertexy
    begin
    pVertex := MS3DVertex(pPtr^);                                               // Ukazatel na vertex
    m_pVertices[i].m_boneID := pVertex.m_boneID;                                // Na�ten� vertexu
    CopyMemory(@m_pVertices[i].m_location,@pVertex.m_vertex,Sizeof(single)*3);
    pPtr := Pointer(Integer(pPtr) + Sizeof(MS3DVertex));                        // Posun za tento vertex
    end;
  nTriangles := Word(pPtr^);                                                    // Po�et troj�heln�k�
  m_numTriangles := nTriangles;                                                 // Nastav� atribut t��dy
  SetLength(m_pTriangles,nTriangles);                                           // Alokace pam�ti pro troj�heln�ky
  pPtr := Pointer(Integer(pPtr) + Sizeof(Word));                                // Posun za po�et troj�heln�k�
  for i := 0 to nTriangles - 1 do                                               // Na��t� troj�heln�ky
    begin
    pTriangle := MS3DTriangle(pPtr^);                                           // Ukazatel na troj�heln�k
    vertexIndices[0] := pTriangle.m_vertexIndices[0];                           // Na�ten� troj�heln�ku
    vertexIndices[1] := pTriangle.m_vertexIndices[1];
    vertexIndices[2] := pTriangle.m_vertexIndices[2];
    t[0] := 1.0 - pTriangle.m_t[0];
    t[1] := 1.0 - pTriangle.m_t[1];
    t[2] := 1.0 - pTriangle.m_t[2];
    CopyMemory(@m_pTriangles[i].m_vertexNormals,@pTriangle.m_vertexNormals,Sizeof(single)*3*3);
    CopyMemory(@m_pTriangles[i].m_s,@pTriangle.m_s,Sizeof(single)*3);
    CopyMemory(@m_pTriangles[i].m_t,@t,Sizeof(single)*3);
    CopyMemory(@m_pTriangles[i].m_vertexIndices,@vertexIndices,Sizeof(integer)*3);
    pPtr := Pointer(Integer(pPtr) + Sizeof(MS3DTriangle));                      // Posun za tento troj�heln�k
    end;
  nGroups := Word(pPtr^);                                                       // Po�et mesh�
  m_numMeshes := nGroups;                                                       // Nastav� atribut t��dy
  SetLength(m_pMeshes,nGroups);                                                 // Alokace pam�ti pro meshe
  pPtr := Pointer(Integer(pPtr) + Sizeof(Word));                                // Posun za po�et mesh�
  for i := 0 to nGroups - 1 do                                                  // Na��t� meshe
    begin
    pPtr := Pointer(Integer(pPtr) + Sizeof(byte));                              // Posun za flagy
    pPtr := Pointer(Integer(pPtr) + 32);                                        // Posun za jm�no
    nTriangles := Word(pPtr^);                                                  // Po�et troj�heln�k� v meshi
    pPtr := Pointer(Integer(pPtr) + Sizeof(Word));                              // Posun za po�et troj�heln�k�
    SetLength(m_pMeshes[i].m_pTriangleIndices,nTriangles);                      // Alokace pam�ti pro indexy troj�heln�k�
    for j := 0 to nTriangles - 1 do                                             // Na��t� indexy troj�heln�k�
      begin
      m_pMeshes[i].m_pTriangleIndices[j] := Word(pPtr^);                        // P�i�ad� index troj�heln�ku
      pPtr := Pointer(Integer(pPtr) + Sizeof(Word));                            // Posun za index troj�heln�ku
      end;
    materialIndex := Char(pPtr^);                                               // Na�te index materi�lu
    pPtr := Pointer(Integer(pPtr) + Sizeof(Char));                              // Posun za index materi�lu
    m_pMeshes[i].m_materialIndex := Integer(materialIndex);                     // Index materi�lu
    m_pMeshes[i].m_numTriangles := nTriangles;                                  // Po�et troj�heln�k�
    end;
  nMaterials := Word(pPtr^);                                                    // Po�et materi�l�
  m_numMaterials := nMaterials;                                                 // Nastav� atribut t��dy
  SetLength(m_pMaterials,nMaterials);                                           // Alokace pam�ti pro materi�ly
  pPtr := Pointer(Integer(pPtr) + Sizeof(Word));                                // Posun za po�et materi�l�
  for i := 0 to nMaterials - 1 do                                               // Proch�z� materi�ly
    begin                                                                       
    pMaterial := MS3DMaterial(pPtr^);                                           // Ukazatel na materi�l
    CopyMemory(@m_pMaterials[i].m_ambient,@pMaterial.m_ambient,Sizeof(single)*4); // Na�te materi�l
    CopyMemory(@m_pMaterials[i].m_diffuse,@pMaterial.m_diffuse,Sizeof(single)*4);
    CopyMemory(@m_pMaterials[i].m_specular,@pMaterial.m_specular,Sizeof(single)*4);
    CopyMemory(@m_pMaterials[i].m_emissive,@pMaterial.m_emissive,Sizeof(single)*4);
    m_pMaterials[i].m_shininess := pMaterial.m_shininess;
    m_pMaterials[i].m_pTextureFilename := pMaterial.m_texture;                  // Zkop�rov�n� jm�na souboru
    pPtr := Pointer(Integer(pPtr) + Sizeof(MS3DMaterial));                      // Posun za materi�l
    end;
  reloadTexture;                                                                // Nahraje textury
  FreeMem(buffer,size);                                                         // Sma�e kopii souboru
  Result := true;                                                               // Model byl nahr�n
  except                                                                        // P�i kritick� chyb� ve funkci ...
  inputFile.Free;                                                               // Uvolnit alokovanou pam�
  FreeMem(buffer,size);                                                         // Uvolnit alokovanou pam�
  Result := false;
  end;
end;

end.
