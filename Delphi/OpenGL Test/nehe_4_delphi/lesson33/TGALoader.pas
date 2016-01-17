unit TGALoader;

interface

uses opengl, windows;

type
  Texture = record                                  // Struktura textury
    imageData: PGLubyte;                            // Data
    bpp: GLuint;                                    // Barevn� hloubka v bitech
    width: GLuint;                                  // ���ka
    height: GLuint;                                 // V��ka
    texID: GLuint;                                  // ID textury
    typ: GLuint;                                    // Typ (GL_RGB, GL_RGBA)
    end;

  PTexture = ^Texture;                              // Ukazatel na strukturu textury

  TTGAHeader = record                               // Hlavi�ka TGA souboru
    Header: array [0..11] of GLubyte;               // Dvan�ct byt�
    end;

  TTGA = record                                     // Struktura obr�zku
    header: array [0..5] of GLubyte;                // �est u�ite�n�ch byt� z hlavi�ky
    bytesPerPixel: GLuint;                          // Barevn� hloubka v bytech
    imageSize: GLuint;                              // Velikost pam�ti pro obr�zek
    typ: GLuint;                                    // Typ
    Heigh: GLuint;                                  // V��ka
    Width: GLuint;                                  // ���ka
    Bpp: GLuint;                                    // Barevn� hloubka v bitech
    end;

function LoadTGA(var texture: Texture; filename: string): boolean;

var
  tgaheader: TTGAHeader;                            // TGA hlavi�ka
  tga: TTGA;                                        // TGA obr�zek
  uTGAcompare: array [0..11] of GLubyte = (0,0,2,0,0,0,0,0,0,0,0,0);  // TGA hlavi�ka nekomprimovan�ho obr�zku
  cTGAcompare: array [0..11] of GLubyte = (0,0,10,0,0,0,0,0,0,0,0,0); // TGA hlavi�ka komprimovan�ho obr�zku
  fTGA: file;                                       // Soubor

implementation

uses SysUtils;

function LoadUncompressedTGA(var texture: Texture; filename: string): boolean;  // Nahraje nekomprimovan� TGA
var
  precteno: integer;                                                            // Po�et p�e�ten�ch byt�
  i: integer;                                                                   // Cyklus
  B, R: PGLubyte;                                                               // Ukazatel na prohazovan� slo�ky barev
  temp: GLubyte;                                                                // Pomocn� prom�nn�
begin
  BlockRead(fTGA,tga.header,sizeof(tga.header),precteno);                       // �est u�ite�n�ch byt�
  if precteno <> sizeof(tga.header) then
    begin
    MessageBox(0,'Could not read info header','ERROR',MB_OK);
    Result := false;
    end;
  texture.width := tga.header[1] * 256 + tga.header[0];                         // ���ka
  texture.height := tga.header[3] * 256 + tga.header[2];                        // V��ka
  texture.bpp := tga.header[4];                                                 // Barevn� hloubka v bitech
  tga.Width := texture.width;                                                   // Kop�rov�n� dat do struktury obr�zku
  tga.Heigh := texture.height;
  tga.Bpp := texture.bpp;
  if (texture.width <= 0) or (texture.height <= 0) or                           // Platn� hodnoty?
      ((texture.bpp <> 24) and (texture.bpp <> 32)) then
    begin
    MessageBox(0,'Invalid texture information','ERROR',MB_OK);
    Result := false;
    end;
  if texture.bpp = 24 then                                                      // 24 bitov� obr�zek?
    texture.typ := GL_RGB
    else                                                                        // 32 bitov� obr�zek
    texture.typ := GL_RGBA;
  tga.bytesPerPixel := texture.bpp div 8;                                       // BYTY na pixel
  tga.imageSize := tga.bytesPerPixel * tga.Width * tga.Heigh;                   // Velikost pam�ti
  texture.imageData := AllocMem(tga.imageSize);                                 // Alokace pam�ti pro data
  if texture.imageData = nil then                                               // Alokace ne�sp�n�
    begin
    MessageBox(0,'Could not allocate memory for image','ERROR',MB_OK);
    Result := false;
    end;
  BlockRead(fTGA,texture.imageData^,tga.imageSize,precteno);                    // Pokus� se nahr�t data obr�zku
  if precteno <> tga.imageSize then
    begin
    MessageBox(0,'Could not read image data','ERROR',MB_OK);
    FreeMem(texture.imageData);                                                 // Uvoln�n� pam�ti
    Result := false;
    end;
  for i := 0 to (tga.Width * tga.Heigh) - 1 do                                  // P�evod BGR na RGB
    begin
    B := Pointer(Integer(texture.imageData) + i * tga.bytesPerPixel);           // Ukazatel na B
    R := Pointer(Integer(texture.imageData) + i * tga.bytesPerPixel+2);         // Ukazatel na R
    temp := B^;                                                                 // B ulo��me do pomocn� prom�nn�
    B^ := R^;                                                                   // R je na spr�vn�m m�st�
    R^ := temp;                                                                 // B je na spr�vn�m m�st�
    end;
  CloseFile(fTGA);                                                              // Zav�en� souboru
  Result := true;                                                               // �sp�ch
end;

function LoadCompressedTGA(var texture: Texture; filename: string): boolean;    // Nahraje komprimovan� obr�zek
var
  precteno: integer;                                                            // Po�et p�e�ten�ch byt�
  pixelcount: GLuint;                                                           // Po�et pixel�
  currentpixel: GLuint;                                                         // Aktu�ln� na��tan� pixel
  currentbyte: GLuint;                                                          // Aktu�ln� na��tan� byte
  colorbuffer: PGLubyte;                                                        // Ukazatel na pole byt�
  chunkheader: GLubyte;                                                         // Byte hlavi�ky
  counter: integer;                                                             // Cyklus
  R, G, B, A: PGLubyte;                                                         // Ukazatel na slo�ky barev
  temp: PGLubyte;                                                               // Pomocn� prom�nn�
begin
  BlockRead(fTGA,tga.header,sizeof(tga.header),precteno);                       // �est u�ite�n�ch byt�
  if precteno <> sizeof(tga.header) then
    begin
    MessageBox(0,'Could not read info header','ERROR',MB_OK);
    Result := false;
    end;
  texture.width := tga.header[1] * 256 + tga.header[0];                         // ���ka
  texture.height := tga.header[3] * 256 + tga.header[2];                        // V��ka
  texture.bpp := tga.header[4];                                                 // Barevn� hloubka v bitech
  tga.Width := texture.width;                                                   // Kop�rov�n� dat do struktury obr�zku
  tga.Heigh := texture.height;
  tga.Bpp := texture.bpp;
  if (texture.width <= 0) or (texture.height <= 0) or                           // Platn� hodnoty?
      ((texture.bpp <> 24) and (texture.bpp <> 32)) then
    begin
    MessageBox(0,'Invalid texture information','ERROR',MB_OK);
    Result := false;
    end;
  if texture.bpp = 24 then                                                      // 24 bitov� obr�zek?
    texture.typ := GL_RGB
    else                                                                        // 32 bitov� obr�zek
    texture.typ := GL_RGBA;
  tga.bytesPerPixel := texture.bpp div 8;                                       // BYTY na pixel
  tga.imageSize := tga.bytesPerPixel * tga.Width * tga.Heigh;                   // Velikost pam�ti
  texture.imageData := AllocMem(tga.imageSize);                                 // Alokace pam�ti pro data
  if texture.imageData = nil then                                               // Alokace ne�sp�n�
    begin
    MessageBox(0,'Could not allocate memory for image','ERROR',MB_OK);
    Result := false;
    end;
  pixelcount := tga.Width * tga.Heigh;                                          // Po�et pixel�
  currentpixel := 0;                                                            // Aktu�ln� na��tan� pixel
  currentbyte := 0;                                                             // Aktu�ln� na��tan� byte
  colorbuffer := AllocMem(tga.bytesPerPixel);                                   // Pam� pro jeden pixel
  if colorbuffer = nil then                                                     // Alokace ne�sp�n�
    begin
    MessageBox(0,'Could not allocate memory for color buffer','ERROR',MB_OK);
    FreeMem(texture.imageData);
    Result := false;
    end;
  repeat                                                                        // Proch�z� cel� soubor
  chunkheader := 0;                                                             // Byte hlavi�ky
  BlockRead(fTGA,chunkheader,sizeof(GLubyte),precteno);                         // Na�te byte hlavi�ky
  if precteno <> sizeof(GLubyte) then
    begin
    MessageBox(0,'Could not read RLE header','ERROR',MB_OK);
    FreeMem(texture.imageData);
    FreeMem(colorbuffer);
    Result := false;
    end;
  if chunkheader < 128 then                                                     // RAW ��st obr�zku
    begin
    Inc(chunkheader);                                                           // Po�et pixel� v sekci p�ed v�skytem dal��ho bytu hlavi�ky
    for counter := 0 to chunkheader - 1 do                                      // Jednotliv� pixely
      begin
      BlockRead(fTGA,colorbuffer^,tga.bytesPerPixel,precteno);                  // Na��t�n� po jednom pixelu
      if precteno <> tga.bytesPerPixel then
        begin
        MessageBox(0,'Could not read image data','ERROR',MB_OK);
        FreeMem(texture.imageData);
        FreeMem(colorbuffer);
        Result := false;
        end;
      R := Pointer(Integer(colorbuffer) + 2);                                   // Z�pis do pam�ti, prohod� R a B slo�ku barvy
      G := Pointer(Integer(colorbuffer) + 1);
      B := Pointer(Integer(colorbuffer) + 0);
      temp := Pointer(Integer(texture.imageData) + currentbyte);
      temp^ := R^;
      temp := Pointer(Integer(texture.imageData) + currentbyte + 1);
      temp^ := G^;
      temp := Pointer(Integer(texture.imageData) + currentbyte + 2);
      temp^ := B^;
      if tga.bytesPerPixel = 4 then                                             // 32 bitov� obr�zek?
        begin
        A := Pointer(Integer(colorbuffer) + 3);                                 // Kop�rov�n� alfy
        temp := Pointer(Integer(texture.imageData) + currentbyte + 3);
        temp^ := A^;
        end;
      Inc(currentbyte,tga.bytesPerPixel);                                       // Aktualizuje byte
      Inc(currentpixel);                                                        // P�esun na dal�� pixel
      if currentpixel > pixelcount then                                         // Jsme za hranic� obr�zku?
        begin
        MessageBox(0,'Too many pixels read','ERROR',MB_OK);
        FreeMem(texture.imageData);
        FreeMem(colorbuffer);
        Result := false;
        end;
      end;
    end
    else                                                                        // RLE ��st obr�zku
    begin
    Dec(chunkheader,127);                                                       // Po�et pixel� v sekci
    BlockRead(fTGA,colorbuffer^,tga.bytesPerPixel,precteno);                    // Na�te jeden pixel
    if precteno <> tga.bytesPerPixel then
      begin
      MessageBox(0,'Could not read from file','ERROR',MB_OK);
      FreeMem(texture.imageData);
      FreeMem(colorbuffer);
      Result := false;
      end;
    for counter := 0 to chunkheader - 1 do                                      // Kop�rov�n� pixelu
      begin
      R := Pointer(Integer(colorbuffer) + 2);                                   // Z�pis do pam�ti, prohod� R a B slo�ku barvy
      G := Pointer(Integer(colorbuffer) + 1);
      B := Pointer(Integer(colorbuffer) + 0);
      temp := Pointer(Integer(texture.imageData) + currentbyte);
      temp^ := R^;
      temp := Pointer(Integer(texture.imageData) + currentbyte + 1);
      temp^ := G^;
      temp := Pointer(Integer(texture.imageData) + currentbyte + 2);
      temp^ := B^;
      if tga.bytesPerPixel = 4 then                                             // 32 bitov� obr�zek?
        begin
        A := Pointer(Integer(colorbuffer) + 3);                                 // Kop�rov�n� alfy
        temp := Pointer(Integer(texture.imageData) + currentbyte + 3);
        temp^ := A^;
        end;
      Inc(currentbyte,tga.bytesPerPixel);                                       // Aktualizuje byte
      Inc(currentpixel);                                                        // P�esun na dal�� pixel
      if currentpixel > pixelcount then                                         // Jsme za hranic� obr�zku?
        begin
        MessageBox(0,'Too many pixels read','ERROR',MB_OK);
        FreeMem(texture.imageData);
        FreeMem(colorbuffer);
        Result := false;
        end;
      end;
    end;
  until currentpixel = pixelcount;                                              // Pokra�uj dokud zb�vaj� pixely
  FreeMem(colorbuffer);                                                         // Uvoln�n� dynamick� pam�ti
  CloseFile(fTGA);                                                              // Zav�en� souboru
  Result := true;                                                               // �sp�ch
end;

function LoadTGA(var texture: Texture; filename: string): boolean;              // Nahraje TGA soubor
var
  precteno: integer;                                                            // Po�et p�e�ten�ch byt�
begin
  AssignFile(fTGA,filename);                                                    // P�i�azen� souboru
  {$I-}
  Reset(fTGA,1);                                                                // Otev�e soubor
  {$I+}
  if IOResult <> 0 then                                                         // Nepoda�ilo se ho otev��t?
    begin
    MessageBox(0,'Could not open texture file','ERROR',MB_OK);
    Result := false;
    exit;
    end;
  BlockRead(fTGA,tgaheader,sizeof(tgaheader),precteno);                         // Na�te hlavi�ku souboru
  if precteno <> sizeof(tgaheader) then
    begin
    MessageBox(0,'Could not read file header','ERROR',MB_OK);
    CloseFile(fTGA);
    Result := false;
    exit;
    end;
  if CompareMem(@uTGAcompare,@tgaheader,sizeof(tgaheader)) then                 // Nekomprimovan�
    begin
    if not LoadUncompressedTGA(texture,filename) then
      begin
      CloseFile(fTGA);
      Result := false;
      exit;
      end;
    end
    else
    if CompareMem(@cTGAcompare,@tgaheader,sizeof(tgaheader)) then               // Komprimovan�
      begin
      if not LoadCompressedTGA(texture,filename) then
        begin
        CloseFile(fTGA);
        Result := false;
        exit;
        end;
      end
      else                                                                      // Ani jeden z nich
      begin
      MessageBox(0,'TGA file be type 2 or type 10','Invalid Image',MB_OK);
      CloseFile(fTGA);
      Result := false;
      exit;
      end;
  Result := true;                                                               // V�e v po��dku
end;

end.
