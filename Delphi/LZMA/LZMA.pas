unit LZMA;

interface

uses
  Windows, Classes, SysUtils;

function LzmaCompress(dest: PByte; destLen: PDWORD; const src: PByte;
  srcLen: DWORD; outProps: PByte;
  outPropsSize: PDWORD; { *outPropsSize must be = 5 }
  level: Integer = 9; { 0 <= level <= 9, default = 5 }
  dictSize: DWORD = 16777216; { default = (1 << 24) }
  lc: Integer = 8; { 0 <= lc <= 8, default = 3 }
  lp: Integer = 4; { 0 <= lp <= 4, default = 0 }
  pb: Integer = 4; { 0 <= pb <= 4, default = 2 }
  fb: Integer = 273; { 5 <= fb <= 273, default = 32 }
  numThreads: Integer = 2 { 1 or 2, default = 2 }
  ): Integer; stdcall;

function LzmaUncompress(dest: PByte; destLen: PDWORD; const src: PByte;
    srcLen: PDWORD; const props: PByte; propsSize: DWORD): Integer; stdcall;

implementation

function LzmaCompress(dest: PByte; destLen: PDWORD; const src: PByte;
  srcLen: DWORD; outProps: PByte;
  outPropsSize: PDWORD; { *outPropsSize must be = 5 }
  level: Integer = 9; { 0 <= level <= 9, default = 5 }
  dictSize: DWORD = 16777216; { default = (1 << 24) }
  lc: Integer = 8; { 0 <= lc <= 8, default = 3 }
  lp: Integer = 4; { 0 <= lp <= 4, default = 0 }
  pb: Integer = 4; { 0 <= pb <= 4, default = 2 }
  fb: Integer = 273; { 5 <= fb <= 273, default = 32 }
  numThreads: Integer = 2 { 1 or 2, default = 2 }
  ): Integer;
  stdcall; external 'LZMA.dll' delayed;

function LzmaUncompress(dest: PByte; destLen: PDWORD; const src: PByte;
    srcLen: PDWORD; const props: PByte; propsSize: DWORD): Integer;
    stdcall; external 'LZMA.dll' delayed;

end.
