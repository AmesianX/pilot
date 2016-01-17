##//*@@@+++@@@@******************************************************************
##//
##// Microsoft Windows Media
##// Copyright (C) Microsoft Corporation. All rights reserved.
##//
##//*@@@---@@@@******************************************************************
## Makefile for building WMPhoto Porting Kit
##

CC=cc

DIR_SYS=image/sys
DIR_DEC=image/decode
DIR_ENC=image/encode

DIR_GLUE=Systems/tools/WMPGlueLib
DIR_META=Systems/tools/WMPMetaLib
DIR_EXEC= Systems/tools/SampleCodeNew/WMPEncDecoder

CFLAGS=-I. -Icommon/include -I$(DIR_SYS) -I$(DIR_META) -D__ANSI__ -w -O
##
## Add following flag to CFLAGS above if target is a big endian machine
## -D_BIG__ENDIAN_
##
##--------------------------------
##
## Common files
##

OBJ_SYS=adapthuff.o image.o strcodec.o strPredQuant.o strTransform.o perfTimerANSI.o

$(OBJ_SYS):
	$(CC) $(CFLAGS) -c $(DIR_SYS)/$*.c


##--------------------------------
##
## Decode files
##

OBJ_DEC=decode.o huffman.o postprocess.o segdec.o strdec.o strInvTransform.o strPredQuantDec.o WMPTranscode.o

$(OBJ_DEC):
	$(CC) $(CFLAGS) -c $(DIR_DEC)/$*.c


##--------------------------------
##
## Encode files
##

OBJ_ENC=encode.o segenc.o strenc.o strFwdTransform.o strPredQuantEnc.o

$(OBJ_ENC):
	$(CC) $(CFLAGS) -c $(DIR_ENC)/$*.c

##--------------------------------
##
## Wmphoto library
##

libwmphoto.a: $(OBJ_ENC) $(OBJ_DEC) $(OBJ_SYS)
	ar rvu $@ $(OBJ_ENC) $(OBJ_DEC) $(OBJ_SYS)
	ranlib $@

##--------------------------------
##
## Glue files
##

OBJ_GLUE=WMPGlue.o WMPGlueBmp.o WMPGlueHdr.o WMPGluePFC.o WMPGluePnm.o WMPGlueTif.o WMPGlueWmp.o WMPGlueYUV.o

$(OBJ_GLUE):
	$(CC) $(CFLAGS) -I$(DIR_GLUE) -c $(DIR_GLUE)/$*.c

##--------------------------------
##
## Meta files
##

OBJ_META=WMPMeta.o

$(OBJ_META):
	$(CC) $(CFLAGS) -c $(DIR_META)/$*.c

##--------------------------------
##
## Wmphoto library
##
libwmglue.a: $(OBJ_GLUE) $(OBJ_META)
	ar rvu $@ $(OBJ_GLUE) $(OBJ_META)
	ranlib $@

##--------------------------------
##
## Enc app files
##

LIBRARIES=libwmglue.a libwmphoto.a
LIBS=-L. $(LIBRARIES)
ENCAPP=WmpEncApp

$(ENCAPP).exe: $(LIBRARIES) 
	$(CC) $(DIR_EXEC)/$(ENCAPP).c -o $(ENCAPP).exe $(CFLAGS) -I$(DIR_GLUE) $(LIBS)

##--------------------------------
##
## Dec app files
##

DECAPP=WmpDecApp

$(DECAPP).exe: $(LIBRARIES) 
	$(CC) $(DIR_EXEC)/$(DECAPP).c -o $(DECAPP).exe $(CFLAGS) -I$(DIR_GLUE) $(LIBS)

##--------------------------------
##
## Wmphoto library
##
all: $(ENCAPP).exe $(DECAPP).exe

clean:
	rm -rf *App.exe *.o libwm*.a 

##
