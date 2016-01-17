// turbojpeg_test.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <windows.h>
#include <turbojpeg.h>


int _tmain(int argc, _TCHAR* argv[])
{
	tjhandle _jpegCompressor = tjInitCompress();
	
	const int JPEG_QUALITY = 75;
	const int COLOR_COMPONENTS = 3;
	const int _width = 1920;
	const int _height = 1080;
	long unsigned int _jpegSize = 0;
	unsigned char *_compressedImage = NULL; 
	unsigned char buffer[_width*_height*COLOR_COMPONENTS]; 

	//tjcompress2(_jpegCompressor, buffer, _width, 0, _height, TJPF_RGB,
	//		  &_compressedImage, &_jpegSize, TJSAMP_444, JPEG_QUALITY,
	//		  TJFLAG_FASTDCT);

	//tjDestroy(_jpegCompressor);

	//tjFree(&_compressedImage);

	return 0;
}

