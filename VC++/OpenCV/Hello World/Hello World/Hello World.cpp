// Hello World.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <opencv2\opencv.hpp>


using namespace cv;


int _tmain(int argc, _TCHAR* argv[])
{
	Mat image;

	VideoCapture cap(0);
	cap.set(CV_CAP_PROP_FRAME_WIDTH, 1280);
	cap.set(CV_CAP_PROP_FRAME_HEIGHT, 720);

	while (true) {
		cap.grab();
		cap >> image;
		imshow("Hello", image);

		if (waitKey(20) == 0x1B) break;
	}


	return 0;
}

