// Image Resize.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <windows.h>
#include <opencv2\opencv.hpp>


using namespace cv;


int _tmain(int argc, _TCHAR* argv[])
{
	//VideoCapture cap(0);
	//cap.set(CV_CAP_PROP_FORMAT, CV_RGB2BGRA);
	//cap.set(CV_CAP_PROP_FRAME_WIDTH, 1280);
	//cap.set(CV_CAP_PROP_FRAME_HEIGHT, 720);

	Mat image_src(720, 1280, CV_8UC4, Scalar(255, 255, 0, 255));
	//Mat image_dst(720 / 2, 1280 / 2, CV_8UC4);
	Mat image_dst(664, 1180, CV_8UC4);

	//cap.grab();
	//cap >> image_src;
	circle(image_src, Point(1280 / 2, 720 / 2), (720 / 2) - 5, Scalar(0, 0, 0, 0));

	LARGE_INTEGER tick_start, tick_end;
	QueryPerformanceCounter(&tick_start);

	for (int i = 0; i < 100; i++) {
		resize(image_src, image_dst, image_dst.size(), 0, 0, CV_INTER_NN);
	}

	QueryPerformanceCounter(&tick_end);

	// CV_INTER_AREA:     7068246
	// CV_INTER_CUBIC:    3183249
	// CV_INTER_LINEAR:   1931607
	// CV_INTER_LANCZOS4: 7964308
	// CV_INTER_NN:        591848

	printf("%d \n", tick_end.QuadPart - tick_start.QuadPart);

	imshow("Image Resize", image_dst);

	while (true) {
		if (waitKey(20) == 0x1B) break;
	}

	return 0;
}