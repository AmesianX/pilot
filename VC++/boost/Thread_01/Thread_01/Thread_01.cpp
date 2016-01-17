// Thread_01.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <iostream>
#include <boost/thread.hpp>

using namespace std;

class CSampleIO
{
public:

	void TestThread()
	{
		while (true)   
		{
			cout << "1 ";
			boost::this_thread::sleep(boost::posix_time::millisec(500));
		}
	}

	void TestThreadSecond(int num)
	{
		while (true)
		{
			cout << num << " ";
			boost::this_thread::sleep(boost::posix_time::millisec(500));
		}
	}

	void TestThreadThird(int num, int num2)
	{
		while (true)
		{
			cout << num << " ";
			boost::this_thread::sleep(boost::posix_time::millisec(500));
		}
	}
};

int _tmain(int argc, _TCHAR* argv[])
{
	CSampleIO io;

	boost::thread th1 = boost::thread(boost::bind(&CSampleIO::TestThread, &io));
	boost::thread th2 = boost::thread(boost::bind(&CSampleIO::TestThreadSecond, &io, 2));
	boost::thread th3 = boost::thread(boost::bind(&CSampleIO::TestThreadThird, &io, 3, NULL));

	getchar();

	return 0;
}

