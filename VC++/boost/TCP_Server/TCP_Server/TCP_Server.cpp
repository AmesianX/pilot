// TCP_Server.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "TCP_Server.hpp"

int _tmain(int argc, _TCHAR* argv[])
{
	TCP_Server server;
	server.start();
	return 0;
}

