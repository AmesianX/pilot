// PortForward.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "UPnPUtils.h"

int _tmain(int argc, _TCHAR* argv[])
{
	char localIP[256];
	WideCharToMultiByte(CP_ACP, 0, argv[1], 256, localIP, 256, NULL, NULL);

	char description[256];
	WideCharToMultiByte(CP_ACP, 0, argv[2], 256, description, 256, NULL, NULL);

	int iInternalPort = _tstoi(argv[3]);
	int iExternalPort = _tstoi(argv[4]);

	printf( "* SetPortForwarding: localIP=%s,  description=%s, iInternalPort=%d, iExternalPort=%d \n", localIP,  description, iInternalPort, iExternalPort );

	int errorCode = SetPortForwarding( localIP,  description, iInternalPort, &iExternalPort );

	printf( "* Result: errorCode=%d, iInternalPort=%d, iExternalPort=%d \n", errorCode, iInternalPort, iExternalPort );

	return 0;
}