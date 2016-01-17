// libPortForward.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "UPnPUtils.h"

extern "C" __declspec(dllexport) int _SetPortForwarding(char *localIP, char *description, int internalPort, int *externalPort)
{
	return SetPortForwarding( localIP, description, internalPort, externalPort );
}
