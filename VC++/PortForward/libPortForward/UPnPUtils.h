#pragma once

#include <windows.h>
#include <Natupnp.h>
#include <UPnP.h>
#include <comdef.h>
#include <CRTDBG.H>
#include <AtlConv.h>

#define ERROR_COCREATEINSTANCE -1;
#define ERROR_UPNP_NOT_FOUNDED -2;
#define ERROR_PORTMAPPING_FAILED -3;

int SetPortForwarding(char *localIP, char *description, int internalPort, int *externalPort);