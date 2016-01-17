#include "stdafx.h"
#include "UPnPUtils.h"

int SetPortForwarding(char *localIP, char *description, int internalPort, int *externalPort)
{
	CoInitialize(NULL);

	int errorCode = 0;

	IUPnPNAT *nat = NULL;
	IStaticPortMappingCollection *mappingCollection = NULL;	
	IStaticPortMapping *mapping = NULL;	

	if( !SUCCEEDED( CoCreateInstance(__uuidof(UPnPNAT), NULL, CLSCTX_ALL, __uuidof(IUPnPNAT), (void **)&nat) ) || ( nat==NULL ) )
	{
		errorCode = ERROR_COCREATEINSTANCE;
		goto ERROR_EXIT;
	}

	if ( !SUCCEEDED( nat->get_StaticPortMappingCollection(&mappingCollection) ) || (mappingCollection==NULL ) )
	{
		errorCode = ERROR_UPNP_NOT_FOUNDED;
		goto ERROR_EXIT;
	}

	while( TRUE ) {
		IStaticPortMapping *existMapping = NULL;

		BOOL hasMappingInformation =
			SUCCEEDED( mappingCollection->get_Item(*externalPort, L"TCP", &existMapping) );

		if ( hasMappingInformation ) {
			//printf( "hasMappingInformation \n" );

			BSTR bStrIP = NULL;
			existMapping->get_InternalClient(&bStrIP);

			BSTR bstrDescryption = NULL;
			existMapping->get_Description(&bstrDescryption);

			long iExistInternalPort = 0;
			existMapping->get_InternalPort(&iExistInternalPort);

			if( bStrIP != NULL && bstrDescryption != NULL )	{
				//printf( "bStrIP != NULL && bstrDescryption != NULL \n" );

				USES_CONVERSION;

				char *sClientIP = OLE2A(bStrIP);
				char *sDescryption = OLE2A(bstrDescryption);

				BOOL hasMapping = 
					( strcmp(sClientIP, localIP) == 0 ) && 
					( strcmp(sDescryption, description) == 0) && 
					( iExistInternalPort == internalPort );

				if ( hasMapping )	{
					//printf( "hasMapping \n" );

					SysFreeString(bStrIP);
					SysFreeString(bstrDescryption);

					break;
				}

				SysFreeString(bStrIP);
				SysFreeString(bstrDescryption);
			}

			existMapping->Release();

			(*externalPort)++;
			//printf( "(*externalPort)++: %d \n", *externalPort );
		} else {
			//printf( "not hasMappingInformation \n" );

			VARIANT_BOOL vb = VARIANT_TRUE;

			USES_CONVERSION;

			BOOL isNewMappingRegistered = 
				SUCCEEDED( mappingCollection->Add(*externalPort, L"TCP", internalPort, A2W(localIP), vb, A2W(description), &mapping) );

			if( ! isNewMappingRegistered ) {
				//printf( "not isNewMappingRegistered \n" );

				errorCode = ERROR_PORTMAPPING_FAILED;
				goto ERROR_EXIT;
			}

			break;
		}
	}

ERROR_EXIT:

	if ( NULL != mapping ) {
		mapping->Release();
		mapping = NULL;
	}

	if ( NULL != mappingCollection ) {
		mappingCollection->Release();
		mappingCollection = NULL;
	}

	if ( NULL != nat ) {
		nat->Release();
		nat = NULL;
	}

	CoUninitialize();

	return errorCode;
}