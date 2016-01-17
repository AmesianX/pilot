#include "stdafx.h"
#include "ImageContainer.h"

TImageContainer::TImageContainer( int AWidth, int AHeight )
{
	Data = new COLOR24BIT[AWidth * AHeight];
	this->Width = AWidth;
	this->Height = AHeight;
	ZeroMemory(Data, AWidth*AHeight*sizeof(COLOR24BIT));
}

TImageContainer::TImageContainer()
{
	Data = NULL;
}
COLOR24BIT* TImageContainer::GetPointerByPoint( int x, int y )
{
	COLOR24BIT *p =  Data + (((Height - y - 1) * Width) + x);
	return p;
}

void* TImageContainer::GetMemoryPointer()
{
	return Data;
}

void TImageContainer::CopyImage( TImageContainer* Dst, int OriX, int OriY, int DstX, int DstY, int AWidth, int AHeight )
{
	int ResultH = this->Height - OriY;
	int ResultW = this->Width - OriX;
	if (ResultH > AHeight) ResultH = AHeight;

	if (ResultW > AWidth) ResultW = AWidth;

	for(int i = 0; i < ResultH; ++i)
	{
		memcpy(Dst->GetPointerByPoint(DstX, i + DstY), this->GetPointerByPoint(OriX ,i + OriY), sizeof(COLOR24BIT) * ResultW);
	}
}

TImageContainer::~TImageContainer()
{
	delete [] Data;
}

bool TImageContainer::CreateImage( int AWidth, int AHeight )
{
	if(Data != NULL) return false;
	
	Data = new COLOR24BIT[AWidth * AHeight];
	this->Width = AWidth;	
	this->Height = AHeight;

	return true;
}

int TImageContainer::GetWidth()
{
	return Width;
}

int TImageContainer::GetHeight()
{	
	return Height;
}