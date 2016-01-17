#pragma once
#include "stdafx.h"

#pragma pack(push, 1)
struct COLOR24BIT
{
	unsigned char B;
	unsigned char G;
	unsigned char R;
};
#pragma pack(pop)

class TImageContainer
{
private:
	COLOR24BIT* Data;

	int Width;
	int Height;
public:
	TImageContainer(int AWidth, int AHeight);
	TImageContainer();

	int GetWidth();
	int GetHeight();
	bool CreateImage(int AWidth, int AHeight);
	COLOR24BIT* GetPointerByPoint(int x, int y);

	void* GetMemoryPointer();
	void CopyImage(TImageContainer* Dst, int OriX, int OriY, int DstX, int DstY, int AWidth, int AHeight);
	virtual ~TImageContainer();
};