
// ScanBoardDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ScanBoard.h"
#include "ScanBoardDlg.h"
#include "ImageContainer.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CScanBoardDlg dialog




CScanBoardDlg::CScanBoardDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CScanBoardDlg::IDD, pParent)
{
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CScanBoardDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CScanBoardDlg, CDialog)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//}}AFX_MSG_MAP
	ON_BN_CLICKED(IDC_BUTTON1, &CScanBoardDlg::OnBnClickedButton1)
END_MESSAGE_MAP()


// CScanBoardDlg message handlers

BOOL CScanBoardDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon

	// TODO: Add extra initialization here

	return TRUE;  // return TRUE  unless you set the focus to a control
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CScanBoardDlg::OnPaint()
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.GetSafeHdc()), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CClientDC * dc2 = new CClientDC(this);
		CDialog::OnPaint();
	}
}

// The system calls this function to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CScanBoardDlg::OnQueryDragIcon()
{
	return static_cast<HCURSOR>(m_hIcon);
}


const int ImgSize = 128;
const int Limit = 25;
const int RLimit = 60;

int ConvertBlight(int R, int B, int G)
{
	return (0.114 * B) + (0.587 * G) + (0.299 * R);
}
int ConvertBlight(COLOR24BIT* Pixel)
{
	return (0.114 * Pixel->B) + (0.587 * Pixel->G) + (0.299 * Pixel->R);
}

void ProcessImage(TImageContainer *Original)
{
	//Todo : 블럭 사이즈를 가변적으로
	//Todo : 휘도별 히스토그램
	//Todo : 펜 색을 복수개로.
	//Todo : 특정 색이 지나치게 많을 경우 펜 색이 아닌것을 투명처리 해버림
	/*
		펜 색과 먼것은 다 제거.

	*/
	COLOR24BIT* Pixel;
	bool Arr[ImgSize][ImgSize]; //true 글씨아님
	int AverR, AverG, AverB;
	int SumR, SumG, SumB;
	int Blight;
	
	ZeroMemory(Arr, sizeof(Arr));
	SumR = SumG = SumB = 0;
	for(int i = 0; i < Original->GetWidth(); ++i)
	{
		for(int j = 0; j < Original->GetHeight(); ++j)
		{
			Pixel = Original->GetPointerByPoint(i, j);
			SumR = SumR + Pixel->R;
			SumG = SumG + Pixel->G;
			SumB = SumB + Pixel->B;
		}		
	}	
	
	AverB = SumB / (ImgSize * ImgSize);
	AverG = SumG / (ImgSize * ImgSize);
	AverR = SumR / (ImgSize * ImgSize);
	
	//AverB = (0.114 * AverB); //Convert
	//AverG = (0.587 * AverG);
	//AverR = (0.299 * AverR);
	Blight = ConvertBlight(AverR, AverG, AverB);

	//피부의 붉은색 골라내기.
	for(int i = 0; i < Original->GetWidth(); ++i)
	{
		for(int j = 0; j < Original->GetHeight(); ++j)
		{
			Pixel = Original->GetPointerByPoint(i,j);
			if (Pixel->R > RLimit)
			{
				Arr[i][j] = true;
			}
		}
	}
	
	for(int i = 0; i < Original->GetWidth(); ++i)
	{
		for(int j = 0; j < Original->GetHeight(); ++j)
		{
			Pixel = Original->GetPointerByPoint(i,j);
			if((Blight * 0.70) < ConvertBlight(Pixel->R, Pixel->G, Pixel->B))
			{
				Arr[i][j] = true;
			}
		}
	}
//블러링
	/*
	가우시안 필터
	1/16 1/8 1/16
	1/8  1/4 1/8
	1/16 1/8 1/16
	*/
	for(int i = 1; i < Original->GetWidth() - 1; ++i)
	{
		for(int j = 1; j < Original->GetHeight() - 1; ++j)
		{
			Pixel = Original->GetPointerByPoint(i,j);
			double Result;
			Result = ConvertBlight(Original->GetPointerByPoint(i - 1, j - 1)) + ConvertBlight(Original->GetPointerByPoint(i - 1, j)) * 2 + ConvertBlight(Original->GetPointerByPoint(i - 1, j + 1)) + 
				     ConvertBlight(Original->GetPointerByPoint(i, j - 1)) * 2 + ConvertBlight(Original->GetPointerByPoint(i, j))     * 4 + ConvertBlight(Original->GetPointerByPoint(i, j + 1)) * 2 + 
					 ConvertBlight(Original->GetPointerByPoint(i + 1, j - 1)) + ConvertBlight(Original->GetPointerByPoint(i + 1, j)) * 2 + ConvertBlight(Original->GetPointerByPoint(i + 1, j + 1));
			Result = (Result / 16) / 255;
			
			if (Result > 0.4)
			{
				Arr[i][j] = true;
			}
			else
			{
				Arr[i][j] = false;
			}
		}
	}
//흑백률 카운터
	int Count = 0;
	for(int i = 0; i < Original->GetWidth(); ++i)
	{
		for(int j = 0; j < Original->GetHeight(); ++j)
		{
			Count = Count + int(Arr[i][j]);
		}
	}

	if (Count < (ImgSize * ImgSize) * 0.8)
	{
		for(int i = 0; i < Original->GetWidth(); ++i)
		{
			for(int j = 0; j < Original->GetHeight(); ++j)
			{
				Arr[i][j] = true;
			}
		}
	}

//흑백 변화
	for(int i = 0; i < Original->GetWidth(); ++i)
	{
		for(int j = 0; j < Original->GetHeight(); ++j)
		{
			Pixel = Original->GetPointerByPoint(i,j);
			if (Arr[i][j] == true) //글씨아님
			{
				Pixel->R = 255;
				Pixel->G = 255;
				Pixel->B = 255;
			}
			else
			{
				Pixel->R = 0;
				Pixel->G = 0;
				Pixel->B = 0;
			}
		}
	}

//샤프닝
}
void CScanBoardDlg::OnBnClickedButton1()
{
	CImage OriginalBitmap, TempBitmap;
	OriginalBitmap.Load(L"Before.bmp");
	TempBitmap.Create(ImgSize, ImgSize, 24);

	TImageContainer *Original = new TImageContainer(OriginalBitmap.GetWidth(), OriginalBitmap.GetHeight());
	int HoriCount = OriginalBitmap.GetWidth() / ImgSize;
	int VertCount = OriginalBitmap.GetHeight() / ImgSize;

	TImageContainer **Splites = new TImageContainer*[HoriCount];
	for(int i = 0; i < HoriCount; ++i)
	{
		Splites[i] = new TImageContainer[VertCount];
		for(int j = 0; j < VertCount; ++j)
		{
			Splites[i][j].CreateImage(ImgSize, ImgSize);
		}
	}
	
	//Splite
	memcpy(Original->GetMemoryPointer(), OriginalBitmap.GetPixelAddress(0, OriginalBitmap.GetHeight() - 1), OriginalBitmap.GetWidth() * OriginalBitmap.GetHeight() * 3); 
	for(int i = 0; i < HoriCount; ++i)
	{
		for(int j = 0; j < VertCount; ++j)
		{		
			Original->CopyImage(&Splites[i][j], i * ImgSize,j * ImgSize, 0, 0, ImgSize, ImgSize);
			memcpy(TempBitmap.GetPixelAddress(0, TempBitmap.GetHeight() - 1), Splites[i][j].GetMemoryPointer(), ImgSize * ImgSize * 3);
		}
	}

	for(int i = 0; i < HoriCount; ++i)
	{
		for(int j = 0; j < VertCount; ++j)
		{
			ProcessImage(&Splites[i][j]);
		}
	}


	//Merge
	for(int i = 0; i < HoriCount; ++i)
	{
		for(int j = 0; j < VertCount; ++j)
		{
			Splites[i][j].CopyImage(Original, 0, 0, i * ImgSize,j * ImgSize, ImgSize, ImgSize);			
		}
	}
	memcpy(OriginalBitmap.GetPixelAddress(0, OriginalBitmap.GetHeight() - 1), Original->GetMemoryPointer(), OriginalBitmap.GetWidth() * OriginalBitmap.GetHeight() * 3); 
	OriginalBitmap.Save(L"ReMarge.bmp");

	ShellExecute(0, L"open",L"ReMarge.bmp", NULL, NULL, SW_SHOW);
}
