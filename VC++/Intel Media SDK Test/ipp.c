stdafx.h]
#include <ipp.h>
#pragma comment(lib, "ippcore.lib")
#pragma comment(lib, "ipps.lib")
#pragma comment(lib, "ippi.lib")
#pragma comment(lib, "ippcv.lib")


[IPPDlg.cpp]

//초기화 부분
BOOL CIPPDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
	const IppLibraryVersion* lib = ippiGetLibVersion();
	if (lib)
	{
		CString strVersion;
		strVersion.Format("%d.%d.%d.%d [%s], %s, %s, %s",
			lib->major,
			lib->minor,
			lib->majorBuild,
			lib->build,
			lib->targetCpu,
			lib->Name,
			lib->Version,
			lib->BuildDate);
		CString strText;
		strText.Format("Version:\r\n%s", strVersion);
		GetDlgItem(IDC_VERSION)->SetWindowText(strText);
	}
	const IppCpuType cpu = ippGetCpuType();
	if (cpu)
	{
		CString strCPU;
		switch (cpu)
		{
		case ippCpuUnknown:
			strCPU = "Unknown";
			break;
		case ippCpuPP:
			strCPU = "Intel(R) Pentium(R) processor";
			break;
		case ippCpuPMX:
			strCPU = "Pentium(R) processor with MMX(TM) technology";
			break;
		case ippCpuPPR:
			strCPU = "Pentium(R) Pro processor";
			break;
		case ippCpuPII:
			strCPU = "Pentium(R) II processor";
			break;
		case ippCpuPIII:
			strCPU = "Pentium(R) III processor and Pentium(R) III Xeon(R) processor";
			break;
		case ippCpuP4:
			strCPU = "Pentium(R) 4 processor and Intel(R) Xeon(R) processor";
			break;
		case ippCpuP4HT:
			strCPU = "Pentium(R) 4 Processor with HT Technology";
			break;
		case ippCpuP4HT2:
			strCPU = "Pentium(R) 4 processor with Streaming SIMD Extensions 3";
			break;
		case ippCpuCentrino:
			strCPU = "Intel(R) Centrino(TM) mobile technology";
			break;
		case ippCpuDS:
			strCPU = "Intel(R) Core(TM) Duo processor or Intel(R) Core(TM) Solo processor";
			break;
		case ippCpuITP:
			strCPU = "Intel(R) Itanium(R) processor";
			break;
		case ippCpuITP2:
			strCPU = "Intel(R) Itanium(R) 2 processor";
			break;
			// case ippCpuEM64T:
			//strCPU="Intel(R) Extended Memory 64 Technology (Intel(R) EM64T)        */
			// case ippCpuNext, 
			// case ippCpuSSE   = 0x40, /* Processor supports Pentium(R) III processor instruction set    */
			// case ippCpuSSE2,        /* Processor supports Streaming SIMD Extensions 2 instruction set */
			// case ippCpuSSE3,        /* Processor supports Streaming SIMD Extensions 3 instruction set */
			// case ippCpuX8664         /* Processor supports 64 bit extension                                 
		}
		int NumCores = ippGetNumCoresOnDie();
		CString strText;
		strText.Format("CPU:\r\n%d Core(s)\r\n%s", NumCores, strCPU);
		GetDlgItem(IDC_CPU)->SetWindowText(strText);
	}
	return TRUE;  // return TRUE  unless you set the focus to a control
}



//실행부분
void CIPPDlg::OnBnClickedButton1()
{
	UpdateData(TRUE);
	CWaitCursor wait;
	LARGE_INTEGER frequency, tStart, tEnd;
	QueryPerformanceFrequency(&frequency);
	IppiSize maskSize = { 3, 3 };
	IppiPoint anchor = { 1, 1 };
	IppiSize sizeSrc = { m_nWidth, m_nHeight };
	IppiSize sizeDst = { m_nWidth, m_nHeight };
	IppiSize szFiltter = { m_nWidth - 2, m_nHeight - 2 };// Filter ROI Size 3x3=2, 5x5=4

	// Step Size
	int nStepSrc = (8 * sizeSrc.width + 31) / 32 * 4;// Step = ((BitSize * Width + 31) / 32) * 4
	int nStepDst = (8 * sizeDst.width + 31) / 32 * 4;
	int nStepTmp = (8 * szFiltter.width + 31) / 32 * 4;
	// 메모리 할당
	Ipp8u* pipDataSrc = ippiMalloc_8u_C1(sizeSrc.width, sizeSrc.height, &nStepSrc);
	Ipp8u* pipDataDst = ippiMalloc_8u_C1(sizeDst.width, sizeDst.height, &nStepDst);
	//Ipp8u* pipDataTmp = (Ipp8u*)ippMalloc( nStepTmp * szFiltter.height);

	IppStatus status = ippStsNoErr;

	// 메모리 초기화
	status = ippiImageJaehne_8u_C1R(pipDataSrc, nStepSrc, sizeSrc);
	status = ippiImageJaehne_8u_C1R(pipDataDst, nStepDst, sizeDst);
	//status = ippiImageJaehne_8u_C1R(pipDataTmp, nStepTmp, szFiltter);
	GetDlgItem(IDC_STATUS)->SetWindowText(ippGetStatusString(status));
	// 원본 버퍼저장
	CStdioFile rfile1;
	rfile1.Open("c:\\ipp_8u_1.raw", CFile::modeCreate | CFile::modeWrite | CFile::typeBinary);
	rfile1.Write(pipDataSrc, sizeof(Ipp8u)*nStepSrc*m_nHeight);
	rfile1.Close();

	// ROI 시작부분 계산
	Ipp8u* pipSrcROI = (Ipp8u*)(((Ipp8u*)pipDataSrc) + anchor.y * nStepSrc + anchor.x * sizeof(Ipp8u));
	Ipp8u* pipDstROI = (Ipp8u*)(((Ipp8u*)pipDataDst) + anchor.y * nStepDst + anchor.x * sizeof(Ipp8u));
	QueryPerformanceCounter(&tStart);
	switch (m_idxFilter)
	{
	case 0://Sharpen
		status = ippiFilterSharpen_8u_C1R(pipSrcROI, nStepSrc, pipDstROI, nStepDst, szFiltter);
		break;
	case 1://Lowpass
		status = ippiFilterLowpass_8u_C1R(pipSrcROI, nStepSrc, pipDstROI, nStepDst, szFiltter, ippMskSize3x3);
		break;
	case 2://Hipass
		status = ippiFilterHipass_8u_C1R(pipSrcROI, nStepSrc, pipDstROI, nStepDst, szFiltter, ippMskSize3x3);
		break;
	case 3://Gauss
		status = ippiFilterGauss_8u_C1R(pipSrcROI, nStepSrc, pipDstROI, nStepDst, szFiltter, ippMskSize3x3);
		break;
	case 4://Median
		status = ippiFilterMedian_8u_C1R(pipSrcROI, nStepSrc, pipDstROI, nStepDst, szFiltter, maskSize, anchor);
		break;
	case 5://Min
		status = ippiFilterMin_8u_C1R(pipSrcROI, nStepSrc, pipDstROI, nStepDst, szFiltter, maskSize, anchor);
		break;
	case 6://Max
		status = ippiFilterMax_8u_C1R(pipSrcROI, nStepSrc, pipDstROI, nStepDst, szFiltter, maskSize, anchor);
		break;
	case 7://Laplace
		status = ippiFilterLaplace_8u_C1R(pipSrcROI, nStepSrc, pipDstROI, nStepDst, szFiltter, ippMskSize3x3);
		break;
	case 8://Wiener
	{
			   int size = 0;
			   status = ippiFilterWienerGetBufferSize(szFiltter, maskSize, 1, &size);
			   Ipp8u* pBuffer = new Ipp8u[size + 2];
			   Ipp32f noise = 0;
			   status = ippiFilterWiener_8u_C1R(pipSrcROI, nStepSrc, pipDstROI, nStepDst, szFiltter, maskSize, anchor, &noise, (Ipp8u*)pBuffer);
			   delete pBuffer;
	}
		break;
	}
	QueryPerformanceCounter(&tEnd);
	// 필터링된 버퍼저장
	CStdioFile rfile2;
	rfile2.Open("c:\\ipp_8u_2.raw", CFile::modeCreate | CFile::modeWrite | CFile::typeBinary);
	rfile2.Write(pipDataDst, sizeof(Ipp8u)*nStepDst * sizeDst.height);
	rfile2.Close();
	Ipp64f Mean = 0;
	Ipp64f StdDev = 0;
	status = ippiMean_StdDev_8u_C1R(pipDataDst, nStepDst, sizeDst, &Mean, &StdDev);
	// 메모리 해제
	ippiFree(pipDataSrc);
	ippiFree(pipDataDst);
	//ippiFree(pipDataTmp);
	GetDlgItem(IDC_STATUS)->SetWindowText(ippGetStatusString(status));

	// 수행시간 계산
	CString strTime;
	strTime.Format("%3.5f msec\r\nMean = %3.2f , StdDev = %3.2f", (double)((tEnd.QuadPart - tStart.QuadPart) / (double)frequency.QuadPart)*(double)1000., Mean, StdDev);
	GetDlgItem(IDC_TIME)->SetWindowText(strTime);

}