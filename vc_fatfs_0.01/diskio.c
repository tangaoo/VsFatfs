#include "diskio.h"
#include <windows.h>  
#include <Winioctl.h>  
  
  
  
static HANDLE hDevice = NULL;  
  
  
static DSTATUS DiskStatus;  //��ʼ������״̬Ϊδ��ʼ��  
  
/* 
���̳�ʼ�� 
*/  
DSTATUS disk_initialize()  
{  
    // TODO: Add your control notification handler code here  
    hDevice = CreateFile("\\\\.\\G:",   
                        GENERIC_READ|GENERIC_WRITE,           //����Դ�Ķ�д����Ȩ��
                        FILE_SHARE_READ | FILE_SHARE_WRITE,   //����ģʽ
                        NULL, OPEN_EXISTING, 0, NULL  
                    );   
    if (hDevice == INVALID_HANDLE_VALUE)   
    {  
        DiskStatus = STA_NOINIT;  
        return DiskStatus;  
    }  
  
    return 0;  
}  
  
  
/* 
����״̬��ȡ 
in:none 
*/  
DSTATUS disk_status ()  
{  
    return DiskStatus;  
}  
  
  
/* 
������ 
out :RES_OK (0)  �ɹ���ȡ���� 
RES_ERROR  ���������д����� 
RES_PARERR  �������� 
RES_NOTRDY ����û�г�ʼ�� 
*/  
DRESULT disk_read (   
  BYTE* Buffer,        /* ��ȡ���ݻ��� */  
  DWORD SectorNumber,  /* ������*/  
  BYTE SectorCount     /* ������ */  
)  
{  
    DWORD bytesread = 0;  
  
    SetFilePointer (hDevice, SectorNumber*512, 0, FILE_BEGIN);  
      
    ReadFile (hDevice, Buffer, SectorCount*512, &bytesread, NULL);  
    return RES_OK;  
  
}  
  
  
DRESULT disk_write (    
  const BYTE* Buffer,        /* ��ȡ���ݻ��� */  
  DWORD SectorNumber,  /* ������*/  
  BYTE SectorCount     /* ������ */  
)  
{  
	DWORD dwByteReturned;
    DWORD bytesread = 0;  
    SetFilePointer (hDevice, SectorNumber*512, 0, FILE_BEGIN);  
	DeviceIoControl(
		hDevice,           
		FSCTL_LOCK_VOLUME, //win7��Ҫ�������������޷�д��       
		NULL,                       
		0,                          
		NULL, 
		0,
		&dwByteReturned,
		NULL
					);
    WriteFile (hDevice, Buffer, SectorCount*512, &bytesread, NULL);  
	DeviceIoControl(
		hDevice,           
		FSCTL_UNLOCK_VOLUME, //������Ҫ����       
		NULL,                       
		0,                          
		NULL, 
		0,
		&dwByteReturned,
		NULL
					);
    return RES_OK;  
}  
  
  
DRESULT disk_ioctl (   
  BYTE Command,    /* ���� */  
  void* Buffer     /* ������ */  
)  
{  
    DISK_GEOMETRY   dg;  
    DWORD ret = 0;  
  
    switch(Command)  
    {  
    case GET_SECTORS:  
        DeviceIoControl(hDevice,   
                IOCTL_DISK_GET_DRIVE_GEOMETRY,  
                NULL,                          // lpInBuffer  
                0,                             // nInBufferSize  
                &dg,  
                sizeof(DISK_GEOMETRY),  
                &ret,  
                NULL  
            );  
        *(int *)Buffer = dg.Cylinders.LowPart * dg.TracksPerCylinder * dg.SectorsPerTrack;//������������  
        break;  
  
//    case GET_SECTOR_SIZE:  
//        *(int *)Buffer = 512;//winƽ̨�ϵ�����Ϊ512��ֱ�ӷ���  
  
    }  
    return RES_OK;  
}  
  
DWORD get_fattime (void)  
{  
  return 0;  
}  