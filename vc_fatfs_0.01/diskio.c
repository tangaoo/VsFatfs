#include "diskio.h"
#include <windows.h>  
#include <Winioctl.h>  
  
  
  
static HANDLE hDevice = NULL;  
  
  
static DSTATUS DiskStatus;  //初始话磁盘状态为未初始化  
  
/* 
磁盘初始化 
*/  
DSTATUS disk_initialize()  
{  
    // TODO: Add your control notification handler code here  
    hDevice = CreateFile("\\\\.\\G:",   
                        GENERIC_READ|GENERIC_WRITE,           //对资源的读写操作权限
                        FILE_SHARE_READ | FILE_SHARE_WRITE,   //共享模式
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
磁盘状态读取 
in:none 
*/  
DSTATUS disk_status ()  
{  
    return DiskStatus;  
}  
  
  
/* 
读磁盘 
out :RES_OK (0)  成功读取数据 
RES_ERROR  操作过程有错误发生 
RES_PARERR  参数错误 
RES_NOTRDY 磁盘没有初始化 
*/  
DRESULT disk_read (   
  BYTE* Buffer,        /* 读取数据缓存 */  
  DWORD SectorNumber,  /* 扇区号*/  
  BYTE SectorCount     /* 扇区数 */  
)  
{  
    DWORD bytesread = 0;  
  
    SetFilePointer (hDevice, SectorNumber*512, 0, FILE_BEGIN);  
      
    ReadFile (hDevice, Buffer, SectorCount*512, &bytesread, NULL);  
    return RES_OK;  
  
}  
  
  
DRESULT disk_write (    
  const BYTE* Buffer,        /* 读取数据缓存 */  
  DWORD SectorNumber,  /* 扇区号*/  
  BYTE SectorCount     /* 扇区数 */  
)  
{  
	DWORD dwByteReturned;
    DWORD bytesread = 0;  
    SetFilePointer (hDevice, SectorNumber*512, 0, FILE_BEGIN);  
	DeviceIoControl(
		hDevice,           
		FSCTL_LOCK_VOLUME, //win7需要，加锁，否则无法写人       
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
		FSCTL_UNLOCK_VOLUME, //锁定后要解锁       
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
  BYTE Command,    /* 命令 */  
  void* Buffer     /* 缓冲区 */  
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
        *(int *)Buffer = dg.Cylinders.LowPart * dg.TracksPerCylinder * dg.SectorsPerTrack;//返回扇区个数  
        break;  
  
//    case GET_SECTOR_SIZE:  
//        *(int *)Buffer = 512;//win平台上的扇区为512，直接返回  
  
    }  
    return RES_OK;  
}  
  
DWORD get_fattime (void)  
{  
  return 0;  
}  