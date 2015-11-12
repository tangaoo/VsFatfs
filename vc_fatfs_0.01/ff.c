/*--------------------------------------------------------------------------/
/  FatFs - FAT file system module  R0.01                     (C)ChaN, 2006
/---------------------------------------------------------------------------/
/ FatFs module is an experimenal project to implement FAT file system to
/ cheap microcontrollers. This is opened for education, reserch and
/ development. You can use, modify and republish it for non-profit or profit
/ use without any limitation under your responsibility.
/---------------------------------------------------------------------------/
/  Feb 26, 2006  R0.00  Prototype
/  Apr 29, 2006  R0.01  First stable version
/---------------------------------------------------------------------------*/

/*****************************************************************************
*注释:	何小龙
*日期:	2014.01.11
*博客:	http://blog.csdn.net/hexiaolong2009
*****************************************************************************/

#include <string.h>
#include "ff.h"			/* FatFs declarations */
#include "diskio.h"		/* Include file for user provided functions */


FATFS *FatFs;			/* Pointer to the file system object */


/*-------------------------------------------------------------------------
  Module Private Functions
-------------------------------------------------------------------------*/

/*----------------------*/
/* Change Window Offset */

//读取新的扇区内容到临时缓冲区win[]，
//如果sector为0，则只需要把win[]的内容写入对应的物理扇区即可
static
BOOL move_window (
	DWORD sector		/* Sector number to make apperance in the FatFs->win */
)						/* Move to zero only writes back dirty window */
{
	DWORD wsect;		//wsect用于检索FAT备份表的相应扇区
	FATFS *fs = FatFs;


	wsect = FatFs->winsect;
	/*首先检查目标扇区号是否与win[]对应的扇区号相同，如果相同则不进行任何操作。*/
	if (wsect != sector) {	/* Changed current window */
#ifndef _FS_READONLY
		BYTE n;

		//首先检测win[]的内容是否做过修改，如果修改过，则需要先将其写入SD卡中。
		if (FatFs->dirtyflag) {	/* Write back dirty window if needed */
			if (disk_write(FatFs->win, wsect, 1) != RES_OK) return FALSE;

			//清除修改标记
			FatFs->dirtyflag = 0;
			
			/*如果当前操作的是FAT表，那么需要将修改后的FAT表拷贝到对应的FAT备份中*/
			if (wsect < (FatFs->fatbase + FatFs->sects_fat)) {	/* In FAT area */
				for (n = FatFs->n_fats; n >= 2; n--) {	/* Refrect the change to all FAT copies */
					wsect += FatFs->sects_fat;
					if (disk_write(FatFs->win, wsect, 1) != RES_OK) break;
				}
			}
		}
#endif

		//然后再读取新的扇区内容到win[]中
		if (sector) {
			if (disk_read(FatFs->win, sector, 1) != RES_OK) return FALSE;
			FatFs->winsect = sector;	//更新当前缓冲区的扇区号
		}
	}
	return TRUE;
}



/*---------------------*/
/* Get Cluster State   */

//读取指定簇的内容，即获取下一条簇链的位置
static
DWORD get_cluster (
	DWORD clust			/* Cluster# to get the link information */
)
{
	FATFS *fs = FatFs;


	if ((clust >= 2) && (clust < fs->max_clust)) {		/* Valid cluster# */
		switch (fs->fs_type) {
		case FS_FAT16 :
			/*读取FAT表，因为FAT16一个扇区包含256个簇，所以这里要除以256*/
			if (!move_window(clust / 256 + fs->fatbase)) break;
			
			/*读取指定簇号的内容，该内容即为下一个簇链的簇号；
			由于FAT16中一个簇用2字节表示，所以这里乘以2；
			对511进行与运算，等同于对512取余。*/
			return LD_WORD(&(fs->win[((WORD)clust * 2) & 511]));
			
		case FS_FAT32 :
			/*读取FAT表，因为FAT32一个扇区包含128个簇，所以这里要除以128*/
			/*读取FAT表，因为FAT32一个扇区包含128个簇号位（每个簇的地址用4个字节的数表示），所以这里要除以128*/
			if (!move_window(clust / 128 + fs->fatbase)) break;
			
			/*读取指定簇号的内容，该内容即为下一个簇链的簇号；
			由于FAT32中一个簇用4字节表示，所以这里乘以4；
			对511进行与运算，等同于对512取余。*/
			return LD_DWORD(&(fs->win[((WORD)clust * 4) & 511]));
		}
	}
	return 1;	/* Return with 1 means failed */
}



/*--------------------------*/
/* Change a Cluster State   */

//将目标值写入FAT表中对应簇号的指定位置，注意:只在win[]中操作
#ifndef _FS_READONLY
static
BOOL put_cluster (
	DWORD clust,		/* Cluster# to change */
	DWORD val			/* New value to mark the cluster */
)
{
	FATFS *fs = FatFs;


	switch (fs->fs_type) {
	case FS_FAT16 :
		/*首先读取clust所在FAT表的相应扇区内容到win[]中*/
		if (!move_window(clust / 256 + fs->fatbase)) return FALSE;
		
		/*其次将val写入win[]中的相应位置*/
		ST_WORD(&(fs->win[((WORD)clust * 2) & 511]), (WORD)val);
		break;
	case FS_FAT32 :
		if (!move_window(clust / 128 + fs->fatbase)) return FALSE;
		ST_DWORD(&(fs->win[((WORD)clust * 4) & 511]), val);
		break;
	default :
		return FALSE;
	}

	/*最后标记win[]已做了修改，需要回写*/
	fs->dirtyflag = 1;
	return TRUE;
}
#endif



/*------------------------*/
/* Remove a Cluster Chain */

//删除一条簇链
#ifndef _FS_READONLY
static
BOOL remove_chain (
	DWORD clust			/* Cluster# to remove chain from */
)
{
	DWORD nxt;

	/*循环搜素目标簇号的整个簇链，并将其清零*/
	while ((nxt = get_cluster(clust)) >= 2) {
		if (!put_cluster(clust, 0)) return FALSE;
		clust = nxt;
	}
	return TRUE;
}
#endif



/*-----------------------------------*/
/* Stretch or Create a Cluster Chain */

//扩展或创建一条新的簇链
//若clust = 0；创建一条新的簇链
//若clust为簇链中的最后一个簇，则在末尾追加一个簇
//若clust不是簇链中的最后一个，则直接返回下一个簇号
#ifndef _FS_READONLY
static
DWORD create_chain (
	DWORD clust			 /* Cluster# to stretch, 0 means create new */
)
{
	//ncl: new cluster,     新的簇号，也是要返回的簇号；
	//ccl: current cluster, 临时保存用的簇号；
	//mcl: max cluster,     簇号上限值
	DWORD ncl, ccl, mcl = FatFs->max_clust;


	//如果是创建一条新的簇链
	if (clust == 0) {	/* Create new chain */
		ncl = 1;
		//从第2簇开始搜索第1个等于0的簇，并将其簇号记录于ncl中
		do {
			ncl++;						/* Check next cluster */
			if (ncl >= mcl) return 0;	/* No free custer was found */
			ccl = get_cluster(ncl);		/* Get the cluster status */
			if (ccl == 1) return 0;		/* Any error occured */
		} while (ccl);				/* Repeat until find a free cluster */
	}
	//如果是扩展簇链
	else {				/* Stretch existing chain */
        
		//如果clust不是簇链中的最后一个簇，则直接返回下一个簇号
		ncl = get_cluster(clust);	/* Check the cluster status */
		if (ncl < 2) return 0;		/* It is an invalid cluster */
		if (ncl < mcl) return ncl;	/* It is already followed by next cluster */

		//从clust指定的簇号开始，按簇号递增顺序搜索FAT表中第一个等于0的簇，并将其簇号记录于ncl中
		ncl = clust;				/* Search free cluster */
		do {
			ncl++;						/* Check next cluster */
			//如果已经搜索到最后1个簇还没搜到的话，则从最开始即第2簇开始搜索
			if (ncl >= mcl) ncl = 2;	/* Wrap around */
			
			//如果搜遍了整个FAT表也没搜到的话，那只好返回0了，表示没有空闲簇可分配
			if (ncl == clust) return 0;	/* No free custer was found */
			
			ccl = get_cluster(ncl);		/* Get the cluster status */
			if (ccl == 1) return 0;		/* Any error occured */
		} while (ccl);				/* Repeat until find a free cluster */
	}

	//标记新的簇号已被占用
	if (!put_cluster(ncl, 0xFFFFFFFF)) return 0;		/* Mark the new cluster "in use" */

	//如果是扩展簇链，那么还要将新的簇链嵌入到clust中去
	if (clust && !put_cluster(clust, ncl)) return 0;	/* Link it to previous one if needed */

	return ncl;		/* Return new cluster number */
}
#endif



/*----------------------------*/
/* Get Sector# from Cluster#  */

//簇号转换成对应的起始扇区号
static
DWORD clust2sect (
	DWORD clust		/* Cluster# to be converted */
)
{
	FATFS *fs = FatFs;

	//因为簇号是从2开始算起的，所以要先减去个2
	clust -= 2;
	if (clust >= fs->max_clust) return 0;		/* Invalid cluster# */

	//文件簇位于数据区，所以要加上数据区的偏移扇区
	return clust * fs->sects_clust + fs->database;
}



/*------------------------*/
/* Check File System Type */

//检查文件系统类型，参数sect一般为DBR所在扇区号
//0:无效文件系统
// 1:FAT16文件系统
// 2:FAT32文件系统
static
BYTE check_fs (
	DWORD sect		/* Sector# to check if it is a FAT boot record or not */
)
{
	static const char fatsign[] = "FAT16FAT32";
	FATFS *fs = FatFs;

	//首先将FATFS对象的扇区缓冲区清零
	memset(fs->win, 0, 512);

	//然后读取指定扇区内容到缓冲区中
	if (disk_read(fs->win, sect, 1) == RES_OK) {	/* Load boot record */

		//检查最后2字节是否为'0x55' '0xAA'
		if (LD_WORD(&(fs->win[510])) == 0xAA55) {		/* Is it valid? */

			//检查是否为FAT16文件系统，通过是否存在"FAT16"字符来判断
			if (!memcmp(&(fs->win[0x36]), &fatsign[0], 5))
				return FS_FAT16;

			//检查是否为FAT32文件系统，通过是否存在"FAT32"字符来判断
			if (!memcmp(&(fs->win[0x52]), &fatsign[5], 5) && (fs->win[0x28] == 0))
				return FS_FAT32;
		}
	}

	//无效则返回0
	return 0;
}



/*--------------------------------*/
/* Move Directory Pointer to Next */

static
BOOL next_dir_entry (
	DIR *scan			/* Pointer to directory object */
)
{
	DWORD clust;
	WORD idx;
	FATFS *fs = FatFs;


	idx = scan->index + 1;
	if ((idx & 15) == 0) {		/* Table sector changed? */
		scan->sect++;			/* Next sector */
		if (!scan->clust) {		/* In static table */
			if (idx >= fs->n_rootdir) return FALSE;	/* Reached to end of table */
		} else {				/* In dynamic table */
			if (((idx / 16) & (fs->sects_clust - 1)) == 0) {	/* Cluster changed? */
				clust = get_cluster(scan->clust);		/* Get next cluster */
				if ((clust >= fs->max_clust) || (clust < 2))	/* Reached to end of table */
					return FALSE;
				scan->clust = clust;				/* Initialize for new cluster */
				scan->sect = clust2sect(clust);
			}
		}
	}
	scan->index = idx;	/* Lower 4 bit of scan->index indicates offset in scan->sect */
	return TRUE;
}



/*--------------------------------------*/
/* Get File Status from Directory Entry */

static
void get_fileinfo (
	FILINFO *finfo, 	/* Ptr to Store the File Information */
	const BYTE *dir		/* Ptr to the Directory Entry */
)
{
	BYTE n, c, a;
	char *p;


	p = &(finfo->fname[0]);
	a = *(dir+12);	/* NT flag */
	for (n = 0; n < 8; n++) {	/* Convert file name (body) */
		c = *(dir+n);
		if (c == ' ') break;
		if (c == 0x05) c = 0xE5;
		if ((a & 0x08) && (c >= 'A') && (c <= 'Z')) c += 0x20;
		*p++ = c;
	}
	if (*(dir+8) != ' ') {		/* Convert file name (extension) */
		*p++ = '.';
		for (n = 8; n < 11; n++) {
			c = *(dir+n);
			if (c == ' ') break;
			if ((a & 0x10) && (c >= 'A') && (c <= 'Z')) c += 0x20;
			*p++ = c;
		}
	}
	*p = '\0';

	finfo->fattrib = *(dir+11);			/* Attribute */
	finfo->fsize = LD_DWORD(dir+28);	/* Size */
	finfo->fdate = LD_WORD(dir+24);		/* Date */
	finfo->ftime = LD_WORD(dir+22);		/* Time */
}



/*-------------------------------------------------------------------*/
/* Pick a Paragraph and Create the Name in Format of Directory Entry */

static
char make_dirfile (
	const char **path,		/* Pointer to the file path pointer */
	char *dirname			/* Pointer to directory name buffer {Name(8), Ext(3), NT flag(1)} */
)
{
	BYTE n, t, c, a, b;


	memset(dirname, ' ', 8+3);	/* Fill buffer with spaces */
	a = 0; b = 0x18;	/* NT flag */
	n = 0; t = 8;
	for (;;) {
		c = *(*path)++;
		if (c <= ' ') c = 0;
		if ((c == 0) || (c == '/')) {			/* Reached to end of str or directory separator */
			if (n == 0) break;
			dirname[11] = a & b; return c;
		}
		if (c == '.') {
			if(!(a & 1) && (n >= 1) && (n <= 8)) {	/* Enter extension part */
				n = 8; t = 11; continue;
			}
			break;
		}
#ifdef _USE_SJIS
		if (((c >= 0x81) && (c <= 0x9F)) ||		/* Accept S-JIS code */
		    ((c >= 0xE0) && (c <= 0xFC))) {
			if ((n == 0) && (c == 0xE5))		/* Change heading \xE5 to \x05 */
				c = 0x05;
			a ^= 1; goto md_l2;
		}
		if ((c >= 0x7F) && (c <= 0x80)) break;	/* Reject \x7F \x80 */
#else
		if (c >= 0x7F) goto md_l1;				/* Accept \x7F-0xFF */
#endif
		if (c == '"') break;					/* Reject " */
		if (c <= ')') goto md_l1;				/* Accept ! # $ % & ' ( ) */
		if (c <= ',') break;					/* Reject * + , */
		if (c <= '9') goto md_l1;				/* Accept - 0-9 */
		if (c <= '?') break;					/* Reject : ; < = > ? */
		if (!(a & 1)) {	/* These checks are not applied to S-JIS 2nd byte */
			if (c == '|') break;				/* Reject | */
			if ((c >= '[') && (c <= ']')) break;/* Reject [ \ ] */
			if ((c >= 'A') && (c <= 'Z'))
				(t == 8) ? (b &= ~0x08) : (b &= ~0x10);
			if ((c >= 'a') && (c <= 'z')) {		/* Convert to upper case */
				c -= 0x20;
				(t == 8) ? (a |= 0x08) : (a |= 0x10);
			}
		}
	md_l1:
		a &= ~1;
	md_l2:
		if (n >= t) break;
		dirname[n++] = c;
	}
	return 1;
}



/*-------------------*/
/* Trace a File Path */

/*查找符合文件名的目录项，并填充scan结构体，同时返回目录项在当前WIN[]缓冲区中的偏移地址到dir*/
/*该函数只有1个输入参数path，3个输出参数scan,fn,dir；
path:用来指定要查找的文件名(全路径名)

scan:用来返回找到的目录项结构体
fn:用来返回标准的8.3格式文件名
dir:用来返回目录项在当前WIN[]缓冲区中的字节偏移量*/
static
FRESULT trace_path (
	DIR *scan,			/* Pointer to directory object to return last directory */
	char *fn,			/* Pointer to last segment name to return */
	const char *path,	/* Full-path string to trace a file or directory */
	BYTE **dir			/* Directory pointer in Win[] to retutn */
)
{
	DWORD clust;
	char ds;
	BYTE *dptr = NULL;
	FATFS *fs = FatFs;

	/*其实目录项就4项内容:
		目录项所在的起始簇号sclust
		目录项所在的当前簇号clust
		目录项所在的当前扇区号sect
		目录项在当前扇区中的索引号index(0~15)
	  只要找到这4项内容就可以了。
	*/
	
	/*首先初始化目录项的基本参数*/
	/* Initialize directory object */
	clust = fs->dirbase;
	if (fs->fs_type == FS_FAT32) {
        //对于FAT32来讲，dirbase为根目录起始簇号；
		scan->clust = scan->sclust = clust;
		scan->sect = clust2sect(clust);
	} else {
        //但对于FAT16来讲，dirbase为根目录起始扇区号*/
		scan->clust = scan->sclust = 0;
		scan->sect = clust;
	}
	scan->index = 0;

	//去掉开头的空格符和目录分隔符'/'
	while ((*path == ' ') || (*path == '/')) path++;	/* Skip leading spaces */

	//不明白这句什么意思
	if ((BYTE)*path < ' ') {							/* Null path means the root directory */
		*dir = NULL; return FR_OK;
	}

	//从根目录开始，逐层查找文件
	for (;;) {
		//每次从路径中截取最上层的目录名到fn中，并格式化成8.3格式
		ds = make_dirfile(&path, fn);			/* Get a paragraph into fn[] */
		if (ds == 1) return FR_INVALID_NAME;

		//在扇区中，依次查找符合fn的目录项
		for (;;) {
			if (!move_window(scan->sect)) return FR_RW_ERROR;
			dptr = &(fs->win[(scan->index & 15) * 32]);	/* Pointer to the directory entry */
			if (*dptr == 0)								/* Has it reached to end of dir? */
				return !ds ? FR_NO_FILE : FR_NO_PATH;
			if (    (*dptr != 0xE5)						/* Matched? */
				&& !(*(dptr+11) & AM_VOL)
				&& !memcmp(dptr, fn, 8+3) ) break;
			if (!next_dir_entry(scan))					/* Next directory pointer */
				return !ds ? FR_NO_FILE : FR_NO_PATH;
		}
		if (!ds) { *dir = dptr; return FR_OK; }			/* Matched with end of path */
		if (!(*(dptr+11) & AM_DIR)) return FR_NO_PATH;	/* Cannot trace because it is a file */
		clust = ((DWORD)LD_WORD(dptr+20) << 16) | LD_WORD(dptr+26); /* Get cluster# of the directory */
		scan->clust = scan->sclust = clust;				/* Restart scan with the new directory */
		scan->sect = clust2sect(clust);
		scan->index = 0;
	}
}



/*---------------------------*/
/* Reserve a Directory Entry */

#ifndef _FS_READONLY
static
BYTE* reserve_direntry (
	DIR *scan			/* Target directory to create new entry */
)
{
	DWORD clust, sector;
	BYTE c, n, *dptr;
	FATFS *fs = FatFs;


	/* Re-initialize directory object */
	clust = scan->sclust;
	if (clust) {	/* Dyanmic directory table */
		scan->clust = clust;
		scan->sect = clust2sect(clust);
	} else {		/* Static directory table */
		scan->sect = fs->dirbase;
	}
	scan->index = 0;

	do {
		if (!move_window(scan->sect)) return NULL;
		dptr = &(fs->win[(scan->index & 15) * 32]);		/* Pointer to the directory entry */
		c = *dptr;
		if ((c == 0) || (c == 0xE5)) return dptr;		/* Found an empty entry! */
	} while (next_dir_entry(scan));						/* Next directory pointer */
	/* Reached to end of the directory table */

	/* Abort when static table or could not stretch dynamic table */
	if ((!clust) || !(clust = create_chain(scan->clust))) return NULL;
	if (!move_window(0)) return 0;

	fs->winsect = sector = clust2sect(clust);			/* Cleanup the expanded table */
	memset(fs->win, 0, 512);
	for (n = fs->sects_clust; n; n--) {
		if (disk_write(fs->win, sector, 1) != RES_OK) return NULL;
		sector++;
	}
	fs->dirtyflag = 1;
	return fs->win;
}
#endif



/*-----------------------------------------*/
/* Make Sure that the File System is Valid */

//实际就是调用f_mountdrv函数
static
FRESULT check_mounted ()
{
	FATFS *fs = FatFs;


	if (!fs) return FR_NOT_ENABLED;		/* Has the FatFs been enabled? */

	if (disk_status() & STA_NOINIT) {	/* The drive has not been initialized */
		if (fs->files)					/* Drive was uninitialized with any file left opend */
			return FR_INCORRECT_DISK_CHANGE;
		else
			return f_mountdrv();;		/* Initialize file system and return resulut */
	} else {							/* The drive has been initialized */
		if (!fs->fs_type)				/* But the file system has not been initialized */
			return f_mountdrv();		/* Initialize file system and return resulut */
	}
	return FR_OK;						/* File system is valid */
}





/*--------------------------------------------------------------------------*/
/* Public Funciotns                                                         */
/*--------------------------------------------------------------------------*/


/*----------------------------------------------------------*/
/* Load File System Information and Initialize FatFs Module */
//本函数做三件事:
// 1.初始化SD卡
// 2.检查文件系统类型，FAT16还是FAT32
// 3.填充FatFs对象，即记录物理磁盘的相关参数
FRESULT f_mountdrv ()
{
	BYTE fat;
	DWORD sect, fatend;
	FATFS *fs = FatFs;


	if (!fs) return FR_NOT_ENABLED;

	//首先对文件系统对象清空
	/* Initialize file system object */
	memset(fs, 0, sizeof(FATFS));

	//然后初始化SD卡
	/* Initialize disk drive */
	if (disk_initialize() & STA_NOINIT)	return FR_NOT_READY;

	//接着收搜索DBR系统引导记录，先检查第0扇区是否就是DBR(无MBR的SD卡)，如果是则检查文件系统的类型；
	//如果不是则说明第0扇区是MBR，则根据MBR中的信息定位到DBR所在扇区，并检查该文件系统的类型
	/* Search FAT partition */
	fat = check_fs(sect = 0);		/* Check sector 0 as an SFD format */
	if (!fat) {						/* Not a FAT boot record, it will be an FDISK format */
		/* Check a pri-partition listed in top of the partition table */
		if (fs->win[0x1C2]) {					/* Is the partition existing? */
			sect = LD_DWORD(&(fs->win[0x1C6]));	/* Partition offset in LBA */
			fat = check_fs(sect);				/* Check the partition */
		}
	}
	if (!fat) return FR_NO_FILESYSTEM;	/* No FAT patition */

	//初始化文件系统对象，根据DBR参数信息对Fs成员赋值
	/* Initialize file system object */

	//文件系统类型:FAT16/FAT32
	fs->fs_type = fat;								/* FAT type */

	//单个FAT表所占的扇区数
	fs->sects_fat = 								/* Sectors per FAT */
		(fat == FS_FAT32) ? LD_DWORD(&(fs->win[0x24])) : LD_WORD(&(fs->win[0x16]));

	//单个簇所占的扇区数
	fs->sects_clust = fs->win[0x0D];				/* Sectors per cluster */

	//FAT表总数
	fs->n_fats = fs->win[0x10];						/* Number of FAT copies */

	//FAT表起始扇区(物理扇区)
	fs->fatbase = sect + LD_WORD(&(fs->win[0x0E]));	/* FAT start sector (physical) */

	//根目录项数
	fs->n_rootdir = LD_WORD(&(fs->win[0x11]));		/* Nmuber of root directory entries */

	//计算根目录起始扇区、数据起始扇区(物理扇区地址)
	fatend = fs->sects_fat * fs->n_fats + fs->fatbase;
	if (fat == FS_FAT32) {
		fs->dirbase = LD_DWORD(&(fs->win[0x2C]));	/* Directory start cluster */
		fs->database = fatend;	 					/* Data start sector (physical) */
	} else {
		fs->dirbase = fatend;						/* Directory start sector (physical) */
		fs->database = fs->n_rootdir / 16 + fatend;	/* Data start sector (physical) */
	}

	//最大簇号
	fs->max_clust = 								/* Maximum cluster number */
		(LD_DWORD(&(fs->win[0x20])) - fs->database + sect) / fs->sects_clust + 2;

	return FR_OK;
}



/*-----------------------------*/
/* Get Number of Free Clusters */

FRESULT f_getfree (
	DWORD *nclust		/* Pointer to the double word to return number of free clusters */
)
{
	DWORD n, clust, sect;
	BYTE m, *ptr, fat;
	FRESULT res;
	FATFS *fs = FatFs;


	if ((res = check_mounted()) != FR_OK) return res;
	fat = fs->fs_type;

	/* Count number of free clusters */
	n = m = clust = 0;
	ptr = NULL;
	sect = fs->fatbase;
	do {
		if (m == 0) {
			if (!move_window(sect++)) return FR_RW_ERROR;
			ptr = fs->win;
		}
		if (fat == FS_FAT32) {
			if (LD_DWORD(ptr) == 0) n++;
			ptr += 4; m += 2;
		} else {
			if (LD_WORD(ptr) == 0) n++;
			ptr += 2; m++;
		}
		clust++;
	} while (clust < fs->max_clust);

	*nclust = n;
	return FR_OK;
}



/*-----------------------*/
/* Open or Create a File */

FRESULT f_open (
	FIL *fp,			/* Pointer to the buffer of new file object to create */
	const char *path,	/* Pointer to the file path */
	BYTE mode			/* Access mode and file open mode flags */
)
{
	FRESULT res;
	BYTE *dir;
	DIR dirscan;
	char fn[8+3+1];			//8.3 DOS文件名
	FATFS *fs = FatFs;

	/*首先初始化SD卡，检测文件系统类型，初始化FATFS对象*/
	if ((res = check_mounted()) != FR_OK) return res;
	
#ifndef _FS_READONLY

	//如果磁盘设置为写保护，则返回错误码:FR_WRITE_PROTECTED
	if ((mode & (FA_WRITE|FA_CREATE_ALWAYS)) && (disk_status() & STA_PROTECT))
		return FR_WRITE_PROTECTED;
#endif

	//根据用户提供的文件路径path，将文件名对应的目录项及其整个扇区读取到win[]中，
	//并填充目录项dirscan、标准格式的文件名fn，以及目录项在win[]中的字节偏移量dir
	res = trace_path(&dirscan, fn, path, &dir);	/* Trace the file path */

#ifndef _FS_READONLY
	/* Create or Open a File */
	if (mode & (FA_CREATE_ALWAYS|FA_OPEN_ALWAYS)) {
		DWORD dw;

        //如果文件不存在，则强制新建文件
		if (res != FR_OK) {		/* No file, create new */
			mode |= FA_CREATE_ALWAYS;
			if (res != FR_NO_FILE) return res;
			dir = reserve_direntry(&dirscan);	/* Reserve a directory entry */
			if (dir == NULL) return FR_DENIED;
			memcpy(dir, fn, 8+3);		/* Initialize the new entry */
			*(dir+12) = fn[11];
			memset(dir+13, 0, 32-13);
		} 
        else {				/* File already exists */
			if ((dir == NULL) || (*(dir+11) & (AM_RDO|AM_DIR)))	/* Could not overwrite (R/O or DIR) */
				return FR_DENIED;
            
            //如果文件存在，但又以FA_CREATE_ALWAYS方式打开文件，则重写文件
			if (mode & FA_CREATE_ALWAYS) {	/* Resize it to zero */
				dw = fs->winsect;			/* Remove the cluster chain */
				if (!remove_chain(((DWORD)LD_WORD(dir+20) << 16) | LD_WORD(dir+26))
					|| !move_window(dw) )
					return FR_RW_ERROR;
				ST_WORD(dir+20, 0); ST_WORD(dir+26, 0);	/* cluster = 0 */
				ST_DWORD(dir+28, 0);					/* size = 0 */
			}
		}

		//如果是强制新建文件操作，则还需更新时间和日期
		if (mode & FA_CREATE_ALWAYS) {
			*(dir+11) = AM_ARC;
			dw = get_fattime();
			ST_DWORD(dir+14, dw);	/* Created time */
			ST_DWORD(dir+22, dw);	/* Updated time */
			fs->dirtyflag = 1;
		}
	}
	/* Open a File */
	else {
#endif
		if (res != FR_OK) return res;		/* Trace failed */

		//如果打开的是一个目录文件，则返回错误码:FR_NO_FILE
		if ((dir == NULL) || (*(dir+11) & AM_DIR))	/* It is a directory */
			return FR_NO_FILE;
		
#ifndef _FS_READONLY

		//如果以FA_WRITE方式打开Read-Only属性的文件，则返回错误码:FR_DENIED
		if ((mode & FA_WRITE) && (*(dir+11) & AM_RDO)) /* R/O violation */
			return FR_DENIED;
	}
#endif

	//填充FIL文件结构体参数
#ifdef _FS_READONLY
	fp->flag = mode & (FA_UNBUFFERED|FA_READ);
#else
	fp->flag = mode & (FA_UNBUFFERED|FA_WRITE|FA_READ);
	fp->dir_sect = fs->winsect;				/* Pointer to the directory entry */
	fp->dir_ptr = dir;
#endif
	fp->org_clust =	((DWORD)LD_WORD(dir+20) << 16) | LD_WORD(dir+26);	/* File start cluster */
	fp->fsize = LD_DWORD(dir+28);		/* File size */
	fp->fptr = 0;						/* File ptr */

    //这一步很重要，它将直接导致f_read和f_write操作中的逻辑顺序
	fp->sect_clust = 1;					/* Sector counter */
    
	fs->files++;
	return FR_OK;
}



/*-----------*/
/* Read File */
//文件读操作
FRESULT f_read (
	FIL *fp, 		/* Pointer to the file object */
	BYTE *buff,		/* Pointer to data buffer */
	WORD btr,		/* Number of bytes to read */
	WORD *br		/* Pointer to number of bytes read */
)
{
	DWORD clust, sect, ln;
	WORD rcnt;		/*已经读取的字节数*/
	BYTE cc;		/*实际单次要读取的连续最大扇区数*/
	FATFS *fs = FatFs;


	*br = 0;
    
    //错误处理
	if (!fs) return FR_NOT_ENABLED;
	if ((disk_status() & STA_NOINIT) || !fs->fs_type) return FR_NOT_READY;	/* Check disk ready */
	if (fp->flag & FA__ERROR) return FR_RW_ERROR;	/* Check error flag */
	if (!(fp->flag & FA_READ)) return FR_DENIED;	/* Check access mode */
	
	//检查要读取的字节数是否超出了剩余的文件长度，如果超出了，则只读取剩余字节长度
	ln = fp->fsize - fp->fptr;
	if (btr > ln) btr = ln;							/* Truncate read count by number of bytes left */

    //该循环以簇为单位，每循环一次就读完一个簇的内容
	/* Repeat until all data transferred */
	for ( ;  btr; buff += rcnt, fp->fptr += rcnt, *br += rcnt, btr -= rcnt) 
	{
			
        //当文件指针与扇区边界对齐时(如:刚打开文件时)，执行以下操作；
        //否则(如:调用了f_lseek函数)只将当前buffer中需要的内容直接copy到目标缓冲区，然后进入下一次循环。
		if ((fp->fptr & 511) == 0) 					/* On the sector boundary */
		{				
            //如果还没有读完当前簇的所有扇区，则将下一个扇区号赋给变量sect。
			//注意，当第一次读取文件时，由于f_open函数中，已经将sect_clust赋值为1，
			//所以应该执行else语句
			if (--(fp->sect_clust)) 				/* Decrement sector counter */
			{	
                //一般走到这里就意味着只剩下最后一个扇区需要读取，且要读取的内容小于512字节
				sect = fp->curr_sect + 1;			/* Next sector */
			} 
			//如果已经读完当前簇的所有扇区，则计算下一个簇的起始位置，
			//并更新FIL中的当前簇号curr_clust和当前簇剩余扇区数sect_clust
			else 									/* Next cluster */
			{	
                //如果当前文件指针在起始簇内，则将clust设置为起始簇号；
                //否则，将clust设置为下一簇号
				clust = (fp->fptr == 0) ? fp->org_clust : get_cluster(fp->curr_clust);
				if ((clust < 2) || (clust >= fs->max_clust)) goto fr_error;
                
				fp->curr_clust = clust;				/* Current cluster */
				sect = clust2sect(clust);			/* Current sector */
				fp->sect_clust = fs->sects_clust;	/* Re-initialize the sector counter */
			}
			
#ifndef _FS_READONLY

			//如果buffer中的内容被修改过，则需要先将buffer中的内容回写到物理磁盘中
			if (fp->flag & FA__DIRTY) 				/* Flush file I/O buffer if needed */
			{				
				if (disk_write(fp->buffer, fp->curr_sect, 1) != RES_OK) goto fr_error;
				fp->flag &= ~FA__DIRTY;
			}
#endif

			//更新当前扇区号
			fp->curr_sect = sect;					/* Update current sector */

			//计算需要读取部分的剩余扇区数cc(最后一个扇区内容若不满512字节则不计入cc中)
			cc = btr / 512;							

			//只要当前扇区不是最后一个扇区，则执行连续的读操作，然后直接进入下一次循环
			/* When left bytes >= 512 */
            /* Read maximum contiguous sectors */
			if (cc) 								
			{	
                //如果cc小于当前簇的剩余扇区数sect_clust，则连续读取cc个扇区；
                //如果cc大于当前簇的剩余扇区数sect_clust，则只读取当前簇的剩余扇区
				if (cc > fp->sect_clust) cc = fp->sect_clust;   

				//执行实际的磁盘读操作，读取当前簇的剩余扇区内容到目标缓冲区中
				//注意，是直接读到目标接收缓冲区的，而不是到buffer中
				if (disk_read(buff, sect, cc) != RES_OK) goto fr_error;

				//更新当前簇的剩余扇区数
				//该语句实际为:sect_clust = sect_clust - (cc - 1) = sect_clust - cc + 1;
				//之所以+1是因为当cc == sect_clust时，sect_clust = sect_clust - sect_clust + 1 = 1；
				//所以当下一次循环执行到 if (--(fp->sect_clust)) 时直接进入else语句。
				fp->sect_clust -= cc - 1;
				
				//更新当前扇区号，扇区号是基于0索引的，所以这里要-1
				fp->curr_sect += cc - 1;

				//更新已读的字节数
				rcnt = cc * 512; 

                //直接进入下一次循环
                continue;
			}

			if (fp->flag & FA_UNBUFFERED)			/* Reject unaligned access when unbuffered mode */
				return FR_ALIGN_ERROR;

			//对于文件的最后一个扇区，则先将内容读取到buffer中，然后将需要的内容拷贝到目标缓冲区中，并退出循环
			if (disk_read(fp->buffer, sect, 1) != RES_OK)	/* Load the sector into file I/O buffer */
				goto fr_error;
            
		}//end if ((fp->fptr & 511) == 0)

		//计算从buffer中实际要读取的字节数rcnt
		rcnt = 512 - (fp->fptr & 511);				
		if (rcnt > btr) rcnt = btr;

		//最后将buffer中的指定数据拷贝到目标缓冲区中
		memcpy(buff, &fp->buffer[fp->fptr & 511], rcnt);    /* Copy fractional bytes from file I/O buffer */

        //一般走到这里就说明已经读完了最后一扇区，下一步将退出循环;
        //如果还没有读完最后一扇，则有可能是在调用f_lseek后第一次调用f_read，那么将进入下一次循环
	}//end for(...)

	return FR_OK;

fr_error:	/* Abort this file due to an unrecoverable error */
	fp->flag |= FA__ERROR;
	return FR_RW_ERROR;
}



/*------------*/
/* Write File */

#ifndef _FS_READONLY
FRESULT f_write (
	FIL *fp,			/* Pointer to the file object */
	const BYTE *buff,	/* Pointer to the data to be written */
	WORD btw,			/* Number of bytes to write */
	WORD *bw			/* Pointer to number of bytes written */
)
{
	DWORD clust, sect;
	WORD wcnt;          /*已经写入的字节数*/
	BYTE cc;            /*实际单次要写入的连续最大扇区数*/
	FATFS *fs = FatFs;


	*bw = 0;

    //错误处理
	if (!fs) return FR_NOT_ENABLED;
	if ((disk_status() & STA_NOINIT) || !fs->fs_type) return FR_NOT_READY;
	if (fp->flag & FA__ERROR) return FR_RW_ERROR;	/* Check error flag */
	if (!(fp->flag & FA_WRITE)) return FR_DENIED;	/* Check access mode */
    
    //保证写入后整个文件大小不得超过4GB
	if (fp->fsize + btw < fp->fsize) btw = 0;		/* File size cannot reach 4GB */

    //该循环以簇为单位，每循环一次就写完一个簇的内容
    /* Repeat until all data transferred */
	for ( ;  btw; buff += wcnt, fp->fptr += wcnt, *bw += wcnt, btw -= wcnt) 
	{
        //当文件指针与扇区边界对齐时(如:刚打开文件时)，执行以下操作；
        //否则(如:调用了f_lseek函数)只将当前扇区所需要的内容copy到buffer中，然后进入下一次循环。
		if ((fp->fptr & 511) == 0)                  /* On the sector boundary */
        {               
            //如果还没有写完当前簇的所有扇区，则将下一个扇区号赋给变量sect。
			//注意，当第一次写文件时，由于f_open函数中，已经将sect_clust赋值为1，
			//所以应该执行else语句
			if (--(fp->sect_clust))                 /* Decrement sector counter */
            {               
                //一般走到这里就意味着只剩下最后一个扇区需要写入，且要写入的内容小于512字节
				sect = fp->curr_sect + 1;			/* Next sector */
			} 
            //如果已经写完当前簇的所有扇区，则计算下一个簇的起始位置，
            //并更新FIL中的当前簇号curr_clust和当前簇剩余扇区数sect_clust
            else                                    /* Next cluster */
            {   
                //如果当前文件指针在起始簇内，则将当前簇设置为起始簇；
				if (fp->fptr == 0)                  /* Top of the file */
                {               
					clust = fp->org_clust;
                    
                    //如果文件不存在，则先创建一个新的簇，并将该簇号设置为当前簇号
					if (clust == 0)					/* No cluster is created */
						fp->org_clust = clust = create_chain(0);	/* Create a new cluster chain */
				} 
                //如果当前文件指针不在起始簇内，则将下一簇号设置为当前簇号
                else                                /* Middle or end of file */
                {                           
					clust = create_chain(fp->curr_clust);			/* Trace or streach cluster chain */
				}
				if ((clust < 2) || (clust >= fs->max_clust)) break;
                
				fp->curr_clust = clust;				/* Current cluster */
				sect = clust2sect(clust);			/* Current sector */
				fp->sect_clust = fs->sects_clust;	/* Re-initialize the sector counter */
			}
            
			//如果buffer中的内容被修改过，则需要先将buffer中的内容回写到物理磁盘中
			if (fp->flag & FA__DIRTY)               /* Flush file I/O buffer if needed */
            {               
				if (disk_write(fp->buffer, fp->curr_sect, 1) != RES_OK) goto fw_error;
				fp->flag &= ~FA__DIRTY;
			}
            
			//更新当前扇区号
			fp->curr_sect = sect;					/* Update current sector */

			//计算需要写入部分的剩余扇区数cc(最后一个扇区内容若不满512字节则不计入cc中)
            cc = btw / 512;							

			//只要当前扇区不是最后一个扇区，则执行连续的写操作，然后直接进入下一次循环
			/* When left bytes >= 512 */
            /* Write maximum contiguous sectors */
			if (cc)                                 
            {                              
                //如果cc小于当前簇的剩余扇区数sect_clust，则连续写入cc个扇区；
                //如果cc大于当前簇的剩余扇区数sect_clust，则只将当前簇的剩余扇区写入物理磁盘中
				if (cc > fp->sect_clust) cc = fp->sect_clust;
                
				//执行实际的磁盘写操作，将源缓冲区中的内容写入到当前簇的剩余扇区中
				//注意，是直接从源缓冲区写到物理磁盘中的，没有经过buffer
				if (disk_write(buff, sect, cc) != RES_OK) goto fw_error;
                
				//更新当前簇的剩余扇区数
				//该语句实际为:sect_clust = sect_clust - (cc - 1) = sect_clust - cc + 1;
				//之所以+1是因为当cc == sect_clust时，sect_clust = sect_clust - sect_clust + 1 = 1；
				//所以当下一次循环执行到 if (--(fp->sect_clust)) 时直接进入else语句。
				fp->sect_clust -= cc - 1;
                
				//更新当前扇区号，扇区号是基于0索引的，所以这里要-1
				fp->curr_sect += cc - 1;
                
				//更新已写的字节数
				wcnt = cc * 512; 

                //直接进入下一次循环
                continue;
			}
            
			if (fp->flag & FA_UNBUFFERED)			/* Reject unalighend access when unbuffered mode */
				return FR_ALIGN_ERROR;
            
            //如果已经写到最后一个扇区了，则先将SD卡中所对应扇区的原始内容读取到buffer中，然后修改buffer中的内容，再从buffer回写到物理磁盘中
			if ((fp->fptr < fp->fsize) && (disk_read(fp->buffer, sect, 1) != RES_OK)) /* Fill sector buffer with file data if needed */
					goto fw_error;
            
		}//end if ((fp->fptr & 511) == 0) 
        
		//计算实际要写入的字节数wcnt
		wcnt = 512 - (fp->fptr & 511);				/* Copy fractional bytes to file I/O buffer */
		if (wcnt > btw) wcnt = btw;
        
		//将源缓冲区中的数据拷贝到buffer中的指定位置
		memcpy(&fp->buffer[fp->fptr & 511], buff, wcnt);

        //设置buffer回写标记，当调用f_close或执行下一次循环操作时，执行buffer回写操作
		fp->flag |= FA__DIRTY;
        
        //一般走到这里就说明已经写完了最后一扇区，下一步将退出循环;
        //如果还没有写完最后一扇，则有可能是在调用f_lseek后第一次调用f_write，那么将进入下一次循环
	}//end for(; btw; buff += wcnt,...)

    //如果对原始文件增加了新的内容，则需要更新文件的大小，并设置文件大小更新标记FA__WRITTEN
	if (fp->fptr > fp->fsize) fp->fsize = fp->fptr;	/* Update file size if needed */
	fp->flag |= FA__WRITTEN;						/* Set file changed flag */
    
	return FR_OK;

fw_error:	/* Abort this file due to an unrecoverable error */
	fp->flag |= FA__ERROR;
	return FR_RW_ERROR;
}
#endif



/*-------------------*/
/* Seek File Pointer */

FRESULT f_lseek (
	FIL *fp,		/* Pointer to the file object */
	DWORD ofs		/* File pointer from top of file */
)
{
	DWORD clust;
	BYTE sc;
	FATFS *fs = FatFs;


    //错误处理
	if (!fs) return FR_NOT_ENABLED;
	if ((disk_status() & STA_NOINIT) || !fs->fs_type) return FR_NOT_READY;
	if (fp->flag & FA__ERROR) return FR_RW_ERROR;
    
#ifndef _FS_READONLY
    //如果buffer中的内容被修改过，则需要先将buffer中的内容回写到物理磁盘中
	if (fp->flag & FA__DIRTY) {			/* Write-back dirty buffer if needed */
		if (disk_write(fp->buffer, fp->curr_sect, 1) != RES_OK) goto fk_error;
		fp->flag &= ~FA__DIRTY;
	}
#endif

	if (ofs > fp->fsize) ofs = fp->fsize;	/* Clip offset by file size */
	if ((ofs & 511) && (fp->flag & FA_UNBUFFERED)) return FR_ALIGN_ERROR;

    //重新初始化文件指针
	fp->fptr = ofs;                         /* Re-initialize file pointer */
    
    fp->sect_clust = 1; 	

	/* Seek file pinter if needed */
	if (ofs) {

        //计算偏移指针ofs所在的簇号
		ofs = (ofs - 1) / 512;				/* Calcurate current sector */
		sc = fs->sects_clust;				/* Number of sectors in a cluster */
		fp->sect_clust = sc - (ofs % sc);	/* Calcurate sector counter */
		ofs /= sc;							/* Number of clusters to skip */
		clust = fp->org_clust;				/* Seek to current cluster */
		while (ofs--)
			clust = get_cluster(clust);
		if ((clust < 2) || (clust >= fs->max_clust)) goto fk_error;
		fp->curr_clust = clust;
        
        //计算偏移指针ofs所在的扇区号
		fp->curr_sect = clust2sect(clust) + sc - fp->sect_clust;	/* Current sector */

        //如果偏移指针不在扇区边界，则将指针所在的扇区内容读到buffer中，供后续f_read，f_write操作
		if (fp->fptr & 511) {										/* Load currnet sector if needed */
			if (disk_read(fp->buffer, fp->curr_sect, 1) != RES_OK)
				goto fk_error;
		}
	}

	return FR_OK;

fk_error:	/* Abort this file due to an unrecoverable error */
	fp->flag |= FA__ERROR;
	return FR_RW_ERROR;
}



/*-------------------------------------------------*/
/* Synchronize between File and Disk without Close */

//将当前操作缓冲区的数据写入物理磁盘中
#ifndef _FS_READONLY
FRESULT f_sync (
	FIL *fp		/* Pointer to the file object */
)
{
	BYTE *ptr;
	FATFS *fs = FatFs;


	if (!fs) return FR_NOT_ENABLED;
	if ((disk_status() & STA_NOINIT) || !fs->fs_type)
		return FR_INCORRECT_DISK_CHANGE;

	//检查文件是否增加了新的内容，如果是则需要修改目录项
	/* Has the file been written? */
	if (fp->flag & FA__WRITTEN) {

        //如果buffer的内容被修改了，则需要先将buffer的内容回写到物理磁盘中去
		/* Write back data buffer if needed */
		if (fp->flag & FA__DIRTY) {
			if (disk_write(fp->buffer, fp->curr_sect, 1) != RES_OK) return FR_RW_ERROR;
			fp->flag &= ~FA__DIRTY;
		}

	/*************由于更改了文件长度，所以需要修改文件对应的目录项************/

		//首先读取文件对应的根目录扇区到win[]
		/* Update the directory entry */
		if (!move_window(fp->dir_sect)) return FR_RW_ERROR;

		//然后修改根目录项
		ptr = fp->dir_ptr;
		*(ptr+11) |= AM_ARC;					/* Set archive bit */
		ST_DWORD(ptr+28, fp->fsize);			/* Update file size */
		ST_WORD(ptr+26, fp->org_clust);			/* Update start cluster */
		ST_WORD(ptr+20, fp->org_clust >> 16);
		ST_DWORD(ptr+22, get_fattime());		/* Updated time */

		//设置win[]回写标志
		fs->dirtyflag = 1;

		//清除文件大小更改标记
		fp->flag &= ~FA__WRITTEN;
	}

	//将win[]中的根目录信息回写到对应的物理扇区
	if (!move_window(0)) return FR_RW_ERROR;

	return FR_OK;
}
#endif


//关闭文件，并将最后剩余的扇区内容刷入物理磁盘中?
/*------------*/
/* Close File */

FRESULT f_close (
	FIL *fp		/* Pointer to the file object to be closed */
)
{
	FRESULT res;


#ifndef _FS_READONLY
	res = f_sync(fp);
#else
	res = FR_OK;
#endif
	if (res == FR_OK) {
		fp->flag = 0;		//文件状态标记清零
		FatFs->files--;		//文件打开次数减1
	}
	return res;
}



/*------------------------------*/
/* Delete a File or a Directory */

#ifndef _FS_READONLY
FRESULT f_unlink (
	const char *path			/* Pointer to the file or directory path */
)
{
	FRESULT res;
	BYTE *dir, *sdir;
	DWORD dclust, dsect;
	DIR dirscan;
	char fn[8+3+1];
	FATFS *fs = FatFs;


	if ((res = check_mounted()) != FR_OK) return res;
	if (disk_status() & STA_PROTECT) return FR_WRITE_PROTECTED;

	res = trace_path(&dirscan, fn, path, &dir);	/* Trace the file path */

	if (res != FR_OK) return res;				/* Trace failed */
	if (dir == NULL) return FR_NO_FILE;			/* It is a root directory */
	if (*(dir+11) & AM_RDO) return FR_DENIED;	/* It is a R/O item */
	dsect = fs->winsect;
	dclust = ((DWORD)LD_WORD(dir+20) << 16) | LD_WORD(dir+26);

	if (*(dir+11) & AM_DIR) {					/* It is a sub-directory */
		dirscan.clust = dclust;					/* Check if the sub-dir is empty or not */
		dirscan.sect = clust2sect(dclust);
		dirscan.index = 0;
		do {
			if (!move_window(dirscan.sect)) return FR_RW_ERROR;
			sdir = &(fs->win[(dirscan.index & 15) * 32]);
			if (*sdir == 0) break;
			if (!((*sdir == 0xE5) || (*sdir == '.')) && !(*(sdir+11) & AM_VOL))
				return FR_DENIED;	/* The directory is not empty */
		} while (next_dir_entry(&dirscan));
	}

	if (!remove_chain(dclust)) return FR_RW_ERROR;	/* Remove the cluster chain */
	if (!move_window(dsect)) return FR_RW_ERROR;	/* Mark the directory entry deleted */
	*dir = 0xE5; fs->dirtyflag = 1;
	if (!move_window(0)) return FR_RW_ERROR;

	return FR_OK;
}
#endif



/*--------------------*/
/* Create a Directory */

#ifndef _FS_READONLY
FRESULT f_mkdir (
	const char *path		/* Pointer to the directory path */
)
{
	FRESULT res;
	BYTE *dir, *w, n;
	DWORD sect, dsect, dclust, tim;
	DIR dirscan;
	char fn[8+3+1];
	FATFS *fs = FatFs;


	if ((res = check_mounted()) != FR_OK) return res;
	if (disk_status() & STA_PROTECT) return FR_WRITE_PROTECTED;

	res = trace_path(&dirscan, fn, path, &dir);	/* Trace the file path */

	if (res == FR_OK) return FR_DENIED;		/* Any file or directory is already existing */
	if (res != FR_NO_FILE) return res;

	dir = reserve_direntry(&dirscan);		/* Reserve a directory entry */
	if (dir == NULL) return FR_DENIED;
	sect = fs->winsect;
	dsect = clust2sect(dclust = create_chain(0));	/* Get a new cluster for new directory */
	if (!dsect) return FR_DENIED;
	if (!move_window(0)) return 0;

	w = fs->win;
	memset(w, 0, 512);						/* Initialize the directory table */
	for (n = fs->sects_clust - 1; n; n--) {
		if (disk_write(w, dsect+n, 1) != RES_OK) return FR_RW_ERROR;
	}

	fs->winsect = dsect;					/* Create dot directories */
	memset(w, ' ', 8+3);
	*w = '.';
	*(w+11) = AM_DIR;
	tim = get_fattime();
	ST_DWORD(w+22, tim);
	memcpy(w+32, w, 32); *(w+33) = '.';
	ST_WORD(w+26, dclust);
	ST_WORD(w+20, dclust >> 16);
	ST_WORD(w+32+26, dirscan.sclust);
	ST_WORD(w+32+20, dirscan.sclust >> 16);
	fs->dirtyflag = 1;

	if (!move_window(sect)) return FR_RW_ERROR;
	memcpy(dir, fn, 8+3);			/* Initialize the new entry */
	*(dir+11) = AM_DIR;
	*(dir+12) = fn[11];
	memset(dir+13, 0, 32-13);
	ST_DWORD(dir+22, tim);			/* Crated time */
	ST_WORD(dir+26, dclust);		/* Table start cluster */
	ST_WORD(dir+20, dclust >> 16);
	fs->dirtyflag = 1;

	if (!move_window(0)) return FR_RW_ERROR;

	return FR_OK;
}
#endif



/*---------------------------*/
/* Initialize directroy scan */

FRESULT f_opendir (
	DIR *scan,			/* Pointer to directory object to initialize */
	const char *path	/* Pointer to the directory path, null str means the root */
)
{
	FRESULT res;
	BYTE *dir;
	char fn[8+3+1];


	if ((res = check_mounted()) != FR_OK) return res;

	res = trace_path(scan, fn, path, &dir);	/* Trace the directory path */

	if (res == FR_OK) {						/* Trace completed */
		if (dir != NULL) {					/* It is not a root dir */
			if (*(dir+11) & AM_DIR) {		/* The entry is a directory */
				scan->clust = ((DWORD)LD_WORD(dir+20) << 16) | LD_WORD(dir+26);
				scan->sect = clust2sect(scan->clust);
				scan->index = 0;
			} else {						/* The entry is a file */
				res = FR_NO_PATH;
			}
		}
	}
	return res;
}



/*----------------------------------*/
/* Read Directory Entry in Sequense */

FRESULT f_readdir (
	DIR *scan,			/* Pointer to the directory object */
	FILINFO *finfo		/* Pointer to file information to return */
)
{
	BYTE *dir, c;
	FATFS *fs = FatFs;


	if (!fs) return FR_NOT_ENABLED;
	finfo->fname[0] = 0;
	if ((disk_status() & STA_NOINIT) || !fs->fs_type) return FR_NOT_READY;

	while (scan->sect) {
		if (!move_window(scan->sect)) return FR_RW_ERROR;
		dir = &(fs->win[(scan->index & 15) * 32]);		/* pointer to the directory entry */
		c = *dir;
		if (c == 0) break;								/* Has it reached to end of dir? */
		if ((c != 0xE5) && (c != '.') && !(*(dir+11) & AM_VOL))	/* Is it a valid entry? */
			get_fileinfo(finfo, dir);
		if (!next_dir_entry(scan)) scan->sect = 0;		/* Next entry */
		if (finfo->fname[0]) break;						/* Found valid entry */
	}

	return FR_OK;
}



/*-----------------*/
/* Get File Status */

FRESULT f_stat (
	const char *path,	/* Pointer to the file path */
	FILINFO *finfo		/* Pointer to file information to return */
)
{
	FRESULT res;
	BYTE *dir;
	DIR dirscan;
	char fn[8+3+1];


	if ((res = check_mounted()) != FR_OK) return res;

	res = trace_path(&dirscan, fn, path, &dir);		/* Trace the file path */

	if (res == FR_OK)								/* Trace completed */
		get_fileinfo(finfo, dir);

	return res;
}



/*-----------------------*/
/* Change File Attribute */

#ifndef _FS_READONLY
FRESULT f_chmod (
	const char *path,	/* Pointer to the file path */
	BYTE value,			/* Attribute bits */
	BYTE mask			/* Attribute mask to change */
)
{
	FRESULT res;
	BYTE *dir;
	DIR dirscan;
	char fn[8+3+1];
	FATFS *fs = FatFs;


	if ((res = check_mounted()) != FR_OK) return res;
	if (disk_status() & STA_PROTECT) return FR_WRITE_PROTECTED;

	res = trace_path(&dirscan, fn, path, &dir);	/* Trace the file path */

	if (res == FR_OK) {						/* Trace completed */
		if (dir == NULL) {
			res = FR_NO_FILE;
		} else {
			mask &= AM_RDO|AM_HID|AM_SYS|AM_ARC;	/* Valid attribute mask */
			*(dir+11) = (value & mask) | (*(dir+11) & ~mask);	/* Apply attribute change */
			fs->dirtyflag = 1;
			if(!move_window(0)) res = FR_RW_ERROR;
		}
	}
	return res;
}
#endif


