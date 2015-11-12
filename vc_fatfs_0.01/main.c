#include <stdio.h>
#include <string.h>

#include "diskio.h"
#include "ff.h"

void die(char *name, int res)
{
	printf("%s: %d\n", name, res);
	//exit(0);
}

void main(void)
{
    FATFS fs;            // FatFs work area
    FIL fsrc, fdst;      // file structures
    BYTE fbuff[512*2];   // file r/w buffers (not required for Tiny-FatFs)
    BYTE buffer[4096];   // file copy buffer
    FRESULT res;         // FatFs function common result code
    WORD br, bw;         // File R/W count


    // Activate FatFs module
    memset(&fs, 0, sizeof(FATFS));
    FatFs = &fs;

    // Open source file
    fsrc.buffer = fbuff+0;	// (not required for Tiny-FatFs)
    res = f_open(&fsrc, "srcdir/srcfile.dat", FA_OPEN_EXISTING | FA_READ);
    if (res) die("f_open err", res);

    // Create destination file
    fdst.buffer = fbuff+512;	// (not required for Tiny-FatFs)
	res = f_mkdir("dstdir");
	if (res) die("f_mkdir err", res);
    res = f_open(&fdst, "dstdir/dstfile.dat", FA_CREATE_ALWAYS | FA_WRITE);
    if (res) die("f_open err", res);

    // Copy source to destination
    for (;;) {
        res = f_read(&fsrc, buffer, sizeof(buffer), &br);
        if (res) die("f_read err", res);
        if (br == 0) break;
        res = f_write(&fdst, buffer, br, &bw);
        if (res) die("f_write err", res);
        if (bw < br) break;
    }

    // Close all files
    f_close(&fsrc);
    f_close(&fdst);

    // Deactivate FatFs module
    FatFs = NULL;
}