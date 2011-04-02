
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "ImageFmtc.h"

typedef unsigned long ulong;

static ulong rev_endian(ulong x, int bytes)
{
    if (bytes==1) return x;
    if (bytes==2) return ((x>>8)&0xFF) | ((x<<8)&0xFF00);
    if (bytes==3) return ((x>>16)&0xFF) | ((x)&0xFF00) | ((x<<16)&0xFF0000);
    return ((x>>24)&0xFF) | ((x>>8)&0xFF00) | ((x<<8)&0xFF0000) | ((x<<24)&0xFF000000);
}

static ulong endian2(unsigned char u1, unsigned char u2, int endian)
{
    if (endian)
      return ((u1<<8)&0xFF00) | ((u2)&0xFF);
    else
      return ((u2<<8)&0xFF00) | ((u1)&0xFF);
}

static ulong endian4(unsigned char u1, unsigned char u2,
                     unsigned char u3, unsigned char u4, int endian)
{
    if (endian)
      return ((u1<<24)&0xFF000000) | ((u2<<16)&0xFF0000)
           | ((u3<<8)&0xFF00) | ((u4)&0xFF);
    else
      return ((u4<<24)&0xFF000000) | ((u3<<16)&0xFF0000)
           | ((u2<<8)&0xFF00) | ((u1)&0xFF);
}

static int fread_endian(ulong *x, int bytes, FILE *fp, int endian)
{
    unsigned char u1, u2, u3, u4;
    if (bytes==1) {
        if (fread(&u1, 1, 1, fp) != 1) return -1;
        *x = u1;
        return 1;
    }
    else if (bytes==2) {
        if (fread(&u1, 1, 1, fp) != 1) return -1;
        if (fread(&u2, 1, 1, fp) != 1) return -1;
        *x = endian2(u1, u2, endian);
        return 2;
    }
    else if (bytes==4) {
        if (fread(&u1, 1, 1, fp) != 1) return -1;
        if (fread(&u2, 1, 1, fp) != 1) return -1;
        if (fread(&u3, 1, 1, fp) != 1) return -1;
        if (fread(&u4, 1, 1, fp) != 1) return -1;
        *x = endian4(u1, u2, u3, u4, endian);
        return 4;
    }
    else {
        *x = 0;
        return 0;
    }
}


static int bwrite_endian(ulong x, int bytes, unsigned char *buf, int endian)
{
    unsigned char u1, u2, u3, u4;
    if (bytes==1) {
        u1 = x&0xFF;
        *buf++ = u1;
        return 1;
    }
    else if (bytes==2) {
        u1 = x&0xFF;
        u2 = (x>>8)&0xFF;
        if (endian) {
            *buf++ = u1;
            *buf++ = u2;
        }
        else {
            *buf++ = u2;
            *buf++ = u1;
        }
        return 2;
    }
    else if (bytes==4) {
        u1 = x&0xFF;
        u2 = (x>>8)&0xFF;
        u3 = (x>>16)&0xFF;
        u4 = (x>>24)&0xFF;
        if (endian) {
            *buf++ = u1;
            *buf++ = u2;
            *buf++ = u3;
            *buf++ = u4;
        }
        else {
            *buf++ = u4;
            *buf++ = u3;
            *buf++ = u2;
            *buf++ = u1;
        }
        return 4;
    }
    else {
        return -1;
    }
}


/* Attempt to load an RGB BMP file. */
int ReadBMP(const char *fname, unsigned char (**data)[3], int *w, int *h)
{
    FILE *fp = fopen(fname, "rb");
    int Endian = 1;
    unsigned char u1tmp, u2tmp;
    ulong ULtmp;
    ulong fileSize;
    ulong dataOffset;
    ulong hdrVersion;
    ulong nPlanes;
    ulong bitCount;
    ulong compression;
    ulong imageSize;
    ulong colorsUsed;
    ulong colorsImportant;
    unsigned char colorTable[256][4];

    *data = NULL;

    if (!fp)
      goto close_fail;

    if (fread(&u1tmp, 1, 1, fp) != 1)
      goto close_fail;
    if (fread(&u2tmp, 1, 1, fp) != 1)
      goto close_fail;
    if (u1tmp != 66 || u2tmp != 77)  /* 'B' and 'M' in order */
      goto close_fail;

    fread_endian(&fileSize, 4, fp, Endian);

    /* Skip 4-byte reserved segment */
    fseek(fp, 4, SEEK_CUR);

    fread_endian(&dataOffset, 4, fp, Endian);

    /* These 4 bytes hold the size of the info header.
     * Pre-standard DIB images have 12-byte "CORE" headers.
     * Standard .BMP files have 40-byte headers.
     * 'Version 4' BMPs have 108-byte headers.
     * 'Version 5' BMPs have 124-byte headers.
     * We don't support the last two formats; hardly anyone does!
     */
    fread_endian(&hdrVersion, 4, fp, Endian);

    if (hdrVersion > 0x10000) {
        /* No header is that big!  Swap endianness and try again. */
        Endian = !Endian;
        fileSize = rev_endian(fileSize, 4);
        dataOffset = rev_endian(dataOffset, 4);
        hdrVersion = rev_endian(hdrVersion, 4);
    }

    if (hdrVersion == 12) {
        /* Read a DIB image. */
        fread_endian(&ULtmp, 2, fp, Endian);
        *w = INT_MAX & ULtmp;
        fread_endian(&ULtmp, 2, fp, Endian);
        *h = INT_MAX & ULtmp;
        fread_endian(&nPlanes, 2, fp, Endian);
        fread_endian(&bitCount, 2, fp, Endian);

        /* Assume the following: */
        compression = 0;
        imageSize = (*w)*(*h);
        if (bitCount == 4) imageSize /= 2;
        else if (bitCount == 24) imageSize *= 3;
        else if (bitCount == 32) imageSize *= 4;
        colorsUsed = 0;
        goto END_OF_INFO_HEADER;
    }
    else if (hdrVersion != 40)
      goto close_fail;

    fread_endian(&ULtmp, 4, fp, Endian);
    *w = INT_MAX & ULtmp;
    fread_endian(&ULtmp, 4, fp, Endian);
    *h = INT_MAX & ULtmp;

    fread_endian(&nPlanes, 2, fp, Endian);
    fread_endian(&bitCount, 2, fp, Endian);

    if (bitCount != 1 && bitCount != 4 && bitCount != 8
                      && bitCount != 16 && bitCount != 24
                      && bitCount != 32)
      goto close_fail;

    fread_endian(&compression, 4, fp, Endian);
    fread_endian(&imageSize, 4, fp, Endian);

#if 0 /* The first test breaks legitimate 3xN-sized images */
    /* Try to correct for some vagaries of MS products */
    if (bitCount == 24 && imageSize == 4u*(*w)*(*h))
      bitCount = 32;
    else if (bitCount == 32 && imageSize == 3u*(*w)*(*h))
      bitCount = 24;
#endif

    /* Skip 8 bytes of size info: X and Y pixels per meter */
    fseek(fp, 8, SEEK_CUR);

    fread_endian(&colorsUsed, 4, fp, Endian);

    if (colorsUsed == 0)
      colorsUsed = (1UL << bitCount);

    fread_endian(&colorsImportant, 4, fp, Endian);

    if (colorsImportant == 0)
      colorsImportant = colorsUsed;

    END_OF_INFO_HEADER:

    if (bitCount <= 8) {
        /* Read the color table. */
        unsigned i;
        for (i=0; i < colorsUsed; ++i) {
            fread(&colorTable[i][2], 1, 1, fp);  /* blue */
            fread(&colorTable[i][1], 1, 1, fp);  /* green */
            fread(&colorTable[i][0], 1, 1, fp);  /* red */
            fread(&colorTable[i][3], 1, 1, fp);  /* reserved */
        }
    }

    /* Allocate space for the image data. */

    *data = malloc((*w) * (*h) * sizeof **data);
    if (*data == NULL)
      goto nospace_fail;

    if ((ulong)ftell(fp) > dataOffset)
      goto close_fail;
    else if ((ulong)ftell(fp) != dataOffset)
      fseek(fp, SEEK_SET, dataOffset);

    if (bitCount == 1) {
        int x, y;

        /* Read the raster data. */
        if (compression != 0)
          goto close_fail;

        for (y = (*h)-1; y >= 0; --y) {
            int bitsread = 0;
            for (x=0; x < (*w+7)/8; ++x) {
                if (fread(&u1tmp, 1, 1, fp) != 1)
                  goto close_fail;
                switch (*w - bitsread) {
                  default:memcpy((*data)[(x*8+7)+y*(*w)], colorTable[(u1tmp>>(7-7))&1], 3);
                  case 7: memcpy((*data)[(x*8+6)+y*(*w)], colorTable[(u1tmp>>(7-6))&1], 3);
                  case 6: memcpy((*data)[(x*8+5)+y*(*w)], colorTable[(u1tmp>>(7-5))&1], 3);
                  case 5: memcpy((*data)[(x*8+4)+y*(*w)], colorTable[(u1tmp>>(7-4))&1], 3);
                  case 4: memcpy((*data)[(x*8+3)+y*(*w)], colorTable[(u1tmp>>(7-3))&1], 3);
                  case 3: memcpy((*data)[(x*8+2)+y*(*w)], colorTable[(u1tmp>>(7-2))&1], 3);
                  case 2: memcpy((*data)[(x*8+1)+y*(*w)], colorTable[(u1tmp>>(7-1))&1], 3);
                  case 1: memcpy((*data)[(x*8+0)+y*(*w)], colorTable[(u1tmp>>(7-0))&1], 3);
                }
                bitsread += 8;
            }
            /* Align to a 32-bit boundary between rows */
            for (x = bitsread/8; x%4 != 0; ++x)
              (void)fread(&u1tmp, 1, 1, fp);
        }
    } else if (bitCount == 4 && compression == 0) {
        int x, y;
        for (y = (*h)-1; y >= 0; --y) {
            int bitsread = 0;
            for (x=0; x < (*w+1)/2; ++x) {
                if (fread(&u1tmp, 1, 1, fp) != 1)
                  goto close_fail;
                memcpy((*data)[(x*2+0)+y*(*w)], colorTable[(u1tmp>>4)&0xF], 3);
                memcpy((*data)[(x*2+1)+y*(*w)], colorTable[(u1tmp)&0xF], 3);
                bitsread += 8;
            }
            /* Align to a 32-bit boundary between rows */
            for (x = bitsread/8; x%4 != 0; ++x)
              (void)fread(&u1tmp, 1, 1, fp);
        }
    } else if (bitCount == 4 && compression == 2) {
        unsigned char n, c;
        int end_of_data = 0;
        int y = (*h)-1;
        int x = 0;
        while (!end_of_data) {
            if (fread(&n, 1, 1, fp) != 1)
              goto close_fail;
            if (fread(&c, 1, 1, fp) != 1)
              goto close_fail;
            if (n==0 && c==0) {
                --y;
                x = 0;
            } else if (n==0 && c==1) {
                end_of_data = 1;
            } else if (n==0 && c==2) {
                /* Compute a delta. */
                int nx, ny;
                if (fread(&u1tmp, 1, 1, fp) != 1)  /* x delta */
                  goto close_fail;
                if (fread(&u2tmp, 1, 1, fp) != 1)  /* y delta */
                  goto close_fail;
                ny = y-u1tmp;
                nx = (x+u2tmp)%(*w);
                if (ny < 0) goto corrupt_fail;
                for (; x < *w; ++x)
                  memcpy(&(*data)[x+y*(*w)], colorTable[0], 3);
                for (--y; y > ny; --y)
                  for (x=0; x < *w; ++x)
                    memcpy(&(*data)[x+y*(*w)], colorTable[0], 3);
                for (x=0; x < nx; ++x)
                  memcpy(&(*data)[x+y*(*w)], colorTable[0], 3);
            } else if (n==0) {
                int extra_byte = (c/2)%2;
                while (c > 0) {
                    if (fread(&u1tmp, 1, 1, fp) != 1)
                      goto close_fail;
                    memcpy((*data)[(x++)+y*(*w)], colorTable[(u1tmp>>4)&0xF], 3);
                    memcpy((*data)[(x++)+y*(*w)], colorTable[(u1tmp)&0xF], 3);
                    c -= 2;
                }
                x += c;
                y -= (x / *w);
                x %= *w;
                /* align to a 16-bit boundary */
                if (extra_byte)
                  (void)fread(&u1tmp, 1, 1, fp);
            } else {
                /* draw n pixels of alternating colors c1 and c2 */
                int c1 = (c >> 4) & 0xF;
                int c2 = c & 0xF;
                while (n > 0) {
                    memcpy((*data)[(x++)+y*(*w)], colorTable[c1], 3);
                    memcpy((*data)[(x++)+y*(*w)], colorTable[c2], 3);
                    n -= 2;
                }
                x += n;
                y -= (x / *w);
                x %= *w;
            }
        }
    } else if (bitCount == 8 && compression == 0) {
        int x;
        int y;
        for (y = (*h)-1; y >= 0; --y) {
            for (x=0; x < *w; ++x) {
                if (fread(&u1tmp, 1, 1, fp) != 1)
                  goto close_fail;
                memcpy((*data)[x+y*(*w)], colorTable[u1tmp], 3);
            }
            if (x%4 != 0) {
                /* Chomp to the next 32-bit boundary */
                unsigned char a4tmp[4];
                (void)fread(a4tmp, 1, x%4, fp);
            }
        }
    } else if (bitCount == 8 && compression == 1) {
        unsigned char n, c;
        int end_of_data = 0;
        int y = (*h)-1;
        int x = 0;
        while (!end_of_data) {
            if (fread(&n, 1, 1, fp) != 1)
              goto close_fail;
            if (fread(&c, 1, 1, fp) != 1)
              goto close_fail;
            if (n==0 && c==0) {
                --y;
                x = 0;
            } else if (n==0 && c==1) {
                end_of_data = 1;
            } else if (n==0 && c==2) {
                /* Compute a delta. */
                int nx, ny;
                if (fread(&u1tmp, 1, 1, fp) != 1)  /* x delta */
                  goto close_fail;
                if (fread(&u2tmp, 1, 1, fp) != 1)  /* y delta */
                  goto close_fail;
                ny = y-u1tmp;
                nx = (x+u2tmp)%(*w);
                if (ny < 0) goto corrupt_fail;
                for (; x < *w; ++x)
                  memcpy(&(*data)[x+y*(*w)], colorTable[0], 3);
                for (--y; y > ny; --y)
                  for (x=0; x < *w; ++x)
                    memcpy(&(*data)[x+y*(*w)], colorTable[0], 3);
                for (x=0; x < nx; ++x)
                  memcpy(&(*data)[x+y*(*w)], colorTable[0], 3);
            } else if (n==0) {
                int extra_byte = (c/2)%2;
                while (c > 0) {
                    if (fread(&u1tmp, 1, 1, fp) != 1)
                      goto close_fail;
                    memcpy((*data)[(x++)+y*(*w)], colorTable[u1tmp&0xFF], 3);
                    --c;
                }
                y -= (x / *w);
                x %= *w;
                /* align to a 16-bit boundary */
                if (extra_byte)
                  (void)fread(&u1tmp, 1, 1, fp);
            } else {
                /* draw n pixels of color c */
                while (n > 0) {
                    memcpy((*data)[(x++)+y*(*w)], colorTable[c & 0xFF], 3);
                    --n;
                }
                y -= (x / *w);
                x %= *w;
            }
        }
    } else if (bitCount == 24 && compression == 0) {
        unsigned char a4tmp[4];
        int x;
        int y;
        for (y = (*h)-1; y >= 0; --y) {
            for (x=0; x < *w; ++x) {
                /* This seems to be the actual format */
                if (fread(a4tmp, 1, 3, fp) != 3)
                  goto close_fail;
                (*data)[x+y*(*w)][0] = a4tmp[2];
                (*data)[x+y*(*w)][1] = a4tmp[1];
                (*data)[x+y*(*w)][2] = a4tmp[0];
            }
            if (x%4 != 0) {
                /* Chomp to the next 32-bit boundary */
                (void)fread(a4tmp, 1, x%4, fp);
            }
        }
    } else if (bitCount == 32 && compression == 0) {
        unsigned char a4tmp[4];
        int x;
        int y;
        for (y = (*h)-1; y >= 0; --y) {
            for (x=0; x < *w; ++x) {
                /* Ignore byte 4, the alpha channel */
                if (fread(a4tmp, 1, 4, fp) != 4)
                  goto close_fail;
                (*data)[x+y*(*w)][0] = a4tmp[2];
                (*data)[x+y*(*w)][1] = a4tmp[1];
                (*data)[x+y*(*w)][2] = a4tmp[0];
            }
        }
    } else {
        /* different bitCount/compression combination */
        goto close_fail;
    }

    if ((ulong)ftell(fp) != dataOffset + imageSize) {
        /* This should not happen; but it does, in practice.
         * Don't worry about it. */
        /* do nothing */ ;
    }

    fclose(fp);
    return 0;

    close_fail:    /* Invalid image header, or no file.  *data is NULL. */
    if (*data)
      free(*data);
    *data = NULL;
    if (fp)
      fclose(fp);
    return -1;

    corrupt_fail:  /* Some image data was read.  *data is not NULL. */
    fclose(fp);
    return -2;

    nospace_fail:  /* Out of memory. */
    fclose(fp);
    return -3;
}


/* Save a 24-bit BMP file. */
int WriteBMP24(const char *fname, unsigned char (*data)[3], int w, int h)
{
    FILE *fp = fopen(fname, "wb");
    int Endian = 1;
    unsigned char buffer[40];
    ulong fileSize;
    ulong imageSize;
    int x, y;

    if (!fp)
      return -1;

    imageSize = 3*w*h;
    fileSize = 14 + 40 + imageSize;

    /* Write header data */
    memset(buffer, 0x00, 14);
    buffer[0] = 66;
    buffer[1] = 77;
    bwrite_endian(fileSize, 4, buffer+2, Endian);
    /* Four bytes of zero data (reserved) */
    bwrite_endian(0, 4, buffer+6, Endian);
    /* Four bytes of data offset (= 14+40 = 54) */
    bwrite_endian(54, 4, buffer+10, Endian);
    fwrite(buffer, 1, 14, fp);

    /* Write info header */
    memset(buffer, 0x00, 40);
    bwrite_endian(40, 4, buffer+0, Endian);
    bwrite_endian(w, 4, buffer+4, Endian);
    bwrite_endian(h, 4, buffer+8, Endian);
    bwrite_endian(1, 2, buffer+12, Endian);  /* Number of planes */
    bwrite_endian(24, 2, buffer+14, Endian);  /* Bit count */
    bwrite_endian(0, 4, buffer+16, Endian);   /* No compression */
    bwrite_endian(imageSize, 4, buffer+20, Endian);
    /* Pixels per meter.  X and Y.  We don't care. Say 0x00010000. */
    bwrite_endian(0x10000, 4, buffer+24, Endian);
    bwrite_endian(0x10000, 4, buffer+28, Endian);
    /* Colors in the image. 2^^24 colors, all "important." */
    bwrite_endian(0x1000000, 4, buffer+32, Endian);
    bwrite_endian(0, 4, buffer+36, Endian);
    fwrite(buffer, 1, 40, fp);

    /* No color table. */

    /* Raster data. */
    memset(buffer, 0x00, 4);
    for (y = h-1; y >= 0; --y) {
        for (x = 0; x < w; ++x) {
            fwrite(&data[x+y*w][2], 1, 1, fp);
            fwrite(&data[x+y*w][1], 1, 1, fp);
            fwrite(&data[x+y*w][0], 1, 1, fp);
        }
        if (x%4 != 0)
          fwrite(buffer, 1, x%4, fp);
    }

    fclose(fp);
    return 0;
}


/* Save a 32-bit BMP file. */
int WriteBMP32(const char *fname, unsigned char (*data)[3], int w, int h)
{
    FILE *fp = fopen(fname, "wb");
    int Endian = 1;
    unsigned char buffer[40];
    ulong fileSize;
    ulong imageSize;
    int x, y;

    if (!fp)
      return -1;

    imageSize = 4*w*h;
    fileSize = 14 + 40 + imageSize;

     /* Write header data */
    /* Write header data */
    memset(buffer, 0x00, 14);
    buffer[0] = 66;
    buffer[1] = 77;
    bwrite_endian(fileSize, 4, buffer+2, Endian);
    /* Four bytes of zero data (reserved) */
    bwrite_endian(0, 4, buffer+6, Endian);
    /* Four bytes of data offset (= 14+40 = 54) */
    bwrite_endian(54, 4, buffer+10, Endian);
    fwrite(buffer, 1, 14, fp);

    /* Write info header */
    memset(buffer, 0x00, 40);
    bwrite_endian(40, 4, buffer+0, Endian);
    bwrite_endian(w, 4, buffer+4, Endian);
    bwrite_endian(h, 4, buffer+8, Endian);
    bwrite_endian(1, 2, buffer+12, Endian);  /* Number of planes */
    bwrite_endian(32, 2, buffer+14, Endian);  /* Bit count */
    bwrite_endian(0, 4, buffer+16, Endian);   /* No compression */
    bwrite_endian(imageSize, 4, buffer+20, Endian);
    /* Pixels per meter.  X and Y.  We don't care. Say 0x00010000. */
    bwrite_endian(0x10000, 4, buffer+24, Endian);
    bwrite_endian(0x10000, 4, buffer+28, Endian);
    /* Colors in the image. 2^^24 colors, all "important." */
    bwrite_endian(0x1000000, 4, buffer+32, Endian);
    bwrite_endian(0, 4, buffer+36, Endian);
    fwrite(buffer, 1, 40, fp);

    /* No color table. */

    /* Raster data. */
    buffer[0] = 0;
    for (y = h-1; y >= 0; --y) {
        for (x = 0; x < w; ++x) {
            fwrite(&data[x+y*w][2], 1, 1, fp);
            fwrite(&data[x+y*w][1], 1, 1, fp);
            fwrite(&data[x+y*w][0], 1, 1, fp);
            /* The zeroed-out alpha channel. */
            fwrite(buffer, 1, 1, fp);
        }
    }

    fclose(fp);
    return 0;
}


struct ColorTable
{
    unsigned char entries[256][3];
    int nentries;
};

static void initColorTable(struct ColorTable *tab);
static int addColorEntry(struct ColorTable *tab, unsigned char data[3]);
static int findColorEntry(struct ColorTable *tab, unsigned char data[3]);


/* Save an 8-bit BMP file. */
int WriteBMP8(const char *fname, unsigned char (*data)[3], int w, int h)
{
    FILE *fp = fopen(fname, "wb");
    int Endian = 1;
    unsigned char buffer[40];
    ulong fileSize;
    ulong dataOffset;
    ulong imageSize;
    ulong i;
    int x, y;
    struct ColorTable colorTable;

    initColorTable(&colorTable);

    if (!fp)
      return -1;

    imageSize = w*h;
    dataOffset = 14 + 40 + 4*256;
    fileSize = dataOffset + imageSize;

    for (i=0; i < imageSize; ++i) {
        if (addColorEntry(&colorTable, data[i]) == -1)
          return -1;
    }

    /* Write header data */
    memset(buffer, 0x00, 14);
    buffer[0] = 66;
    buffer[1] = 77;
    bwrite_endian(fileSize, 4, buffer+2, Endian);
    /* Four bytes of zero data (reserved) */
    bwrite_endian(0, 4, buffer+6, Endian);
    /* Four bytes of data offset */
    bwrite_endian(dataOffset, 4, buffer+10, Endian);
    fwrite(buffer, 1, 14, fp);

    /* Write info header */
    memset(buffer, 0x00, 40);
    bwrite_endian(40, 4, buffer+0, Endian);
    bwrite_endian(w, 4, buffer+4, Endian);
    bwrite_endian(h, 4, buffer+8, Endian);
    bwrite_endian(1, 2, buffer+12, Endian);  /* Number of planes */
    bwrite_endian(8, 2, buffer+14, Endian);  /* Bit count */
    bwrite_endian(0, 4, buffer+16, Endian);   /* No compression */
    bwrite_endian(imageSize, 4, buffer+20, Endian);
    /* Pixels per meter.  X and Y.  We don't care. Say 0x00010000. */
    bwrite_endian(0x10000, 4, buffer+24, Endian);
    bwrite_endian(0x10000, 4, buffer+28, Endian);
    /* Colors in the image. 2^^8 colors, all "important." */
    bwrite_endian(0x100, 4, buffer+32, Endian);
    bwrite_endian(0x100, 4, buffer+36, Endian);
    fwrite(buffer, 1, 40, fp);

    /* Color table. */
    buffer[0] = 0;
    for (i=0; i < 256; ++i) {
        fwrite(&colorTable.entries[i][2], 1, 1, fp);
        fwrite(&colorTable.entries[i][1], 1, 1, fp);
        fwrite(&colorTable.entries[i][0], 1, 1, fp);
        fwrite(buffer, 1, 1, fp);
    }

    /* Raster data. */
    for (y = h-1; y >= 0; --y) {
        for (x = 0; x < w; ++x) {
            int colorEntry = findColorEntry(&colorTable, data[y*w+x]);
            unsigned char buf = colorEntry & 0xFF;
            fwrite(&buf, 1, 1, fp);
        }
        /* Pad with zeroes to the 32-bit boundary. */
        for (; x % 4 != 0; ++x) {
            fwrite(buffer, 1, 1, fp);
        }
    }

    fclose(fp);
    return 0;
}


/* Save a compressed 8-bit BMP file. */
int WriteBMP8C(const char *fname, unsigned char (*data)[3], int w, int h)
{
    FILE *fp = fopen(fname, "wb");
    int Endian = 1;
    unsigned char buffer[40];
    ulong fileSize;
    ulong dataOffset;
    ulong imageSize;
    ulong i;
    struct ColorTable colorTable;

    initColorTable(&colorTable);

    if (!fp)
      return -1;

    imageSize = w*h;
    dataOffset = 14 + 40 + 4*256;

    /* Let's see whether we can get away with this... */
    fileSize = 0;

    for (i=0; i < imageSize; ++i) {
        if (addColorEntry(&colorTable, data[i]) == -1) {
            fclose(fp);
            return -3;
        }
    }

    /* Write header data */
    memset(buffer, 0x00, 14);
    buffer[0] = 66;
    buffer[1] = 77;
    bwrite_endian(fileSize, 4, buffer+2, Endian);
    /* Four bytes of zero data (reserved) */
    bwrite_endian(0, 4, buffer+6, Endian);
    /* Four bytes of data offset */
    bwrite_endian(dataOffset, 4, buffer+10, Endian);
    fwrite(buffer, 1, 14, fp);

    /* Write info header */
    memset(buffer, 0x00, 40);
    bwrite_endian(40, 4, buffer+0, Endian);
    bwrite_endian(w, 4, buffer+4, Endian);
    bwrite_endian(h, 4, buffer+8, Endian);
    bwrite_endian(1, 2, buffer+12, Endian);   /* Number of planes */
    bwrite_endian(8, 2, buffer+14, Endian);   /* Bit count */
    bwrite_endian(1, 4, buffer+16, Endian);   /* RLE_8 compression */
    bwrite_endian(imageSize, 4, buffer+20, Endian);
    /* Pixels per meter.  X and Y.  We don't care. Say 0x00010000. */
    bwrite_endian(0x10000, 4, buffer+24, Endian);
    bwrite_endian(0x10000, 4, buffer+28, Endian);
    /* Colors in the image. 2^^8 colors, all "important." */
    bwrite_endian(0x100, 4, buffer+32, Endian);
    bwrite_endian(0x100, 4, buffer+36, Endian);
    fwrite(buffer, 1, 40, fp);

    /* Color table. */
    buffer[0] = 0;
    for (i=0; i < 256; ++i) {
        fwrite(&colorTable.entries[i][2], 1, 1, fp);
        fwrite(&colorTable.entries[i][1], 1, 1, fp);
        fwrite(&colorTable.entries[i][0], 1, 1, fp);
        fwrite(&buffer, 1, 1, fp);
    }

    /* Raster data. */
    {
        /******
         * Collect as large a 'run' as you can of the same color.
         * If that run is 3 cells or longer, print it out and repeat.
         * Otherwise, collect more cells until you find a single repeat.
         * Print out all the cells before that repeat at once; back up
         * and repeat again.
         *****/
        unsigned char buf[2] = {0};
        int lastEntry = -1;
        int currentSameLength = 0;
        int currentDiffLength = 0;
        int firstUnprintedX = 0;
        int firstUnprintedY = h-1;
        int dataWritten = 0;
        long x, y, i;

        for (y = h-1; y >= 0; --y) {
            for (x = 0; x < w; ++x) {
                int colorEntry = findColorEntry(&colorTable, data[x+y*w]);

                if (lastEntry == -1) {
                    currentSameLength = 1;
                }
                else if (currentSameLength) {
                    if (currentSameLength < 250 && colorEntry == lastEntry) {
                        ++currentSameLength;
                    }
                    else {
                        if (currentSameLength >= 3) {
                            /* print out everything the same */
                            buf[0] = currentSameLength;
                            buf[1] = lastEntry;
                            fwrite(buf, 1, 2, fp);
                            dataWritten += 2;
                            firstUnprintedX = x;
                            firstUnprintedY = y;
                            currentSameLength = 1;
                            currentDiffLength = 0;
                        }
                        else {
                            currentDiffLength = currentSameLength+1;
                            currentSameLength = 0;
                        }
                    }
                }
                else {
                    if (currentDiffLength < 250 &&
                        (currentDiffLength < 5 || colorEntry != lastEntry))
                    {
                        ++currentDiffLength;
                    }
                    else {
                        /* print out everything (less one) different */
                        buf[0] = 0;
                        buf[1] = currentDiffLength-1;
                        dataWritten += fwrite(buf, 1, 2, fp);
                        for (i = y*w+x - currentDiffLength; i+1 < y*w+x; ++i) {
                            buf[0] = findColorEntry(&colorTable, data[i]);
                            dataWritten += fwrite(buf, 1, 1, fp);
                        }
                        if (dataWritten%2) {
                            buf[0] = 0;
                            dataWritten += fwrite(buf, 1, 1, fp);
                        }
                        firstUnprintedX = (x==0)? (w-1): (x-1);
                        firstUnprintedY = (x==0)? (y+1): (y);
                        currentSameLength = 2;
                        currentDiffLength = 0;
                    }
                }

                lastEntry = colorEntry;
            }
        }

        if (currentSameLength) {
            /* print out everything same that's left */
            buf[0] = currentSameLength;
            buf[1] = lastEntry;
            dataWritten += fwrite(buf, 1, 2, fp);
        }
        else if (currentDiffLength >= 3) {
            /* print out everything different that's left */
            buf[0] = 0;
            buf[1] = currentDiffLength;
            fwrite(buf, 1, 2, fp);
            dataWritten += 2;
            for (i=firstUnprintedY*w+firstUnprintedX; i < h*w; ++i) {
                buf[0] = findColorEntry(&colorTable, data[i]);
                dataWritten += fwrite(buf, 1, 1, fp);
            }
            if (dataWritten%2) {
                buf[0] = 0;
                dataWritten += fwrite(buf, 1, 1, fp);
            }
        }
        else if (currentDiffLength) {
            /* print out everything different one-by-one */
            for (i=firstUnprintedY*w+firstUnprintedX; i < h*w; ++i) {
                buf[0] = 1;
                buf[1] = findColorEntry(&colorTable, data[i]);
                dataWritten += fwrite(buf, 1, 2, fp);
            }
        }

        /* Write end-of-image marker. */
        buf[0] = 0;
        buf[1] = 1;
        fwrite(buf, 1, 2, fp);
        dataWritten += 2;
        if (dataWritten % 4) {
            buf[1] = 0;
            fwrite(buf, 1, 2, fp);
        }

        /* End of enclosing declaration block */
    }

    fclose(fp);
    return 0;
}



/* Scan whitespace and comments.
 * Pre- and post-condition: *c holds first unscanned character.
 */
static void PPM_scan_ws(unsigned char *c, FILE *fp)
{
    /* Scan one comment line */
    if (*c=='#')
      while((*c = getc(fp)) != '\n')
        continue;
    /* Scan whitespace */
    while (isspace(*c)) {
        *c = getc(fp);
        /* Scan embedded comment lines */
        if (*c=='#')
          while ((*c = getc(fp)) != '\n')
            continue;
    }
}


/* Attempt to load a binary-formatted PPM file. */
int ReadPPM6(const char *fname, unsigned char (**data)[3], int *w, int *h)
{
    FILE *fp = fopen(fname, "rb");
    unsigned char buf[6] = {0};
    unsigned char c;
    int maxval;
    int i;
    unsigned char (*rgb)[3];

    *data = NULL;

    if (!fp)
      goto close_fail;

    if (fread(buf, 1, 2, fp) != 2)
      goto close_fail;
    if (buf[0] != 'P' || buf[1] != '6')
      goto close_fail;

    *w = *h = 0;
    maxval = 0;

    c = getc(fp);

    /* Scan for image width */
    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *w = 10*(*w) + c - '0';
      c = getc(fp);
    }

    /* Scan for image height */
    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *h = 10*(*h) + c - '0';
      c = getc(fp);
    }

    /* Scan for maxval */
    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      maxval = 10*maxval + c - '0';
      c = getc(fp);
    }

    if (!isspace(c) || maxval > 65536 || maxval <= 0)
      goto close_fail;

    if (*w <= 0 || *h <= 0)
      goto close_fail;

    *data = malloc((*w) * (*h) * sizeof **data);
    if (*data == NULL)
      goto nospace_fail;

    for (i=0, rgb = *data; i < (*w)*(*h); ++i, ++rgb) {
        if (maxval < 256) {
            if (fread(buf, 1, 3, fp) != 3)
              goto corrupt_fail;
            (*rgb)[0] = (buf[0]+1) * 256 / (maxval+1) - 1;
            (*rgb)[1] = (buf[1]+1) * 256 / (maxval+1) - 1;
            (*rgb)[2] = (buf[2]+1) * 256 / (maxval+1) - 1;
        }
        else {
            if (fread(buf, 1, 6, fp) != 6)
              goto corrupt_fail;
            (*rgb)[0] = ((buf[0]<<8 | buf[1])+1)*256 / (maxval+1) - 1;
            (*rgb)[1] = ((buf[2]<<8 | buf[3])+1)*256 / (maxval+1) - 1;
            (*rgb)[2] = ((buf[4]<<8 | buf[5])+1)*256 / (maxval+1) - 1;
        }
    }

    fclose(fp);
    return 0;

    close_fail:    /* Invalid image header, or no file.  *data is NULL. */
    if (*data)
      free(*data);
    *data = NULL;
    if (fp)
      fclose(fp);
    return -1;

    corrupt_fail:  /* Some image data was read.  *data is not NULL. */
    fclose(fp);
    return -2;

    nospace_fail:  /* Out of memory. */
    fclose(fp);
    return -3;
}


/* Attempt to load a "plain" text-formatted PPM file. */
int ReadPPM3(const char *fname, unsigned char (**data)[3], int *w, int *h)
{
    FILE *fp = fopen(fname, "r");
    unsigned char buf[2] = {0};
    unsigned char c;
    int maxval;
    int i, r, g, b;

    *data = NULL;

    if (!fp)
      goto close_fail;

    if (fread(buf, 1, 2, fp) != 2)
      goto close_fail;
    if (buf[0] != 'P' || buf[1] != '3')
      goto close_fail;

    *w = *h = 0;
    maxval = 0;

    c = getc(fp);

    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *w = 10*(*w) + c - '0';
      c = getc(fp);
    }

    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *h = 10*(*h) + c - '0';
      c = getc(fp);
    }

    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      maxval = 10*maxval + c - '0';
      c = getc(fp);
    }

    if (!isspace(c) || maxval > 65536 || maxval <= 0)
      goto close_fail;

    if (*w <= 0 || *h <= 0)
      goto close_fail;

    *data = malloc((*w) * (*h) * sizeof **data);
    if (*data == NULL)
      goto nospace_fail;

    for (i=0; i < (*w)*(*h); ++i) {
        if (fscanf(fp, "%d%d%d", &r, &g, &b) != 3)
          goto corrupt_fail;
        if (r < 0 || r > maxval
         || g < 0 || g > maxval
         || b < 0 || b > maxval)
          goto corrupt_fail;
        (*data)[i][0] = (r+1) * 256 / (maxval+1) - 1;
        (*data)[i][0] = (g+1) * 256 / (maxval+1) - 1;
        (*data)[i][0] = (b+1) * 256 / (maxval+1) - 1;
    }

    fclose(fp);
    return 0;

    close_fail:    /* Invalid image header, or no file.  *data is NULL. */
    if (*data)
      free(*data);
    *data = NULL;
    if (fp)
      fclose(fp);
    return -1;

    corrupt_fail:  /* Some image data was read.  *data is not NULL. */
    fclose(fp);
    return -2;

    nospace_fail:  /* Out of memory. */
    fclose(fp);
    return -3;
}


int ReadPPM(const char *fname, unsigned char (**data)[3], int *w, int *h)
{
    FILE *fp = fopen(fname, "r");
    int c;

    *data = NULL;

    if (!fp)
      goto close_fail;

    if (getc(fp) != 'P')
      goto close_fail;

    c = getc(fp);
    fclose(fp);

    if (c == '3')
      return ReadPPM3(fname, data, w, h);
    else if (c == '6')
      return ReadPPM6(fname, data, w, h);
    else if (c == '1' || c == '2' || c == '4' || c == '5') {
        unsigned char *temp = NULL;
        int rc = ReadPGM(fname, &temp, w, h);
        if (rc != 0) {
            if (temp != NULL)
              free(temp);
            return rc;
        }
        if (Gray2RGB(temp, data, *w, *h)) {
            free(temp);
            return -3;
        }
        return 0;
    }

    close_fail:    /* Invalid image header, or no file.  *data is NULL. */
    if (*data)
      free(*data);
    *data = NULL;
    if (fp)
      fclose(fp);
    return -1;
}


int WritePPM6(const char *fname, unsigned char (*data)[3], int w, int h)
{
    FILE *fp = fopen(fname, "wb");
    int i;

    if (!fp)
      return -1;

    fprintf(fp, "P6\n");
    fprintf(fp, "%d %d 255\n", w, h);
    for (i=0; i < w*h; ++i) {
        if (fwrite(data[i], 1, 3, fp) != 3) {
            fclose(fp);
            return -1;
        }
    }
    fclose(fp);
    return 0;
}


int WritePPM3(const char *fname, unsigned char (*data)[3], int w, int h)
{
    FILE *fp = fopen(fname, "w");
    int i;

    if (!fp)
      return -1;

    fprintf(fp, "P3\n");
    fprintf(fp, "%d %d 255\n", w, h);
    for (i=0; i < w*h; ++i) {
        fprintf(fp, "%d %d %d%c", data[i][0], data[i][1], data[i][2],
            (i%w == w-1)? '\n': '\t');
    }
    fclose(fp);
    return 0;
}



/* Attempt to load a binary-formatted PGM file. */
int ReadPGM5(const char *fname, unsigned char **data, int *w, int *h)
{
    FILE *fp = fopen(fname, "rb");
    unsigned char buf[2] = {0};
    unsigned char c;
    int maxval;
    int i;
    unsigned char *rgb;

    *data = NULL;

    if (!fp)
      goto close_fail;

    if (fread(buf, 1, 2, fp) != 2)
      goto close_fail;
    if (buf[0] != 'P' || buf[1] != '5')
      goto close_fail;

    *w = *h = 0;
    maxval = 0;

    c = getc(fp);

    /* Scan for image width */
    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *w = 10*(*w) + c - '0';
      c = getc(fp);
    }

    /* Scan for image height */
    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *h = 10*(*h) + c - '0';
      c = getc(fp);
    }

    /* Scan for maxval */
    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      maxval = 10*maxval + c - '0';
      c = getc(fp);
    }

    if (!isspace(c) || maxval > 65536 || maxval <= 0)
      goto close_fail;

    if (*w <= 0 || *h <= 0)
      goto close_fail;

    *data = malloc((*w) * (*h) * sizeof **data);
    if (*data == NULL)
      goto nospace_fail;

    for (i=0, rgb = *data; i < (*w)*(*h); ++i, ++rgb) {
        if (maxval < 256) {
            if (fread(buf, 1, 1, fp) != 1)
              goto corrupt_fail;
            *rgb = (buf[0]+1) * 256 / (maxval+1) - 1;
        }
        else {
            if (fread(buf, 1, 2, fp) != 2)
              goto corrupt_fail;
            *rgb = ((buf[0]<<8 | buf[1])+1)*256 / (maxval+1) - 1;
        }
    }

    fclose(fp);
    return 0;

    close_fail:    /* Invalid image header, or no file.  *data is NULL. */
    if (*data)
      free(*data);
    *data = NULL;
    if (fp)
      fclose(fp);
    return -1;

    corrupt_fail:  /* Some image data was read.  *data is not NULL. */
    fclose(fp);
    return -2;

    nospace_fail:  /* Out of memory. */
    fclose(fp);
    return -3;
}


/* Attempt to load a "plain" text-formatted PGM file. */
int ReadPGM2(const char *fname, unsigned char **data, int *w, int *h)
{
    FILE *fp = fopen(fname, "r");
    unsigned char buf[2] = {0};
    unsigned char c;
    int maxval;
    int i, g;

    *data = NULL;

    if (!fp)
      goto close_fail;

    if (fread(buf, 1, 2, fp) != 2)
      goto close_fail;
    if (buf[0] != 'P' || buf[1] != '2')
      goto close_fail;

    *w = *h = 0;
    maxval = 0;

    c = getc(fp);

    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *w = 10*(*w) + c - '0';
      c = getc(fp);
    }

    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *h = 10*(*h) + c - '0';
      c = getc(fp);
    }

    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      maxval = 10*maxval + c - '0';
      c = getc(fp);
    }

    if (!isspace(c) || maxval > 65536 || maxval <= 0)
      goto close_fail;

    if (*w <= 0 || *h <= 0)
      goto close_fail;

    *data = malloc((*w) * (*h) * sizeof **data);
    if (*data == NULL)
      goto nospace_fail;

    for (i=0; i < (*w)*(*h); ++i) {
        if (fscanf(fp, "%d", &g) != 1)
          goto corrupt_fail;
        if (g < 0 || g > maxval)
          goto corrupt_fail;
        (*data)[i] = (g+1) * 256 / (maxval+1) - 1;
    }

    fclose(fp);
    return 0;

    close_fail:    /* Invalid image header, or no file.  *data is NULL. */
    if (*data)
      free(*data);
    *data = NULL;
    if (fp)
      fclose(fp);
    return -1;

    corrupt_fail:  /* Some image data was read.  *data is not NULL. */
    fclose(fp);
    return -2;

    nospace_fail:  /* Out of memory. */
    fclose(fp);
    return -3;
}


int ReadPGM(const char *fname, unsigned char **data, int *w, int *h)
{
    FILE *fp = fopen(fname, "r");
    int c;

    *data = NULL;

    if (!fp)
      goto close_fail;

    if (getc(fp) != 'P')
      goto close_fail;

    c = getc(fp);
    fclose(fp);

    if (c == '2')
      return ReadPGM2(fname, data, w, h);
    else if (c == '5')
      return ReadPGM5(fname, data, w, h);
    else if (c == '1')
      return ReadPBM1(fname, data, w, h);
    else if (c == '4')
      return ReadPBM4(fname, data, w, h);
    else if (c == '3' || c == '6') {
        unsigned char (*temp)[3] = NULL;
        int rc = ReadPPM(fname, &temp, w, h);
        if (rc != 0) {
            if (temp != NULL)
              free(temp);
            return rc;
        }
        if (RGB2Gray(temp, data, *w, *h)) {
            free(temp);
            return -3;
        }
        return 0;
    }

    close_fail:    /* Invalid image header, or no file.  *data is NULL. */
    if (*data)
      free(*data);
    *data = NULL;
    if (fp)
      fclose(fp);
    return -1;
}


int WritePGM5(const char *fname, unsigned char *data, int w, int h)
{
    FILE *fp = fopen(fname, "wb");
    int i;

    if (!fp)
      return -1;

    fprintf(fp, "P5\n");
    fprintf(fp, "%d %d 255\n", w, h);
    for (i=0; i < w*h; ++i) {
        if (fwrite(&data[i], 1, 1, fp) != 1) {
            fclose(fp);
            return -1;
        }
    }
    fclose(fp);
    return 0;
}


int WritePGM2(const char *fname, unsigned char *data, int w, int h)
{
    FILE *fp = fopen(fname, "w");
    int i;

    if (!fp)
      return -1;

    fprintf(fp, "P2\n");
    fprintf(fp, "%d %d 255\n", w, h);
    for (i=0; i < w*h; ++i) {
        fprintf(fp, "%d%c", data[i], (i%w == w-1)? '\n': ' ');
    }
    fclose(fp);
    return 0;
}


/* Attempt to load a binary-formatted PBM file. */
int ReadPBM4(const char *fname, unsigned char **data, int *w, int *h)
{
    FILE *fp = fopen(fname, "rb");
    unsigned char buf[2] = {0};
    unsigned char c;
    int i, j;
    int imagelength;
    unsigned char *gray;

    *data = NULL;

    if (!fp)
      goto close_fail;

    if (fread(buf, 1, 2, fp) != 2)
      goto close_fail;
    if (buf[0] != 'P' || buf[1] != '4')
      goto close_fail;

    *w = *h = 0;

    c = getc(fp);

    /* Scan for image width */
    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *w = 10*(*w) + c - '0';
      c = getc(fp);
    }

    /* Scan for image height */
    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *h = 10*(*h) + c - '0';
      c = getc(fp);
    }

    if (!isspace(c))
      goto close_fail;

    if (*w <= 0 || *h <= 0)
      goto close_fail;

    imagelength = (*w)*(*h);
    *data = malloc(imagelength * sizeof **data);
    if (*data == NULL)
      goto nospace_fail;

    gray = *data;
    for (j=0; j < *h; ++j) {
	/* Bits are stored eight per byte, high bit first, low bit last.
	 * Rows are padded to a multiple of eight bits per row. */
        for (i=0; i < *w; ) {
	    unsigned char *oldgray = gray;
	    if (fread(buf, 1, 1, fp) != 1)
		goto corrupt_fail;
	    switch (*w - i) {
	      default:
		oldgray[7] = (buf[0] & 0x01) ? 0 : 255; ++gray; ++i; /* FALLTHROUGH */
	      case 7:
		oldgray[6] = (buf[0] & 0x02) ? 0 : 255; ++gray; ++i; /* FALLTHROUGH */
	      case 6:
		oldgray[5] = (buf[0] & 0x04) ? 0 : 255; ++gray; ++i; /* FALLTHROUGH */
	      case 5:
		oldgray[4] = (buf[0] & 0x08) ? 0 : 255; ++gray; ++i; /* FALLTHROUGH */
	      case 4:
		oldgray[3] = (buf[0] & 0x10) ? 0 : 255; ++gray; ++i; /* FALLTHROUGH */
	      case 3:
		oldgray[2] = (buf[0] & 0x20) ? 0 : 255; ++gray; ++i; /* FALLTHROUGH */
	      case 2:
		oldgray[1] = (buf[0] & 0x40) ? 0 : 255; ++gray; ++i; /* FALLTHROUGH */
	      case 1:
		oldgray[0] = (buf[0] & 0x80) ? 0 : 255; ++gray; ++i;
	    }
	}
    }

    fclose(fp);
    return 0;

    close_fail:    /* Invalid image header, or no file.  *data is NULL. */
    if (*data)
      free(*data);
    *data = NULL;
    if (fp)
      fclose(fp);
    return -1;

    corrupt_fail:  /* Some image data was read.  *data is not NULL. */
    fclose(fp);
    return -2;

    nospace_fail:  /* Out of memory. */
    fclose(fp);
    return -3;
}


/* Attempt to load a "plain" text-formatted PBM file. */
int ReadPBM1(const char *fname, unsigned char **data, int *w, int *h)
{
    FILE *fp = fopen(fname, "r");
    unsigned char buf[2] = {0};
    unsigned char c;
    int i, g;

    *data = NULL;

    if (!fp)
      goto close_fail;

    if (fread(buf, 1, 2, fp) != 2)
      goto close_fail;
    if (buf[0] != 'P' || buf[1] != '1')
      goto close_fail;

    *w = *h = 0;

    c = getc(fp);

    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *w = 10*(*w) + c - '0';
      c = getc(fp);
    }

    PPM_scan_ws(&c, fp);
    while (isdigit(c)) {
      *h = 10*(*h) + c - '0';
      c = getc(fp);
    }

    if (!isspace(c))
      goto close_fail;

    if (*w <= 0 || *h <= 0)
      goto close_fail;

    *data = malloc((*w) * (*h) * sizeof **data);
    if (*data == NULL)
      goto nospace_fail;

    /* Rows are not padded to a multiple of 8. */
    for (i=0; i < (*w)*(*h); ++i) {
        if (fscanf(fp, "%d", &g) != 1)
          goto corrupt_fail;
        if (g < 0 || g > 1)
          goto corrupt_fail;
        (*data)[i] = (g ? 0 : 255);
    }

    fclose(fp);
    return 0;

    close_fail:    /* Invalid image header, or no file.  *data is NULL. */
    if (*data)
      free(*data);
    *data = NULL;
    if (fp)
      fclose(fp);
    return -1;

    corrupt_fail:  /* Some image data was read.  *data is not NULL. */
    fclose(fp);
    return -2;

    nospace_fail:  /* Out of memory. */
    fclose(fp);
    return -3;
}


int WritePBM4(const char *fname, unsigned char *data, int w, int h)
{
    FILE *fp = fopen(fname, "w");
    int i, j;

    if (!fp)
      return -1;

    fprintf(fp, "P4\n");
    fprintf(fp, "%d %d\n", w, h);
    for (j=0; j < h; ++j) {
	/* Rows are padded to a multiple of 8 bits per row. */
        for (i=0; i < w; i += 8) {
            unsigned char ch = (data[j*w+i+0] ? 0 : 0x80);
            if (i+1 < w) ch |= (data[j*w+i+1] ? 0 : 0x40);
            if (i+2 < w) ch |= (data[j*w+i+2] ? 0 : 0x20);
            if (i+3 < w) ch |= (data[j*w+i+3] ? 0 : 0x10);
            if (i+4 < w) ch |= (data[j*w+i+4] ? 0 : 0x08);
            if (i+5 < w) ch |= (data[j*w+i+5] ? 0 : 0x04);
            if (i+6 < w) ch |= (data[j*w+i+6] ? 0 : 0x02);
            if (i+7 < w) ch |= (data[j*w+i+7] ? 0 : 0x01);
	    if (fwrite(&ch, 1, 1, fp) != 1) {
		fclose(fp);
		return -1;
	    }
	}
    }
    fclose(fp);
    return 0;
}


int WritePBM1(const char *fname, unsigned char *data, int w, int h)
{
    FILE *fp = fopen(fname, "w");
    int i;

    if (!fp)
      return -1;

    fprintf(fp, "P1\n");
    fprintf(fp, "%d %d\n", w, h);
    /* Rows are not padded to a multiple of 8. */
    for (i=0; i < w*h; ++i) {
        putc((data[i]? '0' : '1'), fp);
        putc(((i%w == w-1)? '\n': ' '), fp);
    }
    fclose(fp);
    return 0;
}


int Gray2RGB(unsigned char *gray, unsigned char (**rgb)[3], int w, int h)
{
    int i;
    *rgb = malloc(w*h * sizeof **rgb);
    if (*rgb == NULL)
      return -3;
    for (i=0; i < w*h; ++i) {
        (*rgb)[i][0] = gray[i];
        (*rgb)[i][1] = gray[i];
        (*rgb)[i][2] = gray[i];
    }
    return 0;
}

int RGB2Gray(unsigned char (*rgb)[3], unsigned char **gray, int w, int h)
{
    int i;
    *gray = malloc(w*h * sizeof *gray);
    if (*gray == NULL)
      return -3;
    for (i=0; i < w*h; ++i) {
        (*gray)[i] = (rgb[i][0]+rgb[i][1]+rgb[i][2]) / 3;
    }
    return 0;
}






struct ColorNode {
    unsigned char color[3];
    struct ColorNode *left, *right;
};

/***
 * Functions and structures related to color table manipulation.
 * No optimization here; just a linear search in findColorEntry().
 * Maybe some sort of hash table would speed it up?
 */

static void initColorTable(struct ColorTable *tab)
{
    tab->nentries = 0;
}

static int addColorEntry(struct ColorTable *tab, unsigned char data[3])
{
    int tmp = findColorEntry(tab, data);
    if (tmp != -1)
      return tmp;
    if (tab->nentries >= 256)
      return -1;
    memcpy(tab->entries[tab->nentries], data, 3);
    return tab->nentries++;
}

static int findColorEntry(struct ColorTable *tab, unsigned char data[3])
{
    int i;
    for (i=0; i < tab->nentries; ++i) {
        if (memcmp(tab->entries[i], data, 3) == 0)
          return i;
    }
    return -1;
}


int ReadGenRGB(const char *fname, unsigned char (**data)[3], int *w, int *h)
{
    FILE *fp = fopen(fname, "rb");
    int b1, b2;

    *data = NULL;

    if (!fp)
      return -1;

    b1 = getc(fp);
    b2 = getc(fp);
    fclose(fp);

    if (b1 == 'P' && ('1' <= b2 && b2 <= '6'))
      return ReadPPM(fname, data, w, h);
    else if (b1 == 'B' && b2 == 'M')
      return ReadBMP(fname, data, w, h);
    else
      return -4;
}

int ReadGenGray(const char *fname, unsigned char **data, int *w, int *h)
{
    FILE *fp = fopen(fname, "rb");
    int b1, b2;

    *data = NULL;

    if (!fp)
      return -1;

    b1 = getc(fp);
    b2 = getc(fp);
    fclose(fp);

    if (b1 == 'P' && ('1' <= b2 && b2 <= '6'))
      return ReadPGM(fname, data, w, h);
    else if (b1 == 'B' && b2 == 'M') {
        unsigned char (*temp)[3];
        int rc = ReadBMP(fname, &temp, w, h);
        if (rc != 0) {
            if (temp != NULL)
              free(temp);
            return rc;
        }
        if (RGB2Gray(temp, data, *w, *h)) {
            free(temp);
            return -3;
        }
        return 0;
    }
    else
      return -4;
}

