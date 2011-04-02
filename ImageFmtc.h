
#ifndef H_IMAGEFMTC
 #define H_IMAGEFMTC
 #ifdef __cplusplus
   extern "C" {
 #endif


/***
 * IMAGE FILE READING FUNCTIONS
 */

/* Attempt to load an RGB BMP file. */
int ReadBMP(const char *fname, unsigned char (**data)[3], int *w, int *h);

/* Attempt to load a PPM (or PGM) file.  Calls the three functions below. */
int ReadPPM(const char *fname, unsigned char (**data)[3], int *w, int *h);

/* Attempt to load a "plain" PPM file. */
int ReadPPM3(const char *fname, unsigned char (**data)[3], int *w, int *h);

/* Attempt to load a "binary" PPM file. */
int ReadPPM6(const char *fname, unsigned char (**data)[3], int *w, int *h);

/* Attempt to load a grayscale PGM file.  Calls the two functions below. */
int ReadPGM(const char *fname, unsigned char **data, int *w, int *h);

/* Attempt to load a "plain" PGM file. */
int ReadPGM2(const char *fname, unsigned char **data, int *w, int *h);

/* Attempt to load a "binary" PGM file. */
int ReadPGM5(const char *fname, unsigned char **data, int *w, int *h);

/* Attempt to load a "plain" PBM file. */
int ReadPBM1(const char *fname, unsigned char **data, int *w, int *h);

/* Attempt to load a "binary" PBM file. */
int ReadPBM4(const char *fname, unsigned char **data, int *w, int *h);


/* Attempt to load an RGB image (PGM, PPM, or BMP). */
int ReadGenRGB(const char *fname, unsigned char (**data)[3], int *w, int *h);

/* Attempt to load a grayscale image (PGM, PPM, or BMP). */
int ReadGenGray(const char *fname, unsigned char **data, int *w, int *h);


/***
 * IMAGE FILE WRITING FUNCTIONS
 */

/* Save a 24-bit BMP file. */
int WriteBMP24(const char *fname, unsigned char (*data)[3], int w, int h);

/* Save a 32-bit BMP file (RGB + nulled-out alpha channel). */
int WriteBMP32(const char *fname, unsigned char (*data)[3], int w, int h);

/* Save an 8-bit BMP file. */
int WriteBMP8(const char *fname, unsigned char (*data)[3], int w, int h);

/* Save a compressed 8-bit BMP file. */
int WriteBMP8C(const char *fname, unsigned char (*data)[3], int w, int h);

/* Save an already-palettized 8-bit BMP file. */
int WritePalBMP8(const char *fname, unsigned char *data, int w, int h,
                 unsigned char (*palette)[3], int npalette);

/* Save an already-palettized compressed 8-bit BMP file. */
int WritePalBMP8C(const char *fname, unsigned char *data, int w, int h,
                  unsigned char (*palette)[3], int npalette);

/* Save "plain" and "binary" formatted PPM files, respectively. */
int WritePPM3(const char *fname, unsigned char (*data)[3], int w, int h);
int WritePPM6(const char *fname, unsigned char (*data)[3], int w, int h);

/* Save "plain" and "binary" formatted PGM files, respectively. */
int WritePGM2(const char *fname, unsigned char *data, int w, int h);
int WritePGM5(const char *fname, unsigned char *data, int w, int h);

/* Save "plain" and "binary" formatted PBM files, respectively. */
int WritePBM1(const char *fname, unsigned char *data, int w, int h);
int WritePBM4(const char *fname, unsigned char *data, int w, int h);


/***
 * IMAGE CONVERSION FUNCTIONS
 */

/* Convert grayscale to RGB color. */
int Gray2RGB(unsigned char *gray, unsigned char (**rgb)[3], int w, int h);

/* Convert RGB color to grayscale. */
int RGB2Gray(unsigned char (*rgb)[3], unsigned char **gray, int w, int h);


 #ifdef __cplusplus
   }
 #endif
#endif
