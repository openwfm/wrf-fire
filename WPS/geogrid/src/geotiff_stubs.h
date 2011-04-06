/******************************************************************************
 * geotiff_stubs.h
 * Jonathan Beezley (jon.beezley@gmail.com)
 * April 4, 2011
 *
 * Provides several stubs, callable from fortran,  to the geotiff library.
 ******************************************************************************/

#ifdef _TESTING_GEOTIFF
#define TIFF int
#else
#include <xtiffio.h>
#include <geo_config.h>
#endif

#ifndef GEOTIFF_STUBS_H
#define GEOTIFF_STUBS_H

// cpp defines for fortran name mangling
#ifdef _UNDERSCORE
#define geotiff_header geotiff_header_
#define geotiff_open geotiff_open_
#define geotiff_close geotiff_close_
#define read_geotiff_tile read_geotiff_tile_
#endif
#ifdef _DOUBLEUNDERSCORE
#define geotiff_header geotiff_header__
#define geotiff_open geotiff_open__
#define geotiff_close geotiff_close__
#define read_geotiff_tile read_geotiff_tile__
#endif

/* In case someone wants to compile WPS with double precision... */
typedef float fltType;

/* Function declarations */
#ifdef __cplusplus
extern "C" {
#endif
  void geotiff_header(int *filep, int *nx, int *ny, int *nz, int *tilex, int *tiley,  \
                      int *proj, fltType *dx, fltType *dy, fltType *known_x, fltType *known_y, \
		      fltType *known_lat, fltType *known_lon, fltType *stdlon,         \
		      fltType *truelat1, fltType *truelat2, int *orientation, int *status);
  void get_tile_size(TIFF *filep, int *x, int *y);
  void geotiff_open(char *filename, int *filep, int *status);
  void geotiff_close(int *filep);
  int read_tile_tiled(TIFF *filep, int xtile, int ytile, void *buffer);
  int read_tile_stripped(TIFF *filep, int xtile, int ytile, void *buffer);
  void read_geotiff_tile(int *filep, int *xtile, int *ytile, int *nx, int *ny, int *nz, \
                         fltType *buffer, int *status);
  TIFF *get_tiff_file(int filenum);
#ifdef __cplusplus
}
#endif

/* known projections */
typedef enum {
  lambert=1,           /* Lambert Conformal (geogrid code = PROJ_LC) */
  polar=2,             /* Polar Stereographic (geogrid code = PROJ_PS) */
  mercator=3,          /* Mercator (geogrid code = PROJ_MERC) */
  regular_ll=4,        /* Cylindrical (geographic) Lat/Lon (geogrid code = PROJ_LATLON) */
  albers_nad83=5       /* Albers Equal Area Conic (geogrid code PROJ_ALBERS_NAD83) */
} Projection;

/* invalid values, these are shared with variables in geotiff_module.F */
const int I_INVALID=-1;
const fltType F_INVALID=-1;

/* The maximum number of geotiff files that can be opened at once */
#define MAX_OPEN_GEOTIFF_FILES 64

/* For geographic projections (not regular_ll), we need libproj4 support. */
#ifdef HAVE_LIBPROJ
const int _HAVE_PROJ4=1;
#else
const int _HAVE_PROJ4=0;
#endif

/* Convert a buffer from a given type in the image to floats. */
#define CONVERT_BUFFER(T,n)                          \
  for(i=0;i<tilesize;i++) {                     \
    buffer[i] = (fltType) ( ( (T*) tilebuf)[i] );     \
  }

#endif
