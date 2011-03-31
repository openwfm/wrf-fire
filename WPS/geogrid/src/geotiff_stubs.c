
#ifndef _TESTING_GEOTIFF

#ifdef _HAS_GEOTIFF

#include "geotiff_stubs.h"

#include <geotiff.h>
#include <geo_normalize.h>
#include <geovalues.h>
#include <math.h>

int num_open_geotiff_files=-1;
TIFF *open_geotiff_files[MAX_OPEN_GEOTIFF_FILES];

void get_tile_size(TIFF *filep, int *x, int *y) {
  if( TIFFIsTiled(filep) ) {
    TIFFGetField(filep,TIFFTAG_TILEWIDTH,x);
    TIFFGetField(filep,TIFFTAG_TILELENGTH,y);
  }
  else {
    TIFFGetField(filep,TIFFTAG_IMAGEWIDTH,x);
    *y=1;//TIFFStripSize(filep);
  }
}

TIFF *get_tiff_file(int filenum) {
#ifdef _GEOTIFF_EXTRA_DEBUG
  fprintf(stdout,"getting open geotiff file %i\n",filenum);
#endif
  if(filenum < 0 || filenum > num_open_geotiff_files)
    return( (TIFF*) 0);
  else
    return(open_geotiff_files[filenum]);
}

void geotiff_header(
    int *filen,
    int *nx,
    int *ny,
    int *nz,
    int *tilex,
    int *tiley,
    int *proj,
    fltType *dx,
    fltType *dy,
    int *known_x,
    int *known_y,
    fltType *known_lat,
    fltType *known_lon,
    fltType *stdlon,
    fltType *truelat1,
    fltType *truelat2,
    int *status
    ) {

  double tmpdble;
  int tmpint;
  double x,y;
  TIFF *filep=get_tiff_file(*filen);
  
  if(!filep){
    *status=1;
    return;
  }

  *status=0;
  GTIF *gtifh=GTIFNew(filep);
  GTIFDefn g;
  GTIFGetDefn(gtifh,&g);

  *nx=I_INVALID;
  *ny=I_INVALID;
  *nz=I_INVALID;
  *tilex=I_INVALID;
  *tiley=I_INVALID;
  *proj=I_INVALID;
  *dx=F_INVALID;
  *dy=F_INVALID;
  *known_x=I_INVALID;
  *known_y=I_INVALID;
  *known_lat=F_INVALID;
  *known_lon=F_INVALID;
  *stdlon=F_INVALID;
  *truelat1=F_INVALID;
  *truelat2=F_INVALID;

  TIFFGetField(filep,TIFFTAG_IMAGEWIDTH,&tmpint);
  *nx=tmpint;
  TIFFGetField(filep,TIFFTAG_IMAGELENGTH,&tmpint);
  *ny=tmpint;

  if( !TIFFGetField(filep,TIFFTAG_IMAGEDEPTH,nz) ) *nz=1;
  
  x=0;
  y=0;
  *known_x=x+1;
  *known_y=y;
  if ( !GTIFImageToPCS( gtifh, &x, &y) ) *status=1;
  
  if(g.Model == ModelTypeGeographic) {
    *proj=(int) regular_ll;
    *known_lon=x;
    *known_lat=y;
  }
  else {
    switch (g.CTProjection) {
      case CT_AlbersEqualArea:
        *proj=(int) albers_nad83;
        break;
      case CT_TransverseMercator:
        *proj=(int) mercator;
        break;
      case CT_PolarStereographic:
        *proj=(int) polar;
	break;
      case CT_LambertConfConic:
	*proj=(int) lambert;
	break;
      default:
        fprintf(stderr,"Unsupported projection ID: %i\n",g.CTProjection);
        *status=1;
    }

    if( !GTIFProj4ToLatLong( &g, 1, &x, &y) ) *status=1;
    *known_lon=x;
    *known_lat=y; 
  }
  

  GTIFKeyGet(gtifh,ProjStdParallel1GeoKey,&tmpdble,0,1);
  *truelat1=tmpdble;
  GTIFKeyGet(gtifh,ProjStdParallel2GeoKey,&tmpdble,0,1);
  *truelat2=tmpdble;
  GTIFKeyGet(gtifh,ProjCenterLongGeoKey,&tmpdble,0,1);
  *stdlon=tmpdble;

  //TIFFGetField(filep,TIFFTAG_XRESOLUTION,&tmpdble);
  //*dx=tmpdble;
  //TIFFGetField(filep,TIFFTAG_YRESOLUTION,&tmpdble);
  //*dy=tmpdble;

  short count;
  double *scale;
  TIFFGetField(filep,TIFFTAG_GEOPIXELSCALE,&count,&scale);
  *dx=scale[0];
  GTIFKeyGet(gtifh,TIFFTAG_GEOPIXELSCALE,&tmpdble,1,1);
  *dy=scale[1];

  get_tile_size(filep,tilex,tiley);

  GTIFFree(gtifh);
}

void geotiff_open(char *filename,int *filep,int *status) {
  *filep=++num_open_geotiff_files;
#ifdef _GEOTIFF_EXTRA_DEBUG
  fprintf(stdout,"opening %s as %i.\n",filename,*filep);
#endif
  *status=0;
  //if (!_HAVE_PROJ4) {
  //  *status=1;
  //}
  open_geotiff_files[*filep]=XTIFFOpen(filename,"r");
  if (!open_geotiff_files[*filep]) *status=1;
}

void geotiff_close(int *filep) {
  TIFF *file=get_tiff_file(*filep);
  if (file)  {
  XTIFFClose(open_geotiff_files[*filep]);
  open_geotiff_files[*filep]=(TIFF *)0;
  }
  *filep=-1;
}

int read_tile_tiled(TIFF *filep,int xtile,int ytile,void *buffer) {
  int status,result;
  status=0;
  result=TIFFReadTile(filep,buffer,xtile,ytile,0,0);
#ifdef _GEOTIFF_EXTRA_DEBUG
fprintf(stdout,"xtile=%i,ytile=%i\n",xtile,ytile);
#endif
  if(result == -1) status=99;
  return(status);
}

int read_tile_stripped(TIFF *filep,int tilesize,int ytile,void *buffer) {
  int status,result;
  status=0;
  result=TIFFReadEncodedStrip(filep,ytile,buffer,tilesize);
#ifdef _GEOTIFF_EXTRA_DEBUG
fprintf(stdout,"ytile=%i\n",ytile);
#endif
  if(result == -1) status=99;
  return(status);
}

void read_geotiff_tile(int *filen, int *xtile, int *ytile, 
                       int *nx, int *ny, int *nz, fltType *buffer, 
		       int *status) {
  int tx,ty,mx,my,i;
  unsigned short np,sf;
  int tilesize;
  void *tilebuf;
  int xt,yt,ntx,nty;

  TIFF *filep=get_tiff_file(*filen);
  if(!filep) {
    *status=1;
    return;
  }
  get_tile_size(filep,&tx,&ty);
  TIFFGetField(filep,TIFFTAG_IMAGEWIDTH,&mx);
  TIFFGetField(filep,TIFFTAG_IMAGELENGTH,&my);

  if(tx != *nx || ty != *ny || *nz != 1) {
    *status=99;
    return;
  }

  xt=*xtile-1;
  yt=-*ytile;

  ntx=ceil(((float) mx) / tx);
  nty=ceil(((float) my) / ty);
  if(xt >= ntx || xt < 0 || yt >= nty || yt < 0) {
#ifdef _GEOTIFF_EXTRA_DEBUG
    fprintf(stdout,"TILE: %i %i %i %i\n",*xtile,xt,*ytile,yt);
#endif
    *status=1;
    return;
  }
 
  TIFFGetField(filep,TIFFTAG_SAMPLEFORMAT,&sf);
  TIFFGetField(filep,TIFFTAG_BITSPERSAMPLE,&np);
  tilesize=tx*ty;
  tilebuf=_TIFFmalloc(tilesize*np/8);

  for(i=0;i<tilesize;i++) buffer[i]=-1;
  for(i=0;i<tilesize*np/8;i++) *((unsigned char *)tilebuf)=0x00;

  if(TIFFIsTiled(filep)) {
    *status=read_tile_tiled(filep,xt,yt,tilebuf);
  }
  else {
    *status=read_tile_stripped(filep,tilesize*np/8,yt,tilebuf);
  }

  switch (sf) {
    case SAMPLEFORMAT_UINT:
      switch (np) {
	case 8:
	  CONVERT_BUFFER(uint8,1)
	  break;
	case 16:
	  CONVERT_BUFFER(uint16,2)
	  break;
	case 32:
	  CONVERT_BUFFER(uint32,4)
	  break;
	default:
	  *status=1;
      }
      break;
    case SAMPLEFORMAT_INT:
      switch (np) {
	case 8:
	  CONVERT_BUFFER(int8,1)
	  break;
	case 16:
	  CONVERT_BUFFER(int16,2)
	  break;
	case 32:
          CONVERT_BUFFER(int32,4)
	  break;
	default:
	  *status=1;
      }
      break;
    case SAMPLEFORMAT_IEEEFP:
      switch (np) {
	case 8*sizeof(float):
          CONVERT_BUFFER(float,sizeof(float))
	  break;
	case 8*sizeof(double):
	  CONVERT_BUFFER(double,sizeof(double))
	  break;
	default:
	  *status=1;
      }
      break;
    default:
      *status=1;
      break;
  }

  _TIFFfree(tilebuf);
}

#else
int dummy_c_function() {
  return 0;
}
#endif

#else //testing only

#include <stdio.h>
#include "geotiff_stubs.h"

void geotiff_header(int *filep, int *nx, int *ny, int *nz, int *tilex, int *tiley,  \
                      int *proj, fltType *dx, fltType *dy, int *known_x, int *known_y, \
		      fltType *known_lat, fltType *known_lon, fltType *stdlon,         \
		      fltType *truelat1, fltType *truelat2, int *status) {

  *nx=10000;
  *ny=12000;
  *nz=1;
  *tilex=100;
  *tiley=120;
  *proj=regular_ll;
  *dx=30;
  *dy=30;
  *known_x=0;
  *known_y=0;
  *known_lat=F_INVALID;
  *known_lon=F_INVALID;
  *stdlon=F_INVALID;
  *truelat1=F_INVALID;
  *truelat2=F_INVALID;
  *status=0;
}
void geotiff_open(char *filename, int *filep, int *status){
  *status=0;
}
void geotiff_close(int *filep) {
  filep=0;
}
void get_pointer_size(int *psize){
  int *i;
  *psize=sizeof(i);
}
void read_geotiff_tile(int *filep, int *xtile, int *ytile, int *nx, int *ny, int *nz, \
                       fltType *buffer, int *status) {
  int i;
  fltType val;
  val=*xtile + (*ytile);
  fprintf(stdout,"Writing %f to tile (%i,%i).\n",val,*xtile,*ytile);
  for(i=0;i< (*nx) * (*ny) * (*nz) ; i++) {
    buffer[i]=val;
  }
  *status=0;
}

#endif
