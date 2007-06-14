/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/ncdump/vardata.c,v 1.12 2005/07/22 23:01:39 russ Exp $
 *********************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <netcdf.h>
#include "ncdump.h"
#include "dumplib.h"

/* Output the data for a single variable, in CDL syntax. */
int
vardata_write(
     const ncvar_t *vp,	/* variable */
     size_t vdims[],		/* variable dimension sizes */
     int ncid,			/* netcdf id */
     int varid			/* variable id */
     )
{
    int id;
    int ir;
    size_t nels;
    size_t ncols;
    size_t nrows;
    int vrank = vp->ndims;
    int d1,d2,d3,mone,i;
    char name_l[NC_MAX_NAME];
    int effective_dims=0;

    /*pointer to an array containing the array in double precision*/
    double *dvals, *dvals1;

    d1=1;d2=1;d3=1;
    for(i=vrank-1;i>=0;i--) {
      if(vdims[i]>1) {
	effective_dims++;
	if(effective_dims == 1) d1=vdims[i];
	else if(effective_dims == 2) d2=vdims[i];
	else if(effective_dims == 3) d3=vdims[i];
      }
    }

    if(effective_dims>3) {
      printf(vp->name);
      printf(": write_matrix doesn't support >3 diminsional arrays\n");
	return 1;
    }

    nels = 1;
    for (id = 0; id < vrank; id++) {
	nels *= vdims[id];	/* total number of values for variable */
    }

    if (vrank < 1) {
 	nels=1;
    } 
    /*this shouldn't happen, but just in case */
    if ( nels < 1) {
      printf("total array size is less than one??\n");
      return 1;
    }

    /* allocate values array */
    dvals=malloc(nels * sizeof (double));
    if(dvals == NULL){
      printf("couldn't allocate memory");
      return 1;
    }
    dvals1=dvals;
    nc_get_var_double(ncid, varid, dvals);
    if(dvals != dvals1)return 1;

    /* moved above ^^
    d1=1;
    d2=1;
    d3=1;
    if(vrank >= 1)d1=vdims[2];
    if(vrank >= 2)d2=vdims[1];
    if(vrank >= 3)d3=vdims[0];
    */

/*
    for (i=0;i<NC_MAX_NAME && vp->name[i] != ' ' && vp->name[i] != '\0';i++){
	name_l[i]=vp->name[i];
    }*/
    i=sprintf(name_l,"%s",vp->name);

    mone=-1;
    write_array_(&name_l,&mone,&mone,dvals,&d1,&d2,&d1,&d2,&d3,i);
    free(dvals);
    return 0;
}
