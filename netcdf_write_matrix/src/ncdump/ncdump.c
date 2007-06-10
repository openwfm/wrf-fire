/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/README file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/ncdump/ncdump.c,v 1.26 2006/01/11 17:00:06 ed Exp $
 *********************************************************************/

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include <netcdf.h>
#include "ncdump.h"
#include "dumplib.h"
#include "vardata.h"

#define int64_t long long
#define uint64_t unsigned long long

static void usage(void);
static char* name_path(const char* path);
static const char* type_name(nc_type  type);
static void tztrim(char* ss);
static void pr_att_string(size_t len, const char* string);
static void pr_att_vals(nc_type  type, size_t len, const double* vals);
static void pr_att(int ncid, int varid, const char *varname, int ia);
static void pr_attx(int ncid, int varid, int ia);
static void do_ncdump(const char* path, fspec_t* specp);
static void do_ncdumpx(const char* path, fspec_t* specp);
static void make_lvars(char* optarg, fspec_t* fspecp);
static void set_sigdigs( const char* optarg);
static void set_precision( const char *optarg);
int GL_WRITE_MATRIX;
int main(int argc, char** argv);

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

char *progname;

static void
usage(void)
{
#define USAGE   "\
  [-c]             Coordinate variable data and header information\n\
  [-h]             Header information only, no data\n\
  [-v var1[,...]]  Data for variable(s) <var1>,... only\n\
  [-b [c|f]]       Brief annotations for C or Fortran indices in data\n\
  [-f [c|f]]       Full annotations for C or Fortran indices in data\n\
  [-l len]         Line length maximum in data section (default 80)\n\
  [-n name]        Name for netCDF (default derived from file name)\n\
  [-p n[,n]]       Display floating-point values with less precision\n\
  [-x]             Output XML (NcML) instead of CDL\n\
  [-w]             Output write_matrix formatted file\n\
  file             Name of netCDF file\n"

    (void) fprintf(stderr,
		   "%s [-c|-h] [-v ...] [[-b|-f] [c|f]] [-l len] [-n name] [-p n[,n]] [-x] file\n%s",
		   progname,
		   USAGE);
    
    (void) fprintf(stderr,
                 "netcdf library version %s\n",
                 nc_inq_libvers());
    exit(EXIT_FAILURE);
}


/* 
 * convert pathname of netcdf file into name for cdl unit, by taking 
 * last component of path and stripping off any extension.
 */
static char *
name_path(const char *path)
{
    const char *cp;
    char *new;
    char *sp;

#ifdef vms
#define FILE_DELIMITER ']'
#endif    
#ifdef MSDOS
#define FILE_DELIMITER '\\'
#endif    
#ifndef FILE_DELIMITER /* default to unix */
#define FILE_DELIMITER '/'
#endif
    cp = strrchr(path, FILE_DELIMITER);
    if (cp == 0)		/* no delimiter */
      cp = path;
    else			/* skip delimeter */
      cp++;
    new = (char *) emalloc((unsigned) (strlen(cp)+1));
    (void) strcpy(new, cp);	/* copy last component of path */
    if ((sp = strrchr(new, '.')) != NULL)
      *sp = '\0';		/* strip off any extension */
    return new;
}


static const char *
type_name(nc_type type)
{
    switch (type) {
      case NC_BYTE:
	return "byte";
      case NC_CHAR:
	return "char";
      case NC_SHORT:
	return "short";
      case NC_INT:
	return "int";
      case NC_FLOAT:
	return "float";
      case NC_DOUBLE:
	return "double";
#ifdef USE_NETCDF4
      case NC_UBYTE:
	return "ubyte";
      case NC_USHORT:
	return "ushort";
      case NC_UINT:
	return "uint";
      case NC_INT64:
	return "long";
      case NC_UINT64:
	return "ulong";
      case NC_STRING:
	return "string";
      case NC_VLEN:
	return "vlen";
      case NC_OPAQUE:
	return "opaque";
      case NC_COMPOUND:
	return "compound";
#endif
      default:
	error("type_name: bad type %d", type);
	return "bogus";
    }
}


/*
 * Remove trailing zeros (after decimal point) but not trailing decimal
 * point from ss, a string representation of a floating-point number that
 * might include an exponent part.
 */
static void
tztrim(char *ss)
{
    char *cp, *ep;
    
    cp = ss;
    if (*cp == '-')
      cp++;
    while(isdigit((int)*cp) || *cp == '.')
      cp++;
    if (*--cp == '.')
      return;
    ep = cp+1;
    while (*cp == '0')
      cp--;
    cp++;
    if (cp == ep)
      return;
    while (*ep)
      *cp++ = *ep++;
    *cp = '\0';
    return;
}


/* 
 * Emit initial line of output for CDL, including format variant
 */
void pr_fmtvariant(int ncid, const char *path, const fspec_t* specp)
{
    int format_version;
    char *format_str;
    
    NC_CHECK( nc_inq_format(ncid, &format_version) );
    switch(format_version) {
    case NC_FORMAT_CLASSIC:
	format_str = "classic";
	break;
    case NC_FORMAT_64BIT:
	format_str = "64bit";
	break;
    case NC_FORMAT_NETCDF4:
	format_str = "netCDF4";
	break;
    case NC_FORMAT_NETCDF4_CLASSIC:
	format_str = " netCDF4_classic";
	break;
    default:
	error("unrecognized format: %s", path);
	break;
    }
    if (format_version == NC_FORMAT_CLASSIC) {
	Printf ("netcdf %s {\n", specp->name);
    } else {
	Printf ("netcdf %s { // format variant: %s \n", specp->name, format_str);
    }
}


/* 
 * Emit initial line of output for NcML, including format variant
 */
void pr_fmtvariantx(int ncid, const char *path)
{
    int format_version;
    char *format_str;
    
    NC_CHECK( nc_inq_format(ncid, &format_version) );
    switch(format_version) {
    case NC_FORMAT_CLASSIC:
	format_str = "classic";
	break;
    case NC_FORMAT_64BIT:
	format_str = "64bit";
	break;
    case NC_FORMAT_NETCDF4:
	format_str = "netCDF4";
	break;
    case NC_FORMAT_NETCDF4_CLASSIC:
	format_str = " netCDF4_classic";
	break;
    default:
	error("unrecognized format: %s", path);
	break;
    }

    Printf("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<netcdf xmlns=\"http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2\" format=\"%s\" location=\"%s\">\n", 
	   format_str, path);
}


/*
 * Print attribute string, for text attributes.
 */
static void
pr_att_string(
     size_t len,
     const char *string
     )
{
    int iel;
    const char *cp;
    const char *sp;
    unsigned char uc;

    cp = string;
    Printf ("\"");
    /* adjust len so trailing nulls don't get printed */
    sp = cp + len - 1;
    while (len != 0 && *sp-- == '\0')
	len--;
    for (iel = 0; iel < len; iel++)
	switch (uc = *cp++ & 0377) {
	case '\b':
	    Printf ("\\b");
	    break;
	case '\f':
	    Printf ("\\f");
	    break;
	case '\n':		/* generate linebreaks after new-lines */
	    Printf ("\\n\",\n\t\t\t\"");
	    break;
	case '\r':
	    Printf ("\\r");
	    break;
	case '\t':
	    Printf ("\\t");
	    break;
	case '\v':
	    Printf ("\\v");
	    break;
	case '\\':
	    Printf ("\\\\");
	    break;
	case '\'':
	    Printf ("\\'");
	    break;
	case '\"':
	    Printf ("\\\"");
	    break;
	default:
	    if (iscntrl(uc))
	        Printf ("\\%03o",uc);
	    else
	        Printf ("%c",uc);
	    break;
	}
    Printf ("\"");

}


/*
 * Print list of attribute values, for numeric attributes.  Attribute values
 * must be printed with explicit type tags, because CDL doesn't have explicit
 * syntax to declare an attribute type.
 */
static void
pr_att_vals(
     nc_type type,
     size_t len,
     const double *vals
     )
{
    int iel;
    signed char sc;
    short ss;
    int ii;
    char gps[30];
    float ff;
    double dd;
#ifdef USE_NETCDF4
    unsigned char uc;
    unsigned short us;
    unsigned int ui;
    int64_t i64;
    uint64_t ui64;
#endif /* USE_NETCDF4 */
    if (len == 0)
	return;
    for (iel = 0; iel < len-1; iel++) {
	switch (type) {
	case NC_BYTE:
	    sc = (signed char) vals[iel] & 0377;
	    Printf ("%db, ", sc);
	    break;
	case NC_SHORT:
	    ss = vals[iel];
	    Printf ("%ds, ", ss);
	    break;
	case NC_INT:
	    ii = (int) vals[iel];
	    Printf ("%d, ", ii);
	    break;
	case NC_FLOAT:
	    ff = vals[iel];
	    (void) sprintf(gps, float_att_fmt, ff);
	    tztrim(gps);	/* trim trailing 0's after '.' */
	    Printf ("%s, ", gps);
	    break;
	case NC_DOUBLE:
	    dd = vals[iel];
	    (void) sprintf(gps, double_att_fmt, dd);
	    tztrim(gps);
	    Printf ("%s, ", gps);
	    break;
#ifdef USE_NETCDF4
	case NC_UBYTE:
	    uc = vals[iel];
	    Printf ("%udub, ", uc);
	    break;
	case NC_USHORT:
	    us = vals[iel];
	    Printf ("%huus, ", us);
	    break;
	case NC_UINT:
	    ui = vals[iel];
	    Printf ("%u, ", ui);
	    break;
	case NC_INT64:
	    i64 = vals[iel];
	    Printf ("%lldL, ", i64);
	    break;
	case NC_UINT64:
	    ui64 = vals[iel];
	    Printf ("%lluUL, ", ui64);
	    break;
#endif
	default:
	    error("pr_att_vals: bad type");
	}
    }
    switch (type) {
    case NC_BYTE:
	sc = (signed char) vals[iel] & 0377;
	Printf ("%db", sc);
	break;
    case NC_SHORT:
	ss = vals[iel];
	Printf ("%ds", ss);
	break;
    case NC_INT:
	ii = (int) vals[iel];
	Printf ("%d", ii);
	break;
    case NC_FLOAT:
	ff = vals[iel];
	(void) sprintf(gps, float_att_fmt, ff);
	tztrim(gps);
	Printf ("%s", gps);
	break;
    case NC_DOUBLE:
	dd = vals[iel];
	(void) sprintf(gps, double_att_fmt, dd);
	tztrim(gps);
	Printf ("%s", gps);
	break;
#ifdef USE_NETCDF4
	case NC_UBYTE:
	    uc = vals[iel];
	    Printf ("%udub", uc);
	    break;
	case NC_USHORT:
	    us = vals[iel];
	    Printf ("%huus", us);
	    break;
	case NC_UINT:
	    ui = vals[iel];
	    Printf ("%u", ui);
	    break;
	case NC_INT64:
	    i64 = vals[iel];
	    Printf ("%lldL", i64);
	    break;
	case NC_UINT64:
	    ui64 = vals[iel];
	    Printf ("%lluUL", ui64);
	    break;
#endif
    default:
	error("pr_att_vals: bad type");
    }
}

#ifndef HAVE_STRLCAT
/*	$OpenBSD: strlcat.c,v 1.12 2005/03/30 20:13:52 otto Exp $	*/

/*
 * Copyright (c) 1998 Todd C. Miller <Todd.Miller@courtesan.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Appends src to string dst of size siz (unlike strncat, siz is the
 * full size of dst, not space left).  At most siz-1 characters
 * will be copied.  Always NUL terminates (unless siz <= strlen(dst)).
 * Returns strlen(src) + MIN(siz, strlen(initial dst)).
 * If retval >= siz, truncation occurred.
 */
size_t
strlcat(char *dst, const char *src, size_t siz)
{
	char *d = dst;
	const char *s = src;
	size_t n = siz;
	size_t dlen;

	/* Find the end of dst and adjust bytes left but don't go past end */
	while (n-- != 0 && *d != '\0')
		d++;
	dlen = d - dst;
	n = siz - dlen;

	if (n == 0)
		return(dlen + strlen(s));
	while (*s != '\0') {
		if (n != 1) {
			*d++ = *s;
			n--;
		}
		s++;
	}
	*d = '\0';

	return(dlen + (s - src));	/* count does not include NUL */
}
#endif /* ! HAVE_STRLCAT */


/*
 * Print list of numeric attribute values to string for use in NcML output.
 */
static void
pr_att_valsx(
     nc_type type,
     size_t len,
     const double *vals,
     char *attvals,		/* returned string */
     size_t attvalslen		/* size of attvals buffer, assumed
				   large enough to hold all len
				   blank-separated values */
     )
{
    int iel;
    double dd;
    int ii;
#ifdef USE_NETCDF4
    unsigned int ui;
    int64_t i64;
    uint64_t ui64;
#endif /* USE_NETCDF4 */

    attvals[0]='\0';
    if (len == 0)
	return;
    for (iel = 0; iel < len; iel++) {
	char gps[50];
	switch (type) {
	case NC_BYTE:
	case NC_SHORT:
	case NC_INT:
	    ii = vals[iel];
	    (void) sprintf(gps, "%d", ii);
	    (void) strlcat(attvals, gps, attvalslen);
	    (void) strlcat(attvals, iel < len-1 ? " " : "", attvalslen);
	    break;
#ifdef USE_NETCDF4
	case NC_UBYTE:
	case NC_USHORT:
	case NC_UINT:
	    ui = vals[iel];
	    (void) sprintf(gps, "%u", ui);
	    (void) strlcat(attvals, gps, attvalslen);
	    (void) strlcat(attvals, iel < len-1 ? " " : "", attvalslen);
	    break;
	case NC_INT64:
	    /* TODO, can't really store a 64-bit int in a double so need different function */
	    i64 = vals[iel];
	    (void) sprintf(gps, "%lld", i64);
	    (void) strlcat(attvals, gps, attvalslen);
	    (void) strlcat(attvals, iel < len-1 ? " " : "", attvalslen);
	    break;
	case NC_UINT64:
	    /* TODO, can't really store a 64-bit int in a double so need different function */
	    ui64 = vals[iel];
	    (void) sprintf(gps, "%llu", ui64);
	    (void) strlcat(attvals, gps, attvalslen);
	    (void) strlcat(attvals, iel < len-1 ? " " : "", attvalslen);
	    break;
#endif /* USE_NETCDF4 */
	case NC_FLOAT:
	case NC_DOUBLE:
	    dd = vals[iel];
	    (void) sprintf(gps, double_att_fmt, dd);
	    (void) strlcat(attvals, gps, attvalslen);
	    (void) strlcat(attvals, iel < len-1 ? " " : "", attvalslen);
	    break;
	default:
	    error("pr_att_valsx: bad type");
	}
    }
}


static void
pr_att(
    int ncid,
    int varid,
    const char *varname,
    int ia
    )
{
    ncatt_t att;			/* attribute */
	    
    NC_CHECK( nc_inq_attname(ncid, varid, ia, att.name) );

    Printf ("\t\t%s:%s = ", varname, att.name);

    NC_CHECK( nc_inq_att(ncid, varid, att.name, &att.type, &att.len) );

    if (att.len == 0) {	/* show 0-length attributes as empty strings */
	att.type = NC_CHAR;
    }
    switch (att.type) {
    case NC_CHAR:
	att.string = (char *) emalloc(att.len + 1);
	NC_CHECK( nc_get_att_text(ncid, varid, att.name, att.string ) );
	pr_att_string(att.len, att.string);
	free(att.string);
	break;
    default:
	att.vals = (double *) emalloc((att.len + 1) * sizeof(double));
	NC_CHECK( nc_get_att_double(ncid, varid, att.name, att.vals ) );
	pr_att_vals(att.type, att.len, att.vals);
	free(att.vals);
	break;
    }
    Printf (" ;\n");
}


static void
pr_attx(
    int ncid,
    int varid,
    int ia
    )
{
    ncatt_t att;			/* attribute */
    char *attvals;
    int attvalslen = 0;

    NC_CHECK( nc_inq_attname(ncid, varid, ia, att.name) );
    NC_CHECK( nc_inq_att(ncid, varid, att.name, &att.type, &att.len) );

    /* Put attribute values into a single string, with blanks in between */

    switch (att.type) {
    case NC_CHAR:
#ifdef USE_NETCDF4
	/* fall through */
    case NC_STRING:
#endif
	attvals = (char *) emalloc(att.len + 1);
	NC_CHECK( nc_get_att_text(ncid, varid, att.name, attvals ) );
	break;
#ifdef USE_NETCDF4
	/* TODO
	   case NC_VLEN:
	   case NC_OPAQUE:
	   case NC_COMPOUND:
	*/
#endif

    default:
	att.vals = (double *) emalloc((att.len + 1) * sizeof(double));
	NC_CHECK( nc_get_att_double(ncid, varid, att.name, att.vals ) );
	attvalslen = 20*att.len; /* max 20 chars for each value and blank separator */
	attvals = (char *) emalloc(attvalslen + 1);
	pr_att_valsx(att.type, att.len, att.vals, attvals, attvalslen);
	free(att.vals); 
	free (attvals);
	break;
    }

    Printf ("%s  <attribute name=\"%s\" type=\"%s\" value=\"%s\" />\n", 
	    varid != NC_GLOBAL ? "  " : "", 
	    att.name, 
	    att.type == NC_CHAR ? "String" : type_name(att.type), 
	    attvals);
}


/* Print optional NcML attribute for a variable's shape */
static void
pr_shape(ncvar_t* varp, ncdim_t *dims)
{
    char *shape;
    int shapelen = 0;
    int id;

    if (varp->ndims == 0)
	return;
    for (id = 0; id < varp->ndims; id++) {
	shapelen += strlen(dims[varp->dims[id]].name) + 1;
    }
    shape = (char *) emalloc(shapelen);
    for (id = 0; id < varp->ndims; id++) {
	strlcat(shape, dims[varp->dims[id]].name, shapelen);
	strlcat(shape, id < varp->ndims-1 ? " " : "", shapelen);
    }
    Printf (" shape=\"%s\"", shape);
    free(shape);
}

/* Recursively dump the contents of a group. (Recall that only
 * netcdf-4 format files can have groups. On all other formats, there
 * is just a root group, so recursion will not take place.) */
static void
do_ncdump_rec(int ncid, const char *path, fspec_t* specp)
{
    int ndims;			/* number of dimensions */
    int nvars;			/* number of variables */
    int ngatts;			/* number of global attributes */
    int xdimid;			/* id of unlimited dimension */
    int dimid;			/* dimension id */
    int varid;			/* variable id */
    ncdim_t dims[NC_MAX_DIMS];	/* dimensions */
    size_t vdims[NC_MAX_DIMS];	/* dimension sizes for a single variable */
    ncvar_t var;			/* variable */
    ncatt_t att;			/* attribute */
    int id;			/* dimension number per variable */
    int ia;			/* attribute number */
    int iv;			/* variable number */
    vnode* vlist = 0;		/* list for vars specified with -v option */
    int nc_status;		/* return from netcdf calls */

    /*
     * If any vars were specified with -v option, get list of associated
     * variable ids
     */
    if (specp->nlvars > 0) {
	vlist = newvlist();	/* list for vars specified with -v option */
	for (iv=0; iv < specp->nlvars; iv++) {
	    NC_CHECK( nc_inq_varid(ncid, specp->lvars[iv], &varid) );
	    varadd(vlist, varid);
	}
    }

    /* output initial line, including format variant */
    if (path)
       pr_fmtvariant(ncid, path, specp);

    /*
     * get number of dimensions, number of variables, number of global
     * atts, and dimension id of unlimited dimension, if any
     */
    NC_CHECK( nc_inq(ncid, &ndims, &nvars, &ngatts, &xdimid) );
    /* get dimension info */
    if (ndims > 0)
      Printf ("dimensions:\n");
    for (dimid = 0; dimid < ndims; dimid++) {
	NC_CHECK( nc_inq_dim(ncid, dimid, dims[dimid].name, &dims[dimid].size) );
	if (dimid == xdimid)
	  Printf ("\t%s = %s ; // (%u currently)\n",dims[dimid].name,
		  "UNLIMITED", (unsigned int)dims[dimid].size);
	else
	   Printf ("\t%s = %u ;\n", dims[dimid].name, (unsigned int)dims[dimid].size);
    }

    if (nvars > 0)
	Printf ("variables:\n");
    /* get variable info, with variable attributes */
    for (varid = 0; varid < nvars; varid++) {
	NC_CHECK( nc_inq_varndims(ncid, varid, &var.ndims) );
	var.dims = (int *) emalloc((var.ndims + 1) * sizeof(int));
	NC_CHECK( nc_inq_var(ncid, varid, var.name, &var.type, 0,
			     var.dims, &var.natts) );
	Printf ("\t%s %s", type_name(var.type), var.name);
	if (var.ndims > 0)
	  Printf ("(");
	for (id = 0; id < var.ndims; id++) {
	    Printf ("%s%s",
		    dims[var.dims[id]].name,
		    id < var.ndims-1 ? ", " : ")");
	}
	Printf (" ;\n");

	/* get variable attributes */
	for (ia = 0; ia < var.natts; ia++)
	    pr_att(ncid, varid, var.name, ia); /* print ia-th attribute */
	free(var.dims);
    }


    /* get global attributes */
    if (ngatts > 0)
      Printf ("\n// global attributes:\n");
    for (ia = 0; ia < ngatts; ia++)
	pr_att(ncid, NC_GLOBAL, "", ia); /* print ia-th global attribute */
    
    if (! specp->header_only) {
	if (nvars > 0) {
	    Printf ("data:\n");
	}
	/* output variable data */
	for (varid = 0; varid < nvars; varid++) {
	    /* if var list specified, test for membership */
	    if (specp->nlvars > 0 && ! varmember(vlist, varid))
	      continue;
	    NC_CHECK( nc_inq_varndims(ncid, varid, &var.ndims) );
	    var.dims = (int *) emalloc((var.ndims + 1) * sizeof(int));
	    NC_CHECK( nc_inq_var(ncid, varid, var.name, &var.type, 0,
			    var.dims, &var.natts) );

	    /* If coords-only option specified, don't get data for
	     * non-coordinate vars */
	    if (specp->coord_vals && !iscoordvar(ncid,varid)) {
		continue;
	    }

	    /* Don't get data for record variables if no records have
	     * been written yet */
	    if (isrecvar(ncid, varid) && dims[xdimid].size == 0) {
		continue;
	    }
		
	    /* Collect variable's dim sizes */
	    for (id = 0; id < var.ndims; id++)
		vdims[id] = dims[var.dims[id]].size;
	    var.has_fillval = 1; /* by default, but turn off for bytes */
	    
	    /* get _FillValue attribute */
	    nc_status = nc_inq_att(ncid,varid,_FillValue,&att.type,&att.len);
	    if(nc_status == NC_NOERR &&
	       att.type == var.type && att.len == 1) {
		if(var.type == NC_CHAR) {
		    char fillc;
		    NC_CHECK( nc_get_att_text(ncid, varid, _FillValue,
					      &fillc ) );
		    var.fillval = fillc;
		} else {
		    NC_CHECK( nc_get_att_double(ncid, varid, _FillValue,
						&var.fillval) );
		}
	    } else {
		switch (var.type) {
		case NC_BYTE:
		    /* don't do default fill-values for bytes, too risky */
		    var.has_fillval = 0;
		    break;
		case NC_CHAR:
		    var.fillval = NC_FILL_CHAR;
		    break;
		case NC_SHORT:
		    var.fillval = NC_FILL_SHORT;
		    break;
		case NC_INT:
		    var.fillval = NC_FILL_INT;
		    break;
		case NC_FLOAT:
		    var.fillval = NC_FILL_FLOAT;
		    break;
		case NC_DOUBLE:
		    var.fillval = NC_FILL_DOUBLE;
		    break;
		default:
		    break;
		}
	    }
	    if (GL_WRITE_MATRIX==0 && vardata(&var, vdims, ncid, varid, specp) == -1) {
		error("can't output data for variable %s", var.name);
		NC_CHECK(
			 nc_close(ncid) );
		if (vlist)
		    free(vlist);
		return;
	    }
	    if (GL_WRITE_MATRIX==1 && vardata_write(&var,vdims,ncid,varid)==-1) {
                error("can't output data for variable %s", var.name);
                NC_CHECK(
                         nc_close(ncid) );
                if (vlist)
                    free(vlist);
                return;
            }
	}
    }
    
#ifdef USE_NETCDF4
   /* For netCDF-4 compiles, check to see if the file has any
    * groups. If it does, this function is called recursively on each
    * of them. */
   {
      int g, numgrps, *ncids, format;
      char group_name[NC_MAX_NAME + 1];

      /* Only netCDF-4 files have groups. */
      if ((nc_status = nc_inq_format(ncid, &format)))
	 error("can't get format"); 

      if (format == NC_FORMAT_NETCDF4)
      {
	 /* See how many groups there are. */
	 if ((nc_status = nc_inq_grps(ncid, &numgrps, NULL))) 
	    error("can't read groups"); 
	 
	 /* Allocate memory to hold the list of group ids. */
	 if (!(ncids = malloc(numgrps * sizeof(int))))
	    error("out of memory");
	 
	 /* Get the list of group ids. */
	 nc_status = nc_inq_grps(ncid, NULL, ncids);
	 
	 /* Call this function for each group. */
	 for (g = 0; g < numgrps; g++)
	 {
	    if (nc_inq_grpname(ncids[g], group_name))
	       error("can't find group's name");	       
	    Printf ("\ngroup: %s {\n", group_name);
	    do_ncdump_rec(ncids[g], NULL, specp);
	 }
	 
	 free(ncids);
      }
   }
#endif /* USE_NETCDF4 */

    Printf ("}\n");
    if (vlist)
	free(vlist);
}


static void
do_ncdump(const char *path, fspec_t* specp)
{
   int nc_status;
   int ncid;

   nc_status = nc_open(path, NC_NOWRITE, &ncid);
   if (nc_status != NC_NOERR) {
      error("%s: %s", path, nc_strerror(nc_status));
   }

   do_ncdump_rec(ncid, path, specp);
   NC_CHECK( nc_close(ncid) );
}

static void
do_ncdumpx(const char *path, fspec_t* specp)
{
    int ndims;			/* number of dimensions */
    int nvars;			/* number of variables */
    int ngatts;			/* number of global attributes */
    int xdimid;			/* id of unlimited dimension */
    int dimid;			/* dimension id */
    int varid;			/* variable id */
    ncdim_t dims[NC_MAX_DIMS]; /* dimensions */
    size_t vdims[NC_MAX_DIMS];	/* dimension sizes for a single variable */
    ncvar_t var;		/* variable */
    ncatt_t att;		/* attribute */
    int id;			/* dimension number per variable */
    int ia;			/* attribute number */
    int iv;			/* variable number */
    int ncid;			/* netCDF id */
    vnode* vlist = 0;		/* list for vars specified with -v option */
    int nc_status;		/* return from netcdf calls */

    nc_status = nc_open(path, NC_NOWRITE, &ncid);
    if (nc_status != NC_NOERR) {
	error("%s: %s", path, nc_strerror(nc_status));
    }
    /*
     * If any vars were specified with -v option, get list of associated
     * variable ids
     */
    if (specp->nlvars > 0) {
	vlist = newvlist();	/* list for vars specified with -v option */
	for (iv=0; iv < specp->nlvars; iv++) {
	    NC_CHECK( nc_inq_varid(ncid, specp->lvars[iv], &varid) );
	    varadd(vlist, varid);
	}
    }

    /* output initial line, including format variant */
    pr_fmtvariantx(ncid, path);

    /*
     * get number of dimensions, number of variables, number of global
     * atts, and dimension id of unlimited dimension, if any
     */
    NC_CHECK( nc_inq(ncid, &ndims, &nvars, &ngatts, &xdimid) );
    /* get dimension info */
    for (dimid = 0; dimid < ndims; dimid++) {
	NC_CHECK( nc_inq_dim(ncid, dimid, dims[dimid].name, &dims[dimid].size) );
	if (dimid == xdimid)
  	  Printf("  <dimension name=\"%s\" length=\"%d\" isUnlimited=\"true\" />\n", 
		 dims[dimid].name, (int)dims[dimid].size);
	else
	  Printf ("  <dimension name=\"%s\" length=\"%d\" />\n", 
		  dims[dimid].name, (int)dims[dimid].size);
    }

    /* get global attributes */
    for (ia = 0; ia < ngatts; ia++)
	pr_attx(ncid, NC_GLOBAL, ia); /* print ia-th global attribute */

    /* get variable info, with variable attributes */
    for (varid = 0; varid < nvars; varid++) {
	NC_CHECK( nc_inq_varndims(ncid, varid, &var.ndims) );
	var.dims = (int *) emalloc((var.ndims + 1) * sizeof(int));
	NC_CHECK( nc_inq_var(ncid, varid, var.name, &var.type, 0,
			     var.dims, &var.natts) );
	Printf ("  <variable name=\"%s\"", var.name);
	pr_shape(&var, dims);

	/* handle one-line variable elements that aren't containers
	   for attributes or data values, since they need to be
	   rendered as <variable ... /> instead of <variable ..>
	   ... </variable> */
	if (var.natts == 0) {
	    if (
		/* header-only specified */
		(specp->header_only) ||
		/* list of variables specified and this variable not in list */
		(specp->nlvars > 0 && !varmember(vlist, varid))	||
		/* coordinate vars only and this is not a coordinate variable */
		(specp->coord_vals && !iscoordvar(ncid, varid)) ||
		/* this is a record variable, but no records have been written */
		(isrecvar(ncid,varid) && dims[xdimid].size == 0)
		) {
		Printf (" type=\"%s\" />\n", type_name(var.type));
		continue;
	    }
	}

	/* else nest attributes values, data values in <variable> ... </variable> */
	Printf (" type=\"%s\">\n", type_name(var.type));

	/* get variable attributes */
	for (ia = 0; ia < var.natts; ia++) {
	    pr_attx(ncid, varid, ia); /* print ia-th attribute */
	}
	if (! specp->header_only) {
	    /* if var list specified, test for membership */
	    if (specp->nlvars > 0 && ! varmember(vlist, varid)) {
		continue;
	    }
	    /* output variable data */
	    if (specp->coord_vals) {
		/* don't get data for non-coordinate vars */
		if (!iscoordvar(ncid, varid)) {
		    continue;
		}
	    }
	    /*
	     * Only get data for variable if it is not a record variable,
	     * or if it is a record variable and at least one record has
	     * been written.
	     */
	    if (var.ndims == 0
		|| var.dims[0] != xdimid
		|| dims[xdimid].size != 0) {
		
		/* Collect variable's dim sizes */
		for (id = 0; id < var.ndims; id++)
		    vdims[id] = dims[var.dims[id]].size;
		var.has_fillval = 1; /* by default, but turn off for bytes */

		/* get _FillValue attribute */
		nc_status = nc_inq_att(ncid,varid,_FillValue,&att.type,&att.len);
		if(nc_status == NC_NOERR &&
		   att.type == var.type && att.len == 1) {
		    if(var.type == NC_CHAR) {
			char fillc;
			NC_CHECK( nc_get_att_text(ncid, varid, _FillValue,
						  &fillc ) );
			var.fillval = fillc;
		    } else {
			NC_CHECK( nc_get_att_double(ncid, varid, _FillValue,
						    &var.fillval) );
		    }
		} else {
		    switch (var.type) {
		    case NC_BYTE:
			/* don't do default fill-values for bytes, too risky */
			var.has_fillval = 0;
			break;
		    case NC_CHAR:
			var.fillval = NC_FILL_CHAR;
			break;
		    case NC_SHORT:
			var.fillval = NC_FILL_SHORT;
			break;
		    case NC_INT:
			var.fillval = NC_FILL_INT;
			break;
		    case NC_FLOAT:
			var.fillval = NC_FILL_FLOAT;
			break;
		    case NC_DOUBLE:
			var.fillval = NC_FILL_DOUBLE;
			break;
		    default:
			break;
		    }
		}
		if (vardatax(&var, vdims, ncid, varid, specp) == -1) {
		    error("can't output data for variable %s", var.name);
		    NC_CHECK(nc_close(ncid) );
		    if (vlist)
			free(vlist);
		    return;
		}
	    }
	}
	Printf ("  </variable>\n");
    }
    
    Printf ("</netcdf>\n");
    NC_CHECK(
	nc_close(ncid) );
    if (vlist)
	free(vlist);
}


static void
make_lvars(char *optarg, fspec_t* fspecp)
{
    char *cp = optarg;
    int nvars = 1;
    char ** cpp;

    /* compute number of variable names in comma-delimited list */
    fspecp->nlvars = 1;
    while (*cp++)
      if (*cp == ',')
 	nvars++;

    fspecp->lvars = (char **) emalloc(nvars * sizeof(char*));

    cpp = fspecp->lvars;
    /* copy variable names into list */
    for (cp = strtok(optarg, ",");
	 cp != NULL;
	 cp = strtok((char *) NULL, ",")) {
	
	*cpp = (char *) emalloc(strlen(cp) + 1);
	strcpy(*cpp, cp);
	cpp++;
    }
    fspecp->nlvars = nvars;
}


/*
 * Extract the significant-digits specifiers from the -d argument on the
 * command-line and update the default data formats appropriately.
 */
static void
set_sigdigs(const char *optarg)
{
    char *ptr1 = 0;
    char *ptr2 = 0;
    int flt_digits = FLT_DIGITS; /* default floating-point digits */
    int dbl_digits = DBL_DIGITS; /* default double-precision digits */

    if (optarg != 0 && (int) strlen(optarg) > 0 && optarg[0] != ',')
        flt_digits = (int)strtol(optarg, &ptr1, 10);

    if (flt_digits < 1 || flt_digits > 20) {
	error("unreasonable value for float significant digits: %d",
	      flt_digits);
    }
    if (*ptr1 == ',')
      dbl_digits = (int)strtol(ptr1+1, &ptr2, 10);
    if (ptr2 == ptr1+1 || dbl_digits < 1 || dbl_digits > 20) {
	error("unreasonable value for double significant digits: %d",
	      dbl_digits);
    }
    set_formats(flt_digits, dbl_digits);
}


/*
 * Extract the significant-digits specifiers from the -p argument on the
 * command-line, set flags so we can override C_format attributes (if any),
 * and update the default data formats appropriately.
 */
static void
set_precision(const char *optarg)
{
    char *ptr1 = 0;
    char *ptr2 = 0;
    int flt_digits = FLT_DIGITS;	/* default floating-point digits */
    int dbl_digits = DBL_DIGITS;	/* default double-precision digits */

    if (optarg != 0 && (int) strlen(optarg) > 0 && optarg[0] != ',') {
        flt_digits = (int)strtol(optarg, &ptr1, 10);
	float_precision_specified = 1;
    }

    if (flt_digits < 1 || flt_digits > 20) {
	error("unreasonable value for float significant digits: %d",
	      flt_digits);
    }
    if (*ptr1 == ',') {
	dbl_digits = (int) strtol(ptr1+1, &ptr2, 10);
	double_precision_specified = 1;
    }
    if (ptr2 == ptr1+1 || dbl_digits < 1 || dbl_digits > 20) {
	error("unreasonable value for double significant digits: %d",
	      dbl_digits);
    }
    set_formats(flt_digits, dbl_digits);
}


int
main(int argc, char *argv[])
{
    extern int optind;
    extern int opterr;
    extern char *optarg;
    static fspec_t fspec =	/* defaults, overridden on command line */
      {
	  0,			/* construct netcdf name from file name */
	  false,		/* print header info only, no data? */
	  false,		/* just print coord vars? */
	  false,		/* brief  comments in data section? */
	  false,		/* full annotations in data section?  */
	  LANG_C,		/* language conventions for indices */
	  0,			/* if -v specified, number of variables */
	  0			/* if -v specified, list of variable names */
	  };
    int c;
    int i;
    int max_len = 80;		/* default maximum line length */
    int nameopt = 0;
    boolean xml_out = false;    /* if true, output NcML instead of CDL */


    if(argc==1){
      usage();
#ifdef vms
      exit(EXIT_SUCCESS);
#else
      return EXIT_SUCCESS;
#endif
    }

    opterr = 1;
    progname = argv[0];
    set_formats(FLT_DIGITS, DBL_DIGITS); /* default for float, double data */
    GL_WRITE_MATRIX=0;

    while ((c = getopt(argc, argv, "b:cf:hl:n:v:d:p:x:w")) != EOF)
      switch(c) {
	case 'h':		/* dump header only, no data */
	  fspec.header_only = true;
	  break;
	case 'c':		/* header, data only for coordinate dims */
	  fspec.coord_vals = true;
	  break;
	case 'n':		/*
				 * provide different name than derived from
				 * file name
				 */
	  fspec.name = optarg;
	  nameopt = 1;
	  break;
	case 'b':		/* brief comments in data section */
	  fspec.brief_data_cmnts = true;
	  switch (tolower(optarg[0])) {
	    case 'c':
	      fspec.data_lang = LANG_C;
	      break;
	    case 'f':
	      fspec.data_lang = LANG_F;
	      break;
	    default:
	      error("invalid value for -b option: %s", optarg);
	  }
	  break;
	case 'f':		/* full comments in data section */
	  fspec.full_data_cmnts = true;
	  switch (tolower(optarg[0])) {
	    case 'c':
	      fspec.data_lang = LANG_C;
	      break;
	    case 'f':
	      fspec.data_lang = LANG_F;
	      break;
	    default:
	      error("invalid value for -f option: %s", optarg);
	  }
	  break;
	case 'l':		/* maximum line length */
	  max_len = (int) strtol(optarg, 0, 0);
	  if (max_len < 10) {
	      error("unreasonably small line length specified: %d", max_len);
	  }
	  break;
	case 'v':		/* variable names */
	  /* make list of names of variables specified */
	  make_lvars (optarg, &fspec);
	  break;
	case 'd':		/* specify precision for floats (old option) */
	  set_sigdigs(optarg);
	  break;
	case 'p':		/* specify precision for floats */
	  set_precision(optarg);
	  break;
        case 'x':		/* XML output (NcML) */
	  xml_out = true;
	  break;
	case 'w':
          GL_WRITE_MATRIX=1;
	  break;
	case '?':
	  usage();
	  break;
      }

    set_max_len(max_len);
    
    argc -= optind;
    argv += optind;

    i = 0;

    do {		
        if (!nameopt) 
	    fspec.name = name_path(argv[i]);
	if (argc > 0) {
	    if (xml_out) {
		do_ncdumpx(argv[i], &fspec);
	    } else {
		do_ncdump(argv[i], &fspec);
	    }
	}
    } while (++i < argc);
#ifdef vms
    exit(EXIT_SUCCESS);
#else
    return EXIT_SUCCESS;
#endif
}
