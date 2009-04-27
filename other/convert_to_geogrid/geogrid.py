#!/usr/bin/env python
###############################################################################
#
# Purpose: Write a set of GIS tagged static data files into WPS geogrid format
# Author:  Jonathan Beezley (jon.beezley.math@gmail.com)
# Date:    April 20, 2009
#
# Requires gdal python bindings version 1.6 (lower versions untested)
#
# Origin files must be able to be imported by gdal...  
#   see `gdal-config --formats`
# Output into one or more directories readable by geogrid.
# ( geogrid format limits the number of data points in a single directory to 
#   99999x99999 pixels, larger data sets must be split )
# Finally, print instructions on where to move directories, and how to 
# edit GEOGRID.TBL.
#
# based on gdal standard utilities, gdal_merge.py and gdal_retile.py 
# by Frank Warmerdam.
#
###############################################################################

'''
Created on Apr 21, 2009

@author: jbeezley
'''    
import sys
import warnings
import os
import shutil
import textwrap
from optparse import OptionParser, OptionGroup
try:
    import numpy as np
except ImportError:
    print "Cannot import numeric python modules (numpy/scipy)"
    print "Are they installed?  Is the install directory in your"
    print "PYTHONPATH?"
    sys.exit(3)
from print_control import verbprint, setverbose
from source_data import Source as S
from source_data import source_option_parser


class Geogrid:
    """geogrid export class"""

    class Info:
        """geogrid info class"""

        def __init__(self,source,dirname,opts):
            self.sourcedata=S(source)
            self.dirname=dirname
            self.desc=opts.desc
            self.wordsize=opts.wordsize
            self.halo=opts.halo
            self.missing=opts.missing
            self.endian=opts.endian
            self.nxtile=opts.nxtile
            self.nytile=opts.nytile
            self.nztile=opts.nztile
            self.scale=opts.scale
            self.force=opts.force
            self.units=opts.units
            self.projection="regular_ll"
            self.continuous="True"
            self.signed=opts.signed
            self.script=opts.script
            if self.script:
                setverbose(False)
            self.check()

        def check(self):
            pass
        
        def write(self,d):
            verbprint("creating geogrid header file: "+os.path.join(d,"info"))
            self.check()
            n=self.sourcedata.getsize()
            if n[0] > 99999 or n[1] > 99999:
                raise Exception("Too many input data points to fit into "+\
                                "one geogrid data structure, and "+\
                                "splitting them is not yet implemented")
            f=open(os.path.join(d,"info"),"w")
            f.write("projection="+self.projection+"\n")
            if self.continuous:
                s="continuous"
            else:
                s="categorical"
            f.write("type="+s+"\n")
            if self.units is None:
                if not self.script:
                    print textwrap.fill("WARNING: You didn't specify any units, and I can't find "+ \
                                        "it in the data set.  Substituting \"meters\"",80)
                s="meters"
            else:
                s=self.units
            f.write('units="'+s+'"\n')
            if self.desc is None:
                self.desc=self.sourcedata.getdescription()
            f.write('description="'+self.desc+'"\n')
            r=self.sourcedata.getresolution()
            f.write("dx="+str(r[0])+"\n")
            f.write("dy="+str(r[1])+"\n")
            if self.sourcedata.gettopbottom():
                n=n[1]
                s="top_bottom"
            else:
                n=1
                s="bottom_top"
                
            r=self.sourcedata.getfirstcoord()
            f.write("known_x=1\n")
            f.write("known_y=%i\n" % n)
            f.write("known_lon=%f\n" % r[0])
            f.write("known_lat=%f\n" % r[1])
            f.write("wordsize=%i\n" % int(self.wordsize))
            f.write("tile_bdr=%i\n" % self.halo)
            f.write("missing_value=%i\n" % self.missing)
            f.write("scale_factor=%f\n" % self.scale)
            f.write("row_order=%s\n" % s)
            f.write("endian=%s\n" % self.endian)
            f.write("tile_x=%i\n" % self.nxtile)
            f.write("tile_y=%i\n" % self.nytile)
            f.write("tile_z=%i\n" % self.nztile)
            if self.signed:
                s="yes"
            else:
                s="no"
            f.write("signed=%s\n" % s)
            f.close()
            

    def __init__(self,source,dirname,opts):
        self.info=Geogrid.Info(source,dirname,opts)
        
    def tilename(self,xstart,xend,ystart,yend):
        return "%05i-%05i.%05i-%05i" % (xstart,xend,ystart,yend)
        
    def writetile(self,fname,xstart,xend,ystart,yend):
        #fname=os.path.join(self.info.dirname,self.tilename(xstart,xend,ystart,yend))
        verbprint("creating tile file:"+fname) 
        f=open(fname,'wb')
        a=self.info.sourcedata.gettile(xstart, xend, ystart, yend,
                                       float(self.info.missing)/
                                       float(self.info.scale))
        u="u"
        bb=int(self.info.wordsize)*8
        if self.info.signed:
            u=""
        type="%sint%i" % (u,bb)
        verbprint("scaling data by: "+str(self.info.scale))
        a=a/self.info.scale
        verbprint("converting data to: "+type)
        b=a.astype(type)
        if sys.byteorder not in ["big","little"]:
            raise Exception('strange, sys.byteorder is not "big" or "little"')
        if sys.byteorder != self.info.endian:
            verbprint("swapping byte order to: "+self.info.endian)
            b.byteswap(True)
        verbprint("writing to file:"+fname)
        b.tofile(f)
        f.close()
        
    def createoutput(self):
        verbprint("checking if data directory %s exists" % self.info.dirname)
        if os.path.lexists(self.info.dirname):
            if self.info.force:
                if os.path.isdir(self.info.dirname):
                    verbprint("deleting directory: "+self.info.dirname)
                    shutil.rmtree(self.info.dirname)
                else:
                    verprint("deleting file: "+self.info.dirname)
                    os.remove(self.info.dirname)
            else:
                raise Exception("A file or directory exists at "+
                                self.info.dirname+".  Use -f to overwrite.")
        verbprint("creating output directory: "+self.info.dirname)
        os.mkdir(self.info.dirname)
        self.info.write(self.info.dirname)
        
    def write(self):
        self.createoutput()
        m=self.info.sourcedata.getsize()
        if m[0] <= 2*self.info.halo or m[1] <= 2*self.info.halo:
            print "Either your source data set is too small or your border is too large!!!"
            raise Exception
        h=self.info.halo
        n=(m[0]-2*h,m[1]-2*h)
        ntx=(n[0]-1)/self.info.nxtile + 1
        nty=(n[1]-1)/self.info.nytile + 1
        for x in range(ntx):
            for y in range(nty):
                xstart=x*self.info.nxtile + 1
                ystart=y*self.info.nytile + 1
                xend=min((x+1)*self.info.nxtile + 2*h,n[0])
                yend=min((y+1)*self.info.nytile + 2*h,n[1])
                tname=self.tilename(xstart+h,xend-h,
                                    ystart+h,yend-h)
                fname=os.path.join(self.info.dirname,tname)
                self.writetile(fname,xstart, xend, ystart, yend)
        
        
def mainprog(argv):
    usage = "usage: %prog [options] output input1 [input2 [input3 [...]]]"
    parser=OptionParser(usage)
    out_parse=OptionGroup(parser,"output options")
    source_option_parser(parser)
    out_parse.add_option("-w","--wordsize",choices=["1","2","4"],
                         help="size (in bytes) of each number in the output file "
                              "[default: %default]")
    out_parse.add_option("-b","--border",type="int",dest="halo",metavar="BORDER",
                         help="size of border for each data tile "
                              "[default: %default]")
    out_parse.add_option("-e","--endian",choices=["big","little"],
                         help="byte order of output (conversion handled automatically "
                              "by geogrid, so it is not necessary to change) "
                          "[default= %default]")
    out_parse.add_option("-m","--missing",type="int",
                         help="value (int) to set any missing data in the source data "
                              "[default: %default]")
    out_parse.add_option("-s","--scale",type="float",
                         help="geogrid stores data as integers scaled by some constant value "
                              "[default: %default]")
    out_parse.add_option("-x","--xsize",type="int",dest="nxtile",metavar="XSIZE",
                         help="output tile sizes in x (west-east) direction "
                              "[default: %default]")
    out_parse.add_option("-y","--ysize",type="int",dest="nytile",metavar="YSIZE",
                         help="output tile sizes in y (south-north) direction "
                              "[default: %default]")
    out_parse.add_option("-z","--zsize",type="int",dest="nztile",metavar="ZSIZE",
                         help="output data size (not tiled) "
                              "in vertical direction or the number "
                              "of categories for categorical data "
                              "[default: determined from source data]")
    out_parse.add_option("-d","--description",dest="desc",
                         help="description of the data set"
                               "[default: determined from source data]")
    out_parse.add_option("-u","--units",
                         help="physical units of the data"
                              "[default: determined from source data]")
    out_parse.add_option("-f","--force",action="store_true",
                         help="force creation of output files even if "
                              "they already exist [default: %default]")
    out_parse.add_option("-S","--signed",action="store_true",
                         help="output data is signed [default: %default]")
    parser.add_option("-v","--verbose",action="store_true",
                      help="set verbose output [default: %default]",)
    parser.add_option("--script",action="store_true",
                      help="set non-verbose output for use inside a script")
    parser.add_option_group(out_parse)
    parser.set_defaults(wordsize="2",halo=3,endian="little",nxtile=1000,nytile=1000,
                       nztile=1,scale=1.0,missing=65535,verbose=False,
                       force=False,signed=False,script=False)
    (opts,args)=parser.parse_args(argv)
    setverbose(opts.verbose)
    if len(args) < 2:
        parser.print_usage()
        print "Must supply output directory and one or more input files."
        sys.exit(2)
        
    g=Geogrid(args[1:],args[0],opts)
    g.write()
    if not opts.script:
        print "Completed successfully!"
    if len(args[1:])>1:
        s=args[1:]
    else:
        s=args[1]
    inst="To use this data in geogrid move %s to your wrf geographical \
data directory (i.e. '../../wrfdata/geog/') and insert the following lines \
into the relevant section of your GEOGRID.TBL file (geogrid/GEOGRID.TBL by default).  \
This is a run time file, you do not need to recompile WPS!"
    if not opts.script:
        print textwrap.fill(inst,80)
    #    print "name=<variable name>"
    #    print "\tpriority=1"
        if g.info.continuous:
            s="continuous"
        else:
            s="categorical"
        print "\tdest_type=%s" % s
        fmissing=float(g.info.missing) * g.info.scale
        print "\tfill_missing=%f" % fmissing
        print "\trel_path=default:%s" % g.info.dirname
        
        web="http://www.mmm.ucar.edu/wrf/users/docs/user_guide/users_guide_chap3.html#_Description_of_GEOGRID.TBL"
        inst="These are the only options I can determine from the data set, \
see the GEOGRID.TBL specification:"
        print textwrap.fill(inst,80)
        print web
        print "for a full listing of options."
    else:
        #print g.info.dirname
        return g.info.dirname
   
if __name__ == '__main__':
#    mainprog(["-f","-v","testdir","24/w001001.adf"])
    mainprog(sys.argv[1:])
#    mainprog(["-h"])
    
