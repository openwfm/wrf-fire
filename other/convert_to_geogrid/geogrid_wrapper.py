#!/usr/bin/env python
##############################################################################
#
#  geogrid_wrapper.py
#
#  A python wrapper for geogrid.exe.  Grabs any missing source data that it 
#  knows how to get.  Just an automated method for calling fetch_data.py 
#  and geogrid.py.  It determines the data set needed, gets it, converts it
#  to geogrid format, and moves it to the correct place. 
#
#  Author: Jonathan Beezley (jon.beezley.math@gmail.com)
#  Date:   April 26, 2009
#
##############################################################################
'''A wrapper for WPS's geogrid.exe that automatically obtains data.

Local modules:
    fetch_data : gets data from webserver
    geogrid : converts a dataset into geogrid format
    
Nonstandard modules
    namelist : imports fortran namelists (included)
    
Standard modules
    subprocess, os, shutil, sys, time, re, tarfile, optparse
    
Python 2.5 compatible.  This module is designed to be run as a script, and 
currently supports only two fields, ZSF and NFUEL_CAT.  Adding more fields
requires modifying the global variables destfields and outputfields, plus
a number of other dependencies in submodules (such as defining the data
source location in fetch_data.py, etc.)  

Assumes that geogrid.exe has been built and is in the current directory.  
namelist.wps, which should also be present, is imported to determine where
GEOGRID.TBL is.  GEOGRID.TBL should also be present and valid.  This module
will only correct errors relating to missing data that it knows how to get, 
anything else and it will give up.  As such, the data directories specified
in GEOGRID.TBL must exist and contain a valid index file, but they don't 
necessarily need any actual data files.

This script will run geogrid a number of times and collect data that is needed
from online sources.  Once geogrid completes successfully, execution stops
with exit status 0.  If for any reason something goes wrong, an error 
message is printed and the script exits with >0 status.

Note:  It will not overwrite data files from previous runs.  Either move them 
       out of the way or use the "-f" flag.'''

# standard modules:
import subprocess as sp
import os
import shutil
import sys
import time
import re
import tarfile
from optparse import OptionParser

# included modules
import namelist
import fetch_data
import geogrid

# data field definitions
destfields=['ZSF','NFUEL_CAT']

outputfields=[{},{}]
outputfields[0]['name']=destfields[0]
outputfields[0]['dir']="highres_elev"
outputfields[0]['desc']="National Elevation Dataset (NED)"
outputfields[0]['units']="meters"
outputfields[0]['maxcat']=None
outputfields[0]['source']='NED'
outputfields[0]['found']=False

outputfields[1]['name']=destfields[1]
outputfields[1]['dir']='landfire'
outputfields[1]['desc']='LANDFIRE 13 Anderson Fire Behavior Fuel Models'
outputfields[1]['units']='category'
outputfields[1]['maxcat']=14
outputfields[1]['source']='LANDFIRE13'
outputfields[1]['found']=False

# define a number of static variables used for controlling loops
donotfetch=False
runnum=0
lastmissingvar=''
criticalfail=False
makeforce=False

def main_rep(argv):
    '''Main executable: Take command line arguments and loop over the wrapper.'''
    global runnum
    global makeforce
    numrepeats=len(destfields)
    for i in range(numrepeats):
        runnum+=1
        main(argv)
        if criticalfail:
            break
        print "Running script again with new data."
    else:
        print "All data should be available now, last try."
        donotfetch=True
        main(argv)
        print "Geogrid failed after %i tries... giving up." % runnum

    sys.exit(1)
        

def main(argv):
    '''Wrapper script:  Call geogrid and if data is missing, fetch it, and repeat.'''
    global lastmissingvar
    global criticalfail
    
    # set path names
    geogridexe="geogrid.exe"
    geoem="geo_em.d01.nc"
    defaultnml="namelist.fire.default"
    runnml="namelist.wps"
    cwd="."
    geocmd=os.path.join(cwd,geogridexe)
    expandbdy=.5
    
    # parse commandline options
    parse=OptionParser("usage: %prog [options]")
    parse.add_option("-f","--force",action="store_true",
                     help="delete destination data directory if it exists")
    parse.set_defaults(force=False)
    (opts,args)=parse.parse_args(argv)
    if len(args) > 0:
        parse.print_usage()
    
    # find namelist copy to runtime namelist
    if not os.path.isfile(runnml):
        if not os.path.isfile(defaultnml):
            print "Can't find a namelist file in %s or %s" % (runnml,defaultnml)
            sys.exit(2)
        shutil.copy(defaultnml,runnml)
        
    # read in the namelist
    print "reading namelist %s" % runnml
    nml=namelist.Namelist(runnml)
    
    # get relevant options, geog data path MUST be there,
    # others we can infer defaults
    try:
        geog_data_path=nml['geogrid']['par'][0]['geog_data_path'][0]
    except:
        print "Namelist, %s, doesn't seem to be valid." % runnml
        sys.exit(2)
        
    try:
        max_dom=int(nml['share']['par'][0]['max_dom'][0])
    except:
        max_dom=1
        
    try:
        ioform=int(nml['share']['par'][0]['io_form_geogrid'][0])
    except:
        ioform=2
        
    try:
        tblpath=nml['geogrid']['par'][0]['opt_geogrid_tbl_path'][0]
    except:
        tblpath=os.path.join('.','geogrid','')
        
    outbase="geo_em.d%02i."
    tblfile=tblpath+"GEOGRID.TBL"
    if ioform == 1:
        outpat=outbase+"int"
    elif ioform == 2:
        outpat=outbase+"nc"
    elif ioform == 3:
        outpat=outbase+"gr1"
    else:
        print "Bad 'io_form_geogrid' value in %s" % runnml
    
    
    # make sure geogrid is built
    if not os.access(geocmd,os.F_OK | os.R_OK | os.X_OK):
        print "geogrid.exe doesn't seem to exist or is not executable"
        print "have you run configure/compile?"
        sys.exit(2)
   
    # look at GEOGRID.TBL and make sure that a shell directory exists
    # for each data source that can be fetched automatically
    # make it more foolproof by checking that the file exists and 
    # copying from the default if it does not.
    if not os.access(tblfile, os.F_OK):
        # GEOGRID.TBL doesn't exist, check for the default
        dtblfile=os.path.join('.','geogrid','GEOGRID.TBL.FIRE.NED')
        if not os.access(dtblfile,os.R_OK):
         print "Cannot find or access %s" % tblfile
        sys.exit(2)
        shutil.copy(dtblfile,tblfile)
        
    proc_tbl(tblfile)


    # run geogrid and pipe output in a buffer
    print "running geogrid.exe"
    now=time.time()
    p=sp.Popen(geocmd,stdout=sp.PIPE,stderr=sp.STDOUT,bufsize=-1)
    
    # get output
    (sto,ste)=p.communicate()
    
    # now do a number of things to check if it was successful
    # check error code (requires a patched module_parallel)
    # check existence of and modification date of geo_em.d??.*
    # stderr is empty, check stdout for error strings
    errpat=re.compile("\s*ERROR:")
    if p.returncode == 0 and \
       all([ os.path.isfile(outpat % i) for i in range(1,max_dom+1)]) and \
       all([ os.path.getmtime(outpat % i) > now for i in range(1,max_dom+1)]) and \
       errpat.search(sto) is None:
        print "Geogrid completed successfully."
        sys.exit(0)
    else:
        pass
        #print "returncode=",p.returncode
        #print "isfile=",os.path.isfile(outpat % 1)
        #print "mtime=",os.path.getmtime(outpat % 1),now
        #print "errorstring: ",errpat.search(sto)
    
    # if we got here something went wrong in geogrid, see if it is missing data:
    r=re.compile("Missing value encountered in output field (?P<field>\w*)\n")
    field=r.search(sto)
    
    if field is None:
        print sto
        print "An error occurred while running geogrid, but it doesn't seem to be caused "+\
              "by missing data."
        sys.exit(1)
    
    field=field.group('field').strip()
    if not field.strip() in destfields:  # + others once fetch_data.py is generalized
        print "Data is missing in field, %s, but I don't know how to fetch it." % field.strip()
        sys.exit(1)
        
    if field == lastmissingvar:
        print "I already tried to fetch %s, but it is still missing!!" % field
        sys.exit(1)
    
    lastmissingvar=field
    
    if donotfetch:
        return

    destfield=field
    outputi=destfields.index(field)
    output=outputfields[outputi]

    # Now we know that we need to get NED data from usgs, but we need the domain bounds.
    # regexp the boundaries to get the whole domain.  
    # the findall syntax is for running in parallel, but it probably won't actually work
    # without patching geogrid source because only process 0 prints.
    fnumber='-?[0-9]+[.]?[0-9]*'
    r=re.compile("LATSOUTH=(?P<south>%s)" % fnumber)
    south=r.findall(sto)
    r=re.compile("LATNORTH=(?P<north>%s)" % fnumber)
    north=r.findall(sto)
    r=re.compile("LONWEST=(?P<west>%s)" % fnumber)
    west=r.findall(sto)
    r=re.compile("LONEAST=(?P<east>%s)" % fnumber)
    east=r.findall(sto)

    if south == [] or north == [] or west == [] or east == []:
        print "Can't parse domain boundaries."
        sys.exit(1)
    
    # get domain boundaries and call data fetcher
    south=min(float(x) for x in south)
    north=max(float(x) for x in north)
    west=min(float(x) for x in west)
    east=max(float(x) for x in east)
    
    epsy=(north-south)*expandbdy
    epsx=(east-west)*expandbdy
    south -= epsy
    north += epsy
    west -= epsx
    east += epsx
    
    
    print "Executing: fetch_data.py -d %s -- " % output['source']\
           ,north,south,east,west
    try:
        files=fetch_data.main(["-d",output['source'],"--",str(north),str(south),str(east),str(west)])
    except:
        print "fetch_data.py seems to have failed."
        print "For more information, try running:"
        print "fetch_data.py -v  -d %s -- " % output['source'],north,south,east,west
        raise
        
    # exctract files that were downloaded (assumes tar files with bzip2 or gzip compression)
    print "Extracting data files."
    datafiles=[]
    for f in files:
        ft=tarfile.open(f)
        fn=ft.getnames()
        for i in fn:
            if i[-3:] == "tif":
                datafiles.append(i)
                break
        else:
            print "%s doesn't seem to contain a geotiff file" % f
            sys.exit(1)
    
        # extract and clean up tar files
        ft.extractall()
        os.remove(ft.name)
    
    # run source data to geogrid conversion
    if opts.force or makeforce:
        argv=['-f']
    else:
        argv=[]
    if output['maxcat'] is not None:
        argv.extend(['-m',str(output['maxcat']),'-a','-A',str(output['maxcat']),'-w','1'])
    argv.extend(['-d',output['desc'],'-u',output['units'],'--script','--',output['dir']])
    argv.extend(datafiles)
    print "Running geogrid.py %s" % " ".join(argv)
    try:
        gdir=geogrid.mainprog(argv)
    except:
        print "geogrid.py failed"
        raise
        
def proc_tbl(tblfile):
    # process GEOGRID.TBL
    print "Setting up %s" % tblfile
    tmp=os.tmpfile()
    tbl=open(tblfile)
    line=tbl.readline()
    r=re.compile("^\s*name\s*=\s*(?P<val>\w*)\s*$")
    
    # process all lines in geogrid table:
    #   copy lines to temporary file unless
    #   it is inside of the relevant field description
    #   
    #   if it is inside the relevant field then replace data paths
    #   with what was given back from geogrid.py 
    while line != '':
        val=r.match(line)
        if val != None:
            for i in range(len(destfields)): 
                destfield=destfields[i]
                output=outputfields[i]
                absdir=os.path.realpath(os.path.join('.',output['dir']))
                if val.group('val').strip() == destfield:
                    outputfields[i]['found']=True
                    while line.find("="*6) == -1 and line != '': 
                        if line.find("rel_path=default:") != -1 or line.find("abs_path=default:") != -1:
                            tmp.write('\tabs_path=default:%s\n' % absdir)
                            mkblank_dir(absdir)
                        elif line.find("_path") != -1:
                            pass
                        else:
                            tmp.write(line)
                        line=tbl.readline()
        tmp.write(line)
        line=tbl.readline()
    
    for output in outputfields:
        if not output['found']:
            print "Couldn't find a section in %s matching %s" % (tblfile,destfield)
            print "Inserting reasonable defaults."
            tmp.write("name=%s\n" % destfield)
            tmp.write("\tpriority=1\n")
            if output['maxcat'] is not None:
                tmp.write("\tdest_type=continuous\n")
                tmp.write("\tinterp_option=default:four_pt+average_16pt+search\n")
            else:
                tmp.write("\tdest_type=categorical\n")
                tmp.write("\tdominant_only=%s\n" % output['name'])
                tmp.write("\tz_dim_name=%s\n" % output['units'])
                tmp.write("\tinterp_option=default:nearest_neighbor+average_16pt+search\n")
            
            tmp.write("\thalt_on_missing=yes\n")
            tmp.write("\tabs_path=default:%s\n"% absdir)
            tmp.write("\tsubgrid=yes\n")
            tmp.write("="*30)

    #copy temporary file to geogrid table
    tmp.flush()
    tbl.close()
    os.rename(tblfile,tblfile+"_backup%02i"%runnum)
    tmp.seek(0)
    tbl=open(tblfile,"w")
    tbl.write(tmp.read())
    tbl.close()
   
def mkblank_dir(d):
    global makeforce
    if not os.path.isdir(d):
        if os.path.lexists(d):
            print "A file or link exists at %s, you must move it before I can continue." % d
            sys.exit(1)
        os.mkdir(d)
        # make a dummy index file in this directory, just so geogrid
        # doesn't bomb out
        print "creating shell data directory in %s" % d
        makeforce=True
        h=open(os.path.join(d,'index'),'w')
        s='''projection=regular_ll
type=continuous
units="none"
description="Empty"
dx=9.25925926e-05
dy=9.25925926e-05
known_x=1
known_y=575
known_lon=-107.324537
known_lat=39.732500
wordsize=2
tile_bdr=0
missing_value=65535
scale_factor=1.000000
row_order=top_bottom
endian=little
tile_x=746
tile_y=575
tile_z=1
signed=no'''
        h.write(s)
        h.close()
            
        
        
    
if __name__=="__main__":
    main_rep(sys.argv[1:])


