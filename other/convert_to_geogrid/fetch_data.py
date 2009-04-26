#!/usr/bin/env python
##############################################################################
#  fetch_data.py
#
#  Gets a data set from usgs seamless server
#      (http://seamless.usgs.gov/)
#  By default it retrieves 1/3 " NED (elevation) for a high resultion fire
#  grid, but it is possible to use this script to get other data sets from
#  the same website.  The domain area is defined by a region with boundaries
#  parallel to lines of latitude/longitude given as arguments.  The script
#  only accepts decimal values (it doesn't understand 36N 15' 34.6").
#  There are a number of limitations of the seamless server, which 
#  may cause this script to error out with some obscure message.  
#  For example data sets are limited to ~2 GB chunks at once.  
#  Rerunning with the '-v' flag may give clues as to what went wrong.
#  It shouldn't be too hard to extend this script to work for other servers
#  based on the usgs seamless map (like landfire), maybe in time...
#
#  Author: Jonathan Beezley (jon.beezley.math@gmail.com)
#  Date: April 20, 2009
#
#  Requires:
#    twill python modules (http://twill.idyll.org/)
#
##############################################################################

import sys
import re
import time
from optparse import OptionParser
import urllib

try:
    import twill
except ImportError:
    print "Cannot find twill modules, are they installed and in"+\
          "PYTHONPATH.  You can find them at "+\
          "http://twill.idyll.org/ or `easy_install twill`"
    sys.exit(3)

# a simple class with a write method
class WritableObject:
    def __init__(self):
        self.content = []
    def write(self, string):
        self.content.append(string)
    def clear(self):
        self.content=[]

def code(g):
    if g.get_code() != 200:
        return False
    else:
        return True

if __name__ != "__main__":
    print "This code is not meant to be a module!!!"

usage = "usage: %prog [options] -- north south east west\n"
usage += "coordinates given in lat/lon decimal degrees\n"
usage += "('--' is required!)"
parse=OptionParser(usage)
parse.add_option("-v","--verbose",action="store_true",
                 help="set verbose output for debugging")
parse.add_option("-d","--dataset",action="store",
                 help="set usgs data set name [default: ND301HZ (1/3 arcsec elevation)]")
parse.set_defaults(verbose=False,dataset="ND301HZ")
(opts,args)=parse.parse_args(sys.argv[1:])
verbose=opts.verbose
if len(args) != 4:
    #args=["39.9","39.","-106","-107"]
    parse.print_usage()
    print "Must supply coordinate information."
    sys.exit(2)
args=[ float(a) for a in args ]
args.append(opt.dataset)
u="http://extract.cr.usgs.gov/Website/distreq/RequestSummary.jsp?AL=%9.6f,%9.6f,%9.6f,%9.6f&PL=%s," % \
   tuple(args)
print "Generating data from:"
print u
print "This could take a few minutes"
foo = WritableObject()
sys.stdout = foo
g=twill.browser.TwillBrowser() 
g.go(u)
if not code(g):
    sys.stdout = sys.__stdout__
    raise Exception("Couldn't open webpage")
a=g.get_all_forms()
v=[]
r=re.compile('http://\S*')
for f in a:
    doclick=False
    for i in f.controls:
        # possibly collect other data here
        if i.attrs['type'] == 'submit' and i.attrs['value'] == 'Download':
            doclick=True
    if doclick: 
        g._browser.form=f
        foo.clear()
        
        try:
            g.submit()
        except:
            pass
        
        if verbose:
            sys.stdout = sys.__stdout__
            print ""
            print "Generated this output:"
            print ""
            print "".join(foo.content)
            print ""
            sys.stdout = foo

            
        if g.get_url() is not None:
            sys.stdout = sys.__stdout__
            print ""
            print "WARNING"
            print "The query redirected correctly... this is not expected."
            print "Something might be going wrong."
            if not verbose:
                print "If the files didn't download correctly, rerun with '-v'"
                print "Maybe the website has changed."
            if verbose:
                print "Check the output below for error messages."
                print "The page returned has a title: ",g.get_title()
                print "and here is the full html:"
                print g.get_html()
            print ""
            sys.stdout = foo
        else:
            #reopen the url because g.submit closes it!!!
            g.go(u)
            
        for vv in foo.content:
            m=r.search(vv)
            if m:
                v.append(m.group())
                break
    

sys.stdout = sys.__stdout__

print "Received the following URL's:"
print "\n".join(v)

print ""
print "Waiting for 1 minute for the source files to be generated."
time.sleep(60)
print "Downloading"
i=0
files=[]
maxretries=25
pause=60
for u in v:
    i+=1
    print u," ..."
    sname="datafile%02i.zip" % i
    for i in range(maxretries):
        (fname,finfo)=urllib.urlretrieve(u,sname)
        if finfo.dict['content-type'] != 'text/html':
            break
        if verbose:
            print "Received: ", g.get_title()
            print ""
            print g.get_html()
            print ""
        print "Not finished yet, wait some more..."
    else:
        print "The data should be available by now..."
        print "Something has gone wrong."
        print "Try downloading the files manually."
        raise Exception("Couldn't download data files!")
    if verbose:
        print ""
        print "File meta information:"
        print finfo.dict
        print ""
    if finfo.dict['content-type'] != "application/x-zip-compressed":
        print "Strange, this doesn't seem to be a zip file."
        print "Saving with .zip extension anyway."
    print "saved to: "+fname
    print ""
    files.append(fname)
    
    
    
