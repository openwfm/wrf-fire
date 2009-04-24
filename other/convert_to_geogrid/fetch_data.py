'''
Created on Apr 23, 2009

@author: jbeezley
'''

import sys
import re
import time
from optparse import OptionParser
import urllib

#from twill.commands import *
import twill

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

usage = "usage: %prog [options] -- north south east west\n"
usage += "coordinates given in lat/lon decimal degrees\n"
usage += "('--' is required!)"
parse=OptionParser(usage)
parse.add_option("-v","--verbose",action="store_true",
                 help="set verbose output for debugging")
parse.set_defaults(verbose=False)
(opts,args)=parse.parse_args(sys.argv[1:])
verbose=opts.verbose
if len(args) != 4:
    #args=["39.9","39.","-106","-107"]
    parse.print_usage()
    print "Must supply coordinate information."
    sys.exit(2)
args=[ float(a) for a in args ]
u="http://extract.cr.usgs.gov/Website/distreq/RequestSummary.jsp?AL=%9.6f,%9.6f,%9.6f,%9.6f&PL=ND301HZ," % \
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
    
    
    