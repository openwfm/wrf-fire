#!/usr/bin/env python

#
# A python library for communicating with seamless data servers such as
# USGS and LANDFIRE.
#

#http://extract.cr.usgs.gov/Website/distreq/RequestOptions.jsp?PR=0&CU=Native&ZX=-1.0&ZY=-1.0&ML=COM&MD=DL
#              &AL=39.74127369858583,38.49736905526963,-106.24611383354475,-107.90622991644965&CS=250&PL=ND302XT
#http://landfire.cr.usgs.gov/Website/distreq/RequestSummary.jsp?AL=39.89679054054048,39.300084459459406,-105.99712837837805,-106.63361486486454&CS=250&UTMDATUM=0&PL=F0J02XT

import mechanize
from BeautifulSoup import BeautifulStoneSoup as xmlparser
import cookielib
import tarfile
import shutil
import os

# Browser
br = mechanize.Browser()

# Cookie Jar
cj = cookielib.LWPCookieJar()
br.set_cookiejar(cj)

# Browser options
br.set_handle_equiv(True)
br.set_handle_gzip(True)
br.set_handle_redirect(True)
br.set_handle_referer(True)
br.set_handle_robots(False)

# Follows refresh 0 but not hangs on refresh > 300
br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=300)

# Want debugging messages?
br.set_debug_http(True)
br.set_debug_redirects(True)
br.set_debug_responses(True)

class LatLonBounds(object):
    def __init__(self,latmax,latmin,lonmax,lonmin):
        self.latmax=latmax
        self.latmin=latmin
        self.lonmax=lonmax
        self.lonmin=lonmin
    
    def URLfmt(self):
        return "%f,%f,%f,%f" % (self.latmax,self.latmin,self.lonmax,self.lonmin)

class ProductCode(object):
    meta_d={'XML':'X','HTML':'H','TXT':'T'}
    comp_d={'tgz':'T','zip':'Z'}
    form_d={'ArcGRID':'01','GeoTIFF':'02'}
    def __init__(self,base,meta='XML',comp='tgz',form='GeoTIFF'):
        self.base=base
        self.meta=meta
        self.comp=comp
        self.form=form
    
    def __str__(self):
        return "%s%s%s%s" % (self.base,self.form_d[self.form],self.meta_d[self.meta],self.comp_d[self.comp])

class SeamlessServer(object):
    projs={ \
           'native':'PR=0&CU=Native',\
           'lfnative':'UTMDATUM=0'
    }
    misc='ZX=-1.0&ZY=-1.0&ML=COM&MD=DL'
    boundskwd='AL'
    downloadsizekwd='CS'
    productkwd='PL'
    def __init__(self,baseURL):
        self.baseURL=baseURL
    
    def getURL(self,product,bounds,proj='native',dlsize=250):
        return self.baseURL+\
               "&".join([self.projs[proj], \
                         self.misc,"%s=%s" %(self.boundskwd,bounds.URLfmt()),\
                         "%s=%i" %(self.downloadsizekwd,dlsize),\
                         "%s=%s" %(self.productkwd,product)])
    
    def getDataURL(self,URL):
        br.open(URL)
        br.select_form(nr=0)
        br.submit()
        r=br.response()
        dURL=r.geturl()
        return dURL
    
    def getData(self,product,bounds,targetdir='data',proj='native',dlsize=250):
        URL=self.getURL(product,bounds,proj,dlsize)
        dURL=self.getDataURL(URL)
        v=br.retrieve(dURL)
        f=tarfile.open(v[0],'r')
        try:
            os.mkdir(targetdir)
        except OSError:
            pass
        if not os.path.isdir(targetdir):
            raise OSError("Could not create target directory in %s."%targetdir)
        f.extractall(targetdir)
        filenames=f.getnames()
        description=None
        datafile=None
        for g in filenames:
            f=os.path.join(targetdir,g)
            print f
            if f[-3:] == "xml":
                xmldata=open(f,'r').read()
                soup=xmlparser(xmldata)
                title=soup.find('title')
                if title is not None:
                    description=title.string
            elif f[-3:] == "tif":
                datafile=f
        name=product
        if datafile is None:
            raise Exception("Could not find a geotiff file in the archive.")
        return (datafile,name,description)


testbounds=LatLonBounds(40.07160929510595,40.011401113951685,-105.9405756939366,-106.0093850438272)
nedServer=SeamlessServer('http://extract.cr.usgs.gov/Website/distreq/RequestSummary.jsp?')
nedProduct=ProductCode('ND3')
lfServer=SeamlessServer('http://landfire.cr.usgs.gov/Website/distreq/RequestSummary.jsp?')
lfServer.misc=''
nfuelProduct=ProductCode('F0J')

def test():
    print "Testing USGS server with NED data."
    files=nedServer.getData(nedProduct,testbounds,targetdir='neddata')
    print files
    print ''
    print '*'*80
    print ''
    print "Testing LANDFIRE server with fuel category data"
    files=lfServer.getData(nfuelProduct,testbounds,targetdir='nfueldata',proj='lfnative')
    print files
