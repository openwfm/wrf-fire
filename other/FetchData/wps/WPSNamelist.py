#!/usr/bin/env python

import sys
import os
import re
from namelist import Namelist
from openanything import openAnything

try:
    from WPSProj import get_projection_coords

    def projBounds(nml,idomain):
        (nx,ny)=nml.getDomainSize(idomain)
        lly,llx=get_projection_coords(idomain+1,1,1)
        uly,ulx=get_projection_coords(idomain+1,1,ny)
        lry,lrx=get_projection_coords(idomain+1,nx,1)
        ury,urx=get_projection_coords(idomain+1,nx,ny)
        llat=min(lly,uly,lry,ury)
        ulat=max(lly,uly,lry,ury)
        llon=min(llx,ulx,lrx,urx)
        ulon=max(llx,ulx,lrx,urx)
        return (ulat,llat,ulon,llon)

except ImportError:
    
    print "Could not import geogrid wrapper."
    print "You will need to specify the latitude and longitude bounds for your domain."
    
    def inputValue(s):
        v=True
        while v:
            print s
            r=sys.stdin.readline()
            try:
                o=float(r)
                break
            except:
                print "Invalid response.  Please enter a number."
        return o
    
    def projBounds(nml,idomain):
        s="What is the upper bound of latitude for domain %i?"%idomain
        ulat=inputValue(s)
        s="What is the lower bound of latitude for domain %i?"%idomain
        llat=inputValue(s)
        s="What is the upper bound of longitude for domain %i?"%idomain
        ulon=inputValue(s)
        s="What is the lower bound of longitude for domain %i?"%idomain
        llon=inputValue(s)
        return (ulat,llat,ulon,llon)

class GeogridTBL(dict):
    def __init__(self,filename=None):
        if filename is None:
            self.fid=openAnything('')
        else:
            self.fid=openAnything(filename)
        
        self.parse_file()
    
    @staticmethod
    def cleanlines(lines):
        r=re.compile(r'#.*')
        for l in lines:
            yield r.sub('',l).strip()
    
    @staticmethod
    def splitparam(s):
        o=s.split('=')
        if len(o) < 2:
            return (None,None)
        return (o[0].strip(),'='.join(o[1:]).strip())
    
    def parse_file(self):
        lines=self.fid.readlines()
        opt=0
        name=None
        for l in self.cleanlines(lines):
            if opt == 0:
                if l[0:10] == '='*10:
                    opt=1
            elif opt == 1:
                p,v=self.splitparam(l)
                if p != 'name':
                    raise Exception("Invalid Geogrid table")
                else:
                    name=v
                    self[name]={}
                    opt=2
            elif opt == 2:
                if l[0:10] == '='*10:
                    opt=1
                else:
                    p,v=self.splitparam(l)
                    if p is None:
                        raise Exception("Invalid Geogrid table at line:\n%s"%l)
                    if self[name].has_key(p):
                        self[name][p].append(v)
                    else:
                        self[name][p]=[v]
    
    def write_file(self,filename):
        f=open(filename,'w')
        for name,d in self.iteritems():
            f.write('='*30+'\n')
            f.write('name=%s\n'%name)
            for p,nv in d.iteritems():
                for v in nv:
                    f.write(' '*8+'%s=%s\n'%(p,v))
        f.write('='*30+'\n')
        f.close()
    
    def getSubgridFields(self):
        o=[]
        for n,d in self.iteritems():
            if d.has_key('subgrid') and d['subgrid'][0] == 'yes':
                o.append(n)
        return o

class WPSNamelist(Namelist):
    def getNDomains(self):
        return int(self['share']['par'][0]['max_dom'][0])
    
    def getSubgridRatio(self,idomain):
        return (int(self['share']['par'][0]['subgrid_ratio_x'][idomain]), \
                int(self['share']['par'][0]['subgrid_ratio_y'][idomain]))
    
    def getSubgridDomains(self):
        subdomains=[]
        for i in range(self.getNDomains()):
            srx,sry=self.getSubgridRatio(i)
            if srx >= 1 and sry >= 1:
                subdomains.append(i)
        return subdomains
    
    def getDomainSize(self,idomain):
        return (int(self['geogrid']['par'][0]['e_we'][idomain]),\
                int(self['geogrid']['par'][0]['e_sn'][idomain]))
    
    def getDomainBounds(self,idomain):
       return projBounds(self,idomain)
    
    def getGeogridTBL(self):
        try:
            p=self['geogrid']['par'][0]['opt_geogrid_tbl_path'][0]
        except:
            p='./geogrid'
        return os.path.join(p,'GEOGRID.TBL')
    
    