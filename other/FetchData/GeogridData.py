#!/usr/bin/env python

import os
import shutil
import subprocess as sp
from wps.WPSNamelist import WPSNamelist,GeogridTBL
from SeamlessServer import *

convertBinary='../WPSGeoTiff/convert_geotiff.x'
dataCacheFile='cachedData.txt'

def getDataProduct(serv,prod,bounds,datadir):
    return(serv.getData(prod,bounds,datadir))

dataDict={'NFUEL_CAT':[lfServer,nfuelProduct],'ZSF':[nedServer,nedProduct]}

def dataKey(prod,bounds):
    return "%s:%s" %(prod,bounds.URLfmt())

def writeDataKey(prod,bounds,datadir):
    filename=dataCacheFile
    datakey=dataKey(prod,bounds)
    f=open(filename,'a')
    f.write("%s@%s\n"%(datakey,os.path.abspath(datadir)))
    f.close()

def getCachedData(prod,bounds):
    filename=dataCacheFile
    try:
        f=open(filename,'r')
    except IOError:
        return ''
    lines=f.readlines()
    cdict={}
    k=dataKey(prod,bounds)
    for l in lines:
        p=l.split('@')
        if p[0] == k and len(p) == 2:
            return p[1].strip('\n')
    return ''
    

def getSubgridData(tbl='./geogrid/GEOGRID.TBL',nml='./namelist.wps'):
    t=GeogridTBL(tbl)
    n=WPSNamelist(nml)
    o={}
    for idomain in n.getSubgridDomains():
        datadir='srcdata_%02i'%idomain+1
        bds=n.getDomainBounds(idomain)
        bounds=LatLonBounds(*bds)
        for name in t.getSubgridFields():
            if not o.has_key(name):
                o[name]=[]
            serv=dataDict[name][0]
            prod=dataDict[name][1]
            cd=getCachedData(prod,bounds)
            if os.access(cd,os.R_OK):
                o[name].append(cd)
            else:
                fdesc=getDataProduct(serv,prod,bounds,datadir)
                o[name].append(fdesc[0])
                writeDataKey(prod,bounds,fdesc[0])
    return o

def convertData(data,path='.'):
    for d in data:
        for i in range(len(data[d])):
            installPath=os.path.join(path,"%s_%02i"%(d,i+1))
            try:
                os.remove(installPath)
            except:
                pass
            try:
                shutil.rmtree(installPath,ignore_errors=True)
            except:
                pass
            os.makedirs(installPath)
            if not callConvertUtil(d,data[d][i],installPath):
                raise Exception("%s returned non-zero status."%convertBinary)

def getConvertArgs(name):
    return([])

def callConvertUtil(name,srcFile,destDir):
    args=getConvertArgs(name)
    p=sp.Popen(args,executable=os.path.abspath(convertBinary),\
               stdout=sp.PIPE,stderr=sp.PIPE,stdin=sp.PIPE,\
               cwd=os.path.abspath(destDir))
    p.communicate()
    if p.returncode != 0:
        return False
    else:
        return True