#!/usr/bin/env python

import sys
from SeamlessServer import SeamlessServer,ProductCode

class BaseDataDef(object):
    def __init__(self,servURL,prodCode,destVar,name,units,description,\
                      numCats=None,border=3,\
                      wordsize=2,signed=True,tilesize=100,scalefactor=1.,\
                      missingval=0.):
        self.servURL=servURL
        self.name=name
        self.baseprodCode=prodCode
        self.destVar=destVar
        self.units=units
        self.description=description
        self.numCats=numCats
        self.border=border
        self.wordsize=wordsize
        self.signed=signed
        self.tilesize=tilesize
        self.scalefactor=scalefactor
        self.missingval=missingval
        self.iscategorical=numCats != None
        self.serverClass=SeamlessServer(servURL)
        self.product=ProductCode(self.baseprodCode)
    
    def getConvertArgs(self):
        args=['-b %i'%self.border,\
              '-w %i'%self.wordsize,\
              '-t %i'%self.tilesize,\
              '-s %f'%self.scalefactor,\
              '-u "%s"'%self.units,\
              '-d "%s"'%self.description]
        if self.iscategorical:
            args.append('-c %i'%self.numCats)
        else:
            args.append('-m %f'%self.missingval)
        return args
    
    def getServer(self):
        return self.serverClass
    
    def getProductCode(self):
        return self.product
    
    def getDestVariable(self):
        return self.destVar
    
    def getSourceName(self):
        return self.name
    
    def getSourceDescription(self):
        return self.description

class AllData(dict):
    def read(self):
        try:
            import dataSources
        except:
            print >> sys.stderr, "Could not read data source descriptions from dataSources.py"
            raise
        
        for n,p in dataSources.__dict__.iteritems():
            if n[0] != '_' and type(p) == dict:
                self[n]=BaseDataDef(**p)
    
    def getDataDef(self,name):
        if self.has_key(name):
            return self[name]
        else:
            raise Exception("Could not find data source with name %s"%name)
