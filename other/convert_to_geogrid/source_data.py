'''
Created on Apr 21, 2009

@author: jbeezley
'''
import sys
from optparse import OptionParser, OptionGroup
import numpy as np

try:
    from osgeo import gdal
except ImportError:
    try:
        import gdal
    except ImportError:
        print "Cannot import GDAL python modules"
        print "Are they installed? Is the install directory in your"
        print "PYTHONPATH?"
        raise
try:
    from osgeo import osr
except ImportError:
    try:
        import osr
    except ImportError:
        print "Cannot import OSR (part of GDAL) python modules"
        print "Are they installed? Is the install directory in your"
        print "PYTHONPATH?"
        raise

from print_control import verbprint, verblog, setverbose

class Source:
    '''
    classdocs
    '''

    class FileInfo:
        """A class holding information about a GDAL file."""
    
        def __init__(self, filename):
            """
            Initialize file_info from filename

            """

            verbprint("Opening file: "+filename)
            fg=gdal.Open(filename)
            fh=fg
            if fg is None:
                raise IOError("file: "+filename+" cannot be found or opened by GDAL")
    
            self.filename = filename
            self.driver = fg.GetDriver().LongName
            self.sdriver = fg.GetDriver().ShortName
            self.bands = fh.RasterCount
            self.xsize = fh.RasterXSize
            self.ysize = fh.RasterYSize
            self.band_type = fh.GetRasterBand(1).DataType
            self.projection = fh.GetProjection()
            self.geotransform = fh.GetGeoTransform()
            self.spatialreference=osr.SpatialReference(self.projection)
            sr=self.spatialreference
            self.ct=osr.CoordinateTransformation(sr,sr.CloneGeogCS())
            #print self.ct.TransformPoint(self.geotransform[0],self.geotransform[3])
            ul = self.ct.TransformPoint(self.geotransform[0],self.geotransform[3])
            (self.ulx,self.uly) = (ul[0],ul[1])
            self.xres = abs(self.geotransform[1])
            self.yres = abs(self.geotransform[5])
            lr = self.ct.TransformPoint(self.ulx + self.geotransform[1] * self.xsize,
                                        self.uly + self.geotransform[5] * self.ysize)
            (self.lrx,self.lry) = (lr[0],lr[1]) 
            self.topbottom=(self.geotransform[5]<0)
            self.description=fg.GetDescription()
    
            ct = fh.GetRasterBand(1).GetRasterColorTable()
            if ct is not None:
                self.ct = ct.Clone()
            else:
                self.ct = None

            self.check()
            verbprint(filename+" opened with driver: ")
            verbprint(" "*4+self.driver+" ("+self.sdriver+")")
            if verblog():
                self.report()
    
        def report(self):
            print 'Filename: ' + self.filename
            print 'File Size: %d x %d x %d' \
                  % (self.xsize, self.ysize, self.bands)
            print 'Pixel Size: %f x %f' \
                  % (self.xres, self.yres)
            print 'UL:(%f,%f)   LR:(%f,%f)' \
                  % (self.ulx, self.uly, self.lrx, self.lry)
                  
        def check(self):
            if self.bands != 1:
                raise Exception("Files with more than one band are not supported")


    def __init__(self,filenames):
        '''
        Constructor
        '''
        self.files=[]
        self.source_tiles=[]
        for f in filenames:
            self.files.append(Source.FileInfo(f))
        self.mosaic()
        self.check()
        self.eps=min(self.files[0].xres,self.files[0].yres)/100.
        
    def getsize(self):
        if len(self.source_tiles) > 1:
            bndx,bndy=[(x[0][1]+1,x[1][1]+1) for x in self.source_tiles]
            return max(bndx),max(bndy)
        else:
            return self.source_tiles[0][0][1]+1,self.source_tiles[0][1][1]+1
        
    
    def gettile(self,xstart,xend,ystart,yend,missing):
        # need to fix this!!!!
        # gdal doesn't give this information for some reason
        # this is for NED data
        sourcemissing=-9999
        
        a=np.empty((yend-ystart+1,xend-xstart+1))
        a.fill(float(missing))
        for i in range(len(self.files)):
            f=self.source_tiles[i]
            ff=self.files[i]
            if ( f[0][0] <= xend or f[0][1] >= xstart ) and \
               ( f[1][0] <= yend or f[1][1] >= ystart ):
                txstart=max(f[0][0]+1,xstart)-1  # convert to zero based index
                sxstart=txstart-f[0][0]
                txstart=txstart-xstart+1
                txend=min(f[0][1]+1,xend)
                sxend=txend-f[0][0]
                txend=txend-xstart+1
                tystart=max(f[1][0]+1,ystart)-1  # convert to zero based index
                systart=tystart-f[1][0]
                tystart=tystart-ystart+1
                tyend=min(f[1][1]+1,yend)
                syend=tyend-f[1][0]
                tyend=tyend-ystart+1
                fd=gdal.Open(ff.filename)
                verbprint("fetching from %s: x: %i-%i, y: %i-%i" %
                          (ff.filename,sxstart+1,sxend,systart+1,syend))
                verbprint("to tile: x: %i-%i, y: %i-%i" % 
                          (txstart+1,txend,tystart+1,tyend))
                xsize=sxend-sxstart
                ysize=syend-systart
                #print fd.ReadAsArray(sxstart,systart,xsize,ysize).shape
                a[tystart:tyend,txstart:txend]=\
                fd.ReadAsArray(sxstart,systart,xsize,ysize)#.transpose()
        # don't know if this actually works
        a=np.choose(np.equal(a,sourcemissing),(a,missing))
        return a

    def getprojection(self):
        if self.files[0].spatialreference.IsGeographic():
            return None
        else:
            sr=self.files[0].spatialreference
            ret={}
            ret['projection']=sr.GetAttrValue("projection")
            if sr.GetAttrValue("unit") == "metre":
                ret['unit']="meter"
            else:
                ret['unit']=sr.GetAttrValue("unit")
            ret['datum']=sr.GetAttrValue('datum')
            ret['stdlon']=sr.GetProjParm("longitude_of_center")
            ret['stdlat']=sr.GetProjParm("latitude_of_center")
            ret['truelat1']=sr.GetProjParm("standard_parallel_1")
            ret['truelat2']=sr.GetProjParm("standard_parallel_2")
            return ret

    def gettopbottom(self):
        return self.files[0].topbottom
    
    def getfirstcoord(self):
        return self.files[0].ulx,self.files[0].uly
    
    def getdescription(self):
        return self.files[0].description
    
    def getresolution(self):
        return (self.files[0].xres,self.files[0].yres)
        
    def check(self):
        if self.files is None or len(self.files) <= 0:
            raise Exception("No files have been imported!")
        comp=[(x.xres,x.yres,x.bands,x.projection,x.band_type) for x in self.files]
        [ x == comp[0] for x in comp ]
        if not any(comp):
            print "Input files must be tiles of a single data set.  This module"
            print "cannot do interpolation.  In order to use these files, first"
            print "merge them into a single data set with dgal_merge.py, distributed"
            print "with the GDAL python bindings."
            raise Exception("Files are not compatible.")
        
            
    def mosaic(self):
        f=self.files
        rx=f[0].xres
        ry=f[0].yres
        if len(f) == 1:
            self.source_tiles=[((0,f[0].xsize-1),(0,f[0].ysize-1))]
            return
        
        raise Exception("multiple input files not yet supported")
        up=[0]*len(f)
        down=up
        left=up
        right=up
        for i in range(len(f)):
            for j in range(i,len(f)):
                if abs(f[i].ulx - f[j].lrx) - resx < self.eps:
                    left[i]=j
                    right[j]=i
                elif abs(f[i].lrx - f[j].ulx) - resx < self.eps:
                    right[i]=j
                    left[j]=i
                elif abs(f[i].uly - f[j].lry) - resy < self.eps:
                    up[i]=j
                    down[j]=i
                elif abs(f[i].lry - f[j].uly) - resy < self.eps:
                    down[i]=j
                    up[j]=i

def source_option_parser(parser):
    pass   
                    
if __name__ == '__main__':
    setverbose()
    s=Source(['/Users/jbeezley/Downloads/data/24/w001001.adf'])
    a=s.gettile(1,5,1,10)
    print a
    print a.shape
