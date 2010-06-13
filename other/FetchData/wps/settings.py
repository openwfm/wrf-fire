#!/usr/bin/python
"""
General settings class to read/manipulate data from settings file
This class is meant to be subclassed to parse the settings file data, print the settings...
"""

__author__ = 'Stephane Chamberland (stephane.chamberland@ec.gc.ca)'
__version__ = '$Revision: 1.0 $'[11:-2]
__date__ = '$Date: 2006/08/31 21:16:24 $'
__copyright__ = 'Copyright (c) 2006 RPN'
__license__ = 'LGPL'

import sys
#sys.path.append("/usr/local/env/armnlib/modeles/SURF/python")

import re

from openanything import openAnything


class Settings(dict):
    """
    the dict is organised as
        dict[secName]['raw'] = ['sectionContent w/o comments, empty lines, lead/trail blanks']
        dict[secName]['par'][subSec#][parName] = [val1,val2...]
    """

    def __init__(self,settingFile):
        dict.__init__(self)
        self._setFile = settingFile
        self._setContent = openAnything(settingFile).read()
        self.update(self.parse())

    #==== Helper functions for Parsing of files
    def clean(self,mystringlist,commentexpr=r"^[\s\t]*\#.*$",spacemerge=0,cleancomma=0):
        """
        Remove leading and trailing blanks, comments/empty lines from a list of strings
        mystringlist = foo.clean(mystringlist,spacemerge=0,commentline=r"^[\s\t]*\#",cleancharlist="")
            commentline: definition of commentline
            spacemerge: if <>0, merge/collapse multi space
            cleancomma: Remove leading and trailing commas
        """
        aa = mystringlist
        if cleancomma:
            aa = [re.sub("(^([\s\t]*\,)+)|((\,[\s\t]*)+$)","",item).strip() for item in aa]
        if commentexpr:
            aa = [re.sub(commentexpr,"",item).strip() for item in aa]
        if spacemerge:
            aa = [re.sub("[\s\t]+"," ",item).strip() for item in aa if len(item.strip()) <> 0]
        else:
            aa = [item.strip() for item in aa if len(item.strip()) <> 0]
        return aa

    def splitstring(self,mystr):
        """
        Split a string in a list of strings at quote boundaries
            Input: String
            Output: list of strings
        """
        dquote=r'(^[^\"\']*)(\"[^"]*\")(.*)$'
        squote=r"(^[^\"\']*)(\'[^']*\')(.*$)"
        mystrarr = re.sub(dquote,r"\1\n\2\n\3",re.sub(squote,r"\1\n\2\n\3",mystr)).split("\n")
        #remove zerolenght items
        mystrarr = [item for item in mystrarr if len(item) <> 0]
        if len(mystrarr) > 1:
            mystrarr2 = []
            for item in mystrarr:
                mystrarr2.extend(self.splitstring(item))
            mystrarr = mystrarr2
        return mystrarr

    #==== Virtual function that derived class must implement
    def parse(self):
        """
        Virtual function that must be implemented by derived class

        Parse the file content in the self._setContent string
        Return a dictionary of the parsed setting file organized as:
        dict[secName]['raw'] = 'sectionContent w/o comments; w/ collapsed spaces'
        dict[secName]['par'][subSec#][parName] = [val1,val2...]
        """
        return { all: {'par': [] ,'raw': self.clean(self._setContent.split("\n"))}}


    #==== Output function
    def sec_string(self,secname):
        """
        Return a string containing the "cleaned" content of a sectionContent
        mysecstring = foo.sec_string(secname)
        """
        try:
            return "\n".join(self[secname]['raw'])
        except:
            return ''

    def param_singleval(self,secname,parname):
        aa = self.param_vallist(secname,parname)
        if aa[0]:
            return self.param_vallist(secname,parname)[0][0]
        else:
            return ''
        
    def param_vallist(self,secname,parname,subsec=-1):
        """
        Return a list of values for the specified section/param
        myvallist = foo.param_vallist(secname,parname,subsec=-1)
        if subsec is not specified, the list contain the values for param in each subsec
        """
        try:
            if subsec>=0:
                return self[secname]['par'][subsec][parname]
            else:
                return [item[parname] for item in self[secname]['par']]
        except:
            if subsec>=0:
                return []
            else:
                return [[]]

    def param_string(self,secname,parname,subsec=-1):
        """
        Return a string of comma separated list of values for the specified section/param
        myvalliststring = foo.param_string(secname,parname,subsec=-1)
        if subsec is not specified, the list contain the values for param in each subsec
        """
        try:
            return self.param_vallist(secname,parname,subsec).__repr__()[1:-1]
            #alternate way:
            #return "\n".join(self.param_vallist(secname,parname,subsec))
        except:
            return ''

if __name__ == '__main__':
    print __doc__
