#!/usr/bin/python
"""
    Manipulates Fortran Namelists

    Defines
        Namelist class

    TODO: == As a Stand alone program ==
    Print info about Fortran Namelists

    Usage: namelist.py -f FILENAME [-n NAMELIST [-p PARNAME]]

        FILENAME: path/name of config file
        NAMELIST: namelist name (since a file may contain many namelists)
        PARNAME: Name of parameter to print value for

        if NAMELIST is not provided,
            print the list of namelist present in file in list form:
                ['name1','name2',...]
        if NAMELIST is provided (PARNAME not provided),
            print all namelist param sin a "param=value" format, one per line
            for multiple values of same param, print each one
            as "param=value" format, one per line
        if NAMELIST and PARNAME is provided,
                if not specify, print all param in a "param=value" format
                if specify and exist, print PARNAME's value only
                for multiple values of same param, print each PARNAME's
                values, one per line

        This script is a generic Fortan namelist parser
        and will recognize all namelist in a file with the following format,
        and ignores the rest.

        &namelistname
            opt1 = value1
            ...
        /
"""

__author__ = 'Stephane Chamberland (stephane.chamberland@ec.gc.ca)'
__version__ = '$Revision: 1.0 $'[11:-2]
__date__ = '$Date: 2006/09/05 21:16:24 $'
__copyright__ = 'Copyright (c) 2006 RPN'
__license__ = 'LGPL'

import sys
#sys.path.append("/usr/local/env/armnlib/modeles/SURF/python")

import re

from settings import Settings

# import sys
# import getopt
# import string

class Namelist(Settings):
    """
    Namelist class
    Scan a Fortran Namelist file and put Section/Parameters into a dictionary

    Intentiation:

        foo = Namelist(NamelistFile)

        where NamelistFile can be a filename, an URL or a string

    Functions:
        [Pending]

        This is a generic Fortan namelist parser
        it will recognize all namelist in a file with the following format,
        and ignores the rest.

        &namelistname
            opt1 = value1
            ...
        /
    """

    def parse(self):
        """Config file parser, called from the class initialization"""
        varname   = r'\b[a-zA-Z][a-zA-Z0-9_]*\b'
        valueInt  = re.compile(r'[+-]?[0-9]+')
        valueReal = re.compile(r'[+-]?([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)')
        valueNumber = re.compile(r'\b(([\+\-]?[0-9]+)?\.)?[0-9]*([eE][-+]?[0-9]+)?')
        valueBool = re.compile(r"(\.(true|false|t|f)\.)",re.I)
        valueTrue = re.compile(r"(\.(true|t)\.)",re.I)
        spaces = r'[\s\t]*'
        quote = re.compile(r"[\s\t]*[\'\"]")

        namelistname = re.compile(r"^[\s\t]*&(" + varname + r")[\s\t]*$")
        paramname = re.compile(r"[\s\t]*(" + varname+r')[\s\t]*=[\s\t]*')
        namlistend = re.compile(r"^" + spaces + r"/" + spaces + r"$")

        #split sections/namelists
        mynmlfile  = {}
        mynmlname  = ''
        for item in self.clean(self._setContent.split("\n"),cleancomma=1):
            if re.match(namelistname,item):
                mynmlname = re.sub(namelistname,r"\1",item)
                mynmlfile[mynmlname] = {
                    'raw' : [],
                    'par' : [{}]
                    }
            elif re.match(namlistend,item):
                mynmlname = ''
            else:
                if mynmlname:
                    mynmlfile[mynmlname]['raw'].append(item)

        #parse param in each section/namelist
        for mynmlname in mynmlfile.keys():
            #split strings
            bb = []
            for item in mynmlfile[mynmlname]['raw']:
                bb.extend(self.splitstring(item))
            #split comma and =
            aa = []
            for item in bb:
                if not re.match(quote,item):
                    aa.extend(re.sub(r"[\s\t]*=",r" =\n",re.sub(r",+",r"\n",item)).split("\n"))
                else:
                    aa.append(item)
            del(bb)
            aa = self.clean(aa,cleancomma=1)

            myparname  = ''
            for item in aa:
                if re.search(paramname,item):
                    myparname  = re.sub(paramname,r"\1",item).lower()
                    mynmlfile[mynmlname]['par'][0][myparname] = []
                elif paramname:
                    #removed quotes, spaces (then how to distinguish .t. of ".t."?)
                    if re.match(valueBool,item):
                        if re.match(valueTrue,item):
                            mynmlfile[mynmlname]['par'][0][myparname].append('.true.')
                        else:
                            mynmlfile[mynmlname]['par'][0][myparname].append('.false.')
                    else:
                        mynmlfile[mynmlname]['par'][0][myparname].append(re.sub(r"(^[\'\"]|[\'\"]$)",r"",item.strip()).strip())
        return mynmlfile

