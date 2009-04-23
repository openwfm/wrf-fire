'''
Created on Apr 22, 2009

@author: jbeezley
'''

import textwrap

global verbose
verbose=False
def setverbose(l=True):
    global verbose
    verbose=l

def verbprint(str):
    global verbose
    if verbose:
        print textwrap.fill(str,80)
        
def verblog():
    global verbose
    return verbose