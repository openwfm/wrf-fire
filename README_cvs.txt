Parts of this file tree are on git branch jm and on cvs project wrf. If there
is a discrepancy between the git version and the cvs version, the git version
is authoritative.

The git branch jm should contain only files that go into the wrf build.
The cvs repository should only files that are needed to develop the fire model
and run it independently in ./wrfv2_fire/phys, standalone testers for those
files, documentation, and some wrf files of interest to look at only.

See doc/README_git.txt how to access from git. 
To access from cvs, 
setenv CVSROOT ml-dddas.csr.uky.edu:/u/wf/cvsroot 
cvs checkout wrf

See README.txt how to compile and run the fire model with wrf.

To compile standalone and run tests on the new fire model, do

cd wrfv2_fire/phys
make -f Makefile_test X_test 

where X is one of util core prop fuel model

Unless you really know what you are doing do not use the same directory
structure for both git and cvs access.

Jan Mandel, September 4, 2007
