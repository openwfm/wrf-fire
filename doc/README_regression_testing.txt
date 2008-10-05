Regression testing protocol
===========================

The purpose of regression testing is to make sure that the output of the code
did not change from previous runs on a database of examples and in particular
that bugs that were fixed previously did not reappear (=the code did not regress).

Set a baseline before changing anything. After the change, compare against the baseline.
If the change was supposed to change numbers, verify the science and set up new baseline.
If the change caused rounding error differences only, set up a new baseline.
If there was any change notify other developers in particular Jan.

1. in wrfv2_fire: pull, configure with debug, single processor, compile_fire, 
   cd test/em_fire, edit namelist.input to set fire_print_file=1 (do NOT commit
   such edited file!), wrf.exe >& wrf.log, let run until some wrfrst* files are 
   produced, ideally 800+ time steps. This will create about 5GB of .txt files.
   If the .txt files were not created, make sure all module_fr_sfire_*.F source files
   have #define DEBUG_OUT at the beginning (those where this string appears later)

2. to set up a baseline: ./moveto  will create directory ~/tmp/<current commit number>
   and move the wrf/fire output file there
   
   or

3. to check current commit against baseline: compareto ~/tmp/<commit number>

4. Compile with other options (OpenMP, DM, various combinations of number of processes
   and threads), run until some wrfrst* files are produced, make sure the results are 
   the same up to rounding error as in the corresponding wrfrst* files in
   the baseline. You can use ncdiff in matlab for that.  The proper path in matlab
   is set by starting matlab in test/em_fire or test/em_real, or execute startup.m there.
   Note wrf/other/Matlab/netcdf/mexnc must be installed, see README_matlab_netcdf.txt

5. in matlab, type go in the directory with all those *.txt files which should produce
   a movie, check for any weirdness and artefacts such as fire propagation stops and 
   zigzag instabilities of the fireline.

6. in matlab, load LFN from the wrfrst* files and check for any weirdness such as positive
   or negative spikes at the boundary. These were past bugs and should not reappear. Some
   of these may show up in OpenMP or DM runs only. (But you checked that the parallel
   and serial results are the same up to rounding already, right?)

In case of any difference find the first commit where the results deviate and either fix or
justify that this is actually correct and establish a new baseline.

Please note in the commit message if the results are to be different because of a change
in science or a bug fix. 

To get the current commit number you can use  git log | head -1

The commit number captures the exact state of all files in the project so the
results associated with a commit are reproducible. You can get all files as of a given
commit by git checkout <commit number>

Note that bugs can reappear by changing namelist.input esp. the method parameters at the end.


Jan Mandel, October 6, 2008
 



