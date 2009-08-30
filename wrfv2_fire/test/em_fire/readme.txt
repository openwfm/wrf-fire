The fire test problems are in subdirectories. If you want to make your own
test case subdirectory, all you need to do is create soft links for ideal.exe
and wrf.exe pointing the the parent directory, and create the files
namelist.input and input_sounding (best by modifying a copy from another 
subdirectory).

Do not just copy one of the existing subdirectories, the soft links may not 
survive that.

