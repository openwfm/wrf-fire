% 1. set file name: file='wrfout file'
% 2. set the time steps to display: steps=[1:20]
% 3. edit outvars2frame3d to set the mesh section to visualize,
% uncomment capture to video, etc.
% 4. run runvis3d 
% 5. to repeat without the legthy reading of the wrfout file, run just
% outvars2frame3d

wrfout2outvars  % read wrfout into memory

outvars2frame3d % display 
