function write_array_2d(filename,a)
% write_array_2d(filename,a)
% Purpose: write 2d matrix as input to wrf-fire
%
% Arguments
% filename  string, the name of the file
% a         2d matrix, the array to be written
%
% Example: write_array_2d('input_ht',ht)
% Note: you can read the file back by a=read_array_2d(filename)
[m,n]=size(a);
h=fopen(filename,'w');
fprintf(h,'%6i\n',m,n);
fprintf(h,'%24.16g\n',a');
fclose(h);
end