function write_array_2d(filename,a)
% write_array_2d(filename,a)
% write 2d matrix as input to wrf-fire
% usage: write_array_2d('input_ht',ht)
[m,n]=size(a);
h=fopen(filename,'w');
fprintf(h,'%6i\n',m,n);
fprintf(h,'%12.6g\n',a');
fclose(h);
end

