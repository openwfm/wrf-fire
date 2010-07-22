function write_array_2d(filename,a)
[m,n]=size(a);
h=fopen(filename,'w');
fprintf(h,'%6i\n',m,n);
fprintf(h,'%12.6g\n',a');
fclose(h);
end

