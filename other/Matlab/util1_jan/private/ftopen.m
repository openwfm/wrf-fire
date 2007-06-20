function fid=ftopen(file)
% open fortran unformatted file with proper endian
endian(1).par='native';
endian(2).par='ieee-le';
endian(3).par='ieee-be';
endian(4).par='ieee-be.l64';
endian(5).par='ieee-be.l64';
mode=[];
for i=1:length(endian);
	fid=fopen(file,'r',endian(i).par);
   if fid<0, 
      fprintf('cannot open file %s\n',file),
      return
   end
   j=fread(fid,1,'int');
   % fprintf('first word in %s mode is %i\n',endian(i).par,j)
   fclose(fid);
   if abs(j)<2^15,
      mode=endian(i).par;
      break, 
   end
end
if length(mode)==0, 
   fprintf(1,'cannot find mode to open file %s\n',file),
   fid=-1;
   return
end
fid=fopen(file,'r',mode);
fprintf('FORTRAN file %s mode %s\n',file,mode);
return
