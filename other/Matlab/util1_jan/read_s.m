function a=read_s(name,num1,num2)
% read sparse matrix from file created by call write_array subroutines with this name num1 num2
% the mode of reading may change; do not call the functions below directly
if ~exist('num1','var'),
	num1=-1;
end
if ~exist('num2','var'),
	num2=-1;
end
% read the pieces
filename=file_name([name,'_sz'],num1,num2);
nn=read_array(filename);
filename=file_name([name,'_ia'],num1,num2);
ia=read_array(filename);
filename=file_name([name,'_ja'],num1,num2);
ja=read_array(filename);
filename=file_name([name,'_da'],num1,num2);
da=read_array(filename);
% create the sparse magtrix
a=sparse(ia,ja,da,nn(1),nn(2));
