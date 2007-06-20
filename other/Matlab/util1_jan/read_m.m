function a=read_m(name,num1,num2)
% a=read_m(name,num1,num2)
% a=read_m(name,num1)
% a=read_m(name)
%
% read array from file created by call write_array subroutines with this name num1 num2
% Jan Mandel, 2005

% the mode of reading may change; do not call the functions below directly
if ~exist('num1','var'),
	num1=-1;
end
if ~exist('num2','var'),
	num2=-1;
end
name=file_name(name,num1,num2);
a=read_array(name);
 
