function write_m(a,name,num1,num2)
% write_m(a,name,num1,num2)
% a         array
% name      root for file number
% num1,num2 numbers to form file name name_num1_num2
%           if either num1<0 or num2<0 it is not used
% write array to file same as created by call write_array subroutines 
% the mode of reading may change; do not call the functions below directly
% this file can be read by function read_m
if ~exist('num1','var'),
	num1=-1;
end
if ~exist('num2','var'),
	num2=-1;
end
name=file_name(name,num1,num2);
write_array(a,name);
 
