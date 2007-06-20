function a=read_rsparse(name,num1,num2)
% function read_rsparse(name,num1,num2)
% read sparse matrix in compressed row format
if ~exist('num1','var'),
	num1=-1;
end
if ~exist('num2','var'),
	num2=-1;
end
a=read_csparse(name,num1,num2)';
