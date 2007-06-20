function write_rsparse(a,name,num1,num2)
% function write_rsparse(a,name,num1,num2)
% write sparse matrix in compressed row format
if ~exist('num1','var'),
	num1=-1;
end
if ~exist('num2','var'),
	num2=-1;
end
write_csparse(a',name,num1,num2);

