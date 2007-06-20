function a=read_csparse(name,num1,num2)
% function read_csparse(name,num1,num2)
% read sparse matrix in compressed column format
if ~exist('num1','var'),
	num1=-1;
end
if ~exist('num2','var'),
	num2=-1;
end
id=file_name([name,'_d'],num1,num2);
d=read_m(id);
id=file_name([name,'_x'],num1,num2);
x=read_m(id);
id=file_name([name,'_r'],num1,num2);
r=read_m(id);
id=file_name([name,'_v'],num1,num2);
v=read_m(id);
a=ijsparse(d,x,r,v);
