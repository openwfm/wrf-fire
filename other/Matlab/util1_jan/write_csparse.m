function write_csparse(a,name,num1,num2)
% function write_csparse(a,name,num1,num2)
% write sparse matrix in compressed column format
[d,x,r,v]=csparse(a);
if ~exist('num1','var'),
	num1=-1;
end
if ~exist('num2','var'),
	num2=-1;
end
id=file_name([name,'_d'],num1,num2);
write_m(d,id);
id=file_name([name,'_x'],num1,num2);
write_m(x,id);
id=file_name([name,'_r'],num1,num2);
write_m(r,id);
id=file_name([name,'_v'],num1,num2);
write_m(v,id);

