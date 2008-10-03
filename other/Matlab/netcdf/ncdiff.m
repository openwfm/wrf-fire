function [relerr,ssq,p]=ncdiff(file1,file2,var)
% [relerr,stdev,p]=ncdiff(file1,file2,'var')
% compare variable var in 2 netcdf files
% relerr = max relative difference
% ssq = square mean relative difference
% p = p-value, close to +1 -1 means errors significantly in one direction
% for rounding errors expect relerr=const*eps, p small
v1=ncread(file1,var);
v2=ncread(file2,var);
d=(v2(:)-v1(:))/max(big(v1),big(v2)); % scaled diff
avgdiff=mean(d);
ssq=std(d);
n=length(v1(:));
t=sqrt(n)*avgdiff/(ssq+eps);
p=erf(t);
relerr=big(d);
fprintf('relative error max %g  ssq mean %g p-value %g\n',relerr,ssq,p) 
end
