function [relerr,ssq]=ncdiff(file1,file2,var)
% [relerr,stdev,p]=ncdiff(file1,file2[,'var'])
% compare variable var in 2 netcdf files
% if var is missing, compare all variables
% relerr = max relative difference
% ssq = square mean relative difference
% p = p-value, close to +1 -1 means errors significantly in one direction
% for rounding errors expect relerr=const*eps, p small
if ~exist('var','var'),
    v=ncdump(file1,'-q');
    for i=1:length(v),
        var=v(i).varname;
        [r(i),s(i)]=ncdiff(file1,file2,var);
    end
    reless=max(r);
    ssq=max(s);
    return
end
v1=ncread(file1,var);
v2=ncread(file2,var);
d=(v2(:)-v1(:))/(max(big(v1),big(v2))+realmin); % scaled diff
avgdiff=mean(d);
ssq=std(d);
n=length(v1(:));
t=sqrt(n)*avgdiff/(ssq+realmin);
p=erf(t);
relerr=big(d);
fprintf('relative error max %g min %g ssq %g avg diff %g t-stats %g p-value %g\n',...
    max(d),min(d),ssq,avgdiff,t,p) 
end
