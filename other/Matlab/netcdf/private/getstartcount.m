function [s,c]=getstartcount(varinfo)
s=zeros(varinfo.ndims,1);
c=s;
for i=1:varinfo.ndims
   c(i)=varinfo.dimlength(i); 
end
end