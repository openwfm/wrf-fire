function s=ssum(a)
% s=ssum(a)  
% sum of any array, arbitrary number of dimensions, ignoring any nans
s=sum(a(~isnan(a(:))));
end

