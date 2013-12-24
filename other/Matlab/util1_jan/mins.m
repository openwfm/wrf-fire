function m=mins(varargin)
% minimum of variable number of arrays
n=length(varargin);
if n==0,
    m=Inf;
    return
end
m=varargin{1};
for i=2:n
    m=min(m,varargin{i});
end
end
    