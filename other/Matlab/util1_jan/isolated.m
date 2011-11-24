function i=isolated(l,scheme)
if ~exist('scheme','var'),
  scheme=5;
end
i=l(2:end-1,2:end-1,:)<=0 & ...
  l(1:end-2,2:end-1,:)>0 & l(3:end,2:end-1,:)>0 & l(2:end-1,1:end-2,:) >0 & l(2:end-1,3:end,:)>0 ;
if scheme>5,
i=i & l(1:end-2,1:end-2,:)>0 & l(1:end-2,3:end,:) > 0 & l(3:end,1:end-2,:)>0 & l(3:end,3:end,:) >0;
end

