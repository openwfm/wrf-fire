function i=isolated(l,d)
% return the location of all isolated elements where l<0
if ~exist('d','var'),
  disp('default 5 point scheme') 
  i=l(2:end-1,2:end-1,:)<=0 & ...
  l(1:end-2,2:end-1,:)>0 & l(3:end,2:end-1,:)>0 & l(2:end-1,1:end-2,:) >0 & l(2:end-1,3:end,:)>0 ;
else
  fprintf('looking within distance %g ...',d)
  i=l(1+d:end-d,1+d:end-d,:) <= 0;
  for ii=-d:d,
    for jj=-d:d,
       if ii ~= 0 | jj ~= 0,
          fprintf(' %i %i',ii,jj)
          i=i & l(1+d+ii:end-d+ii,1+d+jj:end-d+jj,:) > 0;
       end
    end
  end
  fprintf('\n')
end
end

