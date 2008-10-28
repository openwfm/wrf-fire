function v=extract_sp(i,j,a,ii,jj)
% a=extract_sp(i,j,a,ii,jj)
% get s square submatrix (as dense) for index span ii jj
% from a sparse matrix (i,j,a)
% i, j do not need to be > 0

for k=1:length(a)
   iv=i(k);
   jv=j(k);
   ix= (ii==iv); 
   jx= (jj==jv);
   v(ix,jx)=a(k);
end
end
