format compact
% get coordinates
load c
m=c(1,1);
n=c(1,2);
c(1,:)=[];
c1=c(:,1);
c2=c(:,2);
c1=reshape(c1,m,n);
c2=reshape(c2,m,n);
for i=1:2
    y1=1+100*rand;
    y2=1+200*rand;
    [x1,x2]=bint2_inv(c1,c2,bint2(c1,y1,y2),bint2(c2,y1,y2));
    err=[y1-x1,y2-x2]
end
