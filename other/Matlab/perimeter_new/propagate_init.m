function [t,d]=propagate_init(tign,distance)
% [t,d]=propagate_init(tign,distance)
% create the initial state before the first call of propagate
[m,n]=size(tign);
t=zeros(m,n,3,3);
for i=1:m, 
    for j=1:n
        t(i,j,:,:)=tign(i,j);
    end
end
d=distance;
end