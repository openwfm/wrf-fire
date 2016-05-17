m=4;
n=4;
ros=ones(m,n,3,3)*0.5;
t=20*ones(m,n,3,3);
t(2,2,:,:)=1;
d=0*t;

for a=1:3,
    for b=1:3,
        d(:,:,a,b)=norm([a-2,b-2]);
    end
end
t_now=6;

propagate
