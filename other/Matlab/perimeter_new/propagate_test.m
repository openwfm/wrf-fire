
m=10;
n=12;
ros=ones(m,n,3,3)*0.5;
tign=100*ones(m,n);
tign(2,2)=1;
distance=0*ros;
for a=1:3,
    for b=1:3,
        distance(:,:,a,b)=norm([a-2,b-2]);
    end
end
time_now=50;
fire_area=ones(m,n);
fire_area(4:6,8:11)=0;
tign=propagate(tign,1,fire_area,distance,ros,time_now)
tign=100*ones(m,n); tign(:,1)=1; tign(:,end)=1; tign(1,:)=1;tign(end,:)=1;
tign=propagate(tign,1,fire_area,distance,ros,time_now)
