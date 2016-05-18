disp('testing propagation forward')
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
fire_area=ones(m,n);
fire_mask=ones(m,n);
fire_area(2:5,6:9)=0;
time_end=20;
[t,d]=propagate_init(tign,distance);
[t,d]=propagate(t,d,1,fire_area,fire_mask,distance,ros,time_end,0);
tign1=t(:,:,2,2)
[t,d]=propagate_init(tign,distance);
mx=10;
for i=1:mx,
    time_end_part=time_end*i/mx
    [t,d]=propagate(t,d,1,fire_area,fire_mask,distance,ros,time_end_part,0);
end
tign2=t(:,:,2,2)
err=norm(tign1-tign2,1)

disp('testing propagation back')
dir=-1;
time_end=-8;
tign=100*ones(m,n); tign(:,1)=1; tign(:,end)=1; tign(1,:)=1;tign(end,:)=1;
tign=dir*tign+2;
tign
[t,d]=propagate_init(tign,distance);
[t,d]=propagate(t,d,dir,fire_area,fire_mask,distance,ros,time_end,0);
tign1=t(:,:,2,2)
[t,d]=propagate_init(tign,distance);
mx=10;
for i=1:mx,
    time_end_part=time_end*i/mx
    [t,d]=propagate(t,d,dir,fire_area,fire_mask,distance,ros,time_end_part,0);
end
tign2=t(:,:,2,2)
err=norm(tign1-tign2,1)

disp('testing propagation from fire_area both ways')
[i,j]=ndgrid(1:m,1:n);
fire_area=(i-6).^2+(j-6).^2<10;
time_now=1;
time_end=30
[tign,fire_mask_out,fire_mask_in]=initial_tign(fire_area,time_now,time_end)
disp('out')
[t,d]=propagate_init(tign,distance);
[t,d]=propagate(t,d,1,~fire_area,fire_mask_out,distance,ros,max(tign(:)),2);
tign=t(:,:,2,2)
disp('in')
[t,d]=propagate_init(tign,distance);
[t,d]=propagate(t,d,-1,fire_area,fire_mask_in,distance,ros,min(tign(:)),0);
tign=t(:,:,2,2)


