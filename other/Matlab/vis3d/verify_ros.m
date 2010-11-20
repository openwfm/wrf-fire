function err=check_ros(fuel)
% err=check_ros(fuel)
nfuels=length(fuel);
for k=1:nfuels
    nwinds=length(fuel(k).wind);
    for j=1:nwinds
        speed=fuel(k).wind(j);
        ros=fire_ros(fuel(k),speed,0);
        err(k,j,1)=fuel(k).ros_wind(j)-ros;
    end
    nslopes=length(fuel(k).slope);
    for j=1:nslopes
        tanphi=fuel(k).slope(j);
        ros=fire_ros(fuel(k),0,tanphi);
        err(k,j,2)=fuel(k).ros_slope(j)-ros;
    end
end