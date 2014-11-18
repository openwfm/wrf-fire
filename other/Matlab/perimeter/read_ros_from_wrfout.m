function ros=read_ros_from_wrfout(wrfout,time)

% for witch its 3100 2600

format long

p=nc2struct(wrfout,{'F_ROS11','F_ROS12','F_ROS13','F_ROS21','F_ROS23','F_ROS31','F_ROS32','F_ROS33'},{},time);

ros=zeros(size(p.f_ros11,1),size(p.f_ros11,2),3,3);

ros(:,:,1,1)=p.f_ros11;

ros(:,:,1,2)=p.f_ros12;

ros(:,:,1,3)=p.f_ros13;

ros(:,:,3,1)=p.f_ros31;

ros(:,:,3,2)=p.f_ros32;

ros(:,:,3,3)=p.f_ros33;

ros(:,:,2,1)=p.f_ros21;

ros(:,:,2,3)=p.f_ros23;
end




function ros=read_all_data_from_wrfout(wrfout,time)

format long

f_ros11=ncread(wrfout,'F_ROS11');

f_ros12=ncread(wrfout,'F_ROS12');

f_ros13=ncread(wrfout,'F_ROS13');

f_ros21=ncread(wrfout,'F_ROS21');

f_ros23=ncread(wrfout,'F_ROS23');

f_ros31=ncread(wrfout,'F_ROS31');

f_ros32=ncread(wrfout,'F_ROS32');

f_ros33=ncread(wrfout,'F_ROS33');

ros=zeros(size(f_ros11,1),size(f_ros11,2),3,3,time);
ros(:,:,1,1,:)=f_ros11(:,:,1:time);

ros(:,:,1,2,:)=f_ros12(:,:,1:time);

ros(:,:,1,3,:)=f_ros13(:,:,1:time);

ros(:,:,3,1,:)=f_ros31(:,:,1:time);

ros(:,:,3,2,:)=f_ros32(:,:,1:time);

ros(:,:,3,3,:)=f_ros33(:,:,1:time);

ros(:,:,2,1,:)=f_ros21(:,:,1:time);

ros(:,:,2,3,:)=f_ros23(:,:,1:time);
end




      





