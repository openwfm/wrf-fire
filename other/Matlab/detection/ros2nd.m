% convert ros stored in a structure to 3d
function t=ros2nd(tt)
t.lfn=tt.lfn;
t.tign_g=tt.tign_g;
t.times=tt.times;
t.timestr=char(tt.times');
[m,n]=size(tt.f_ros11);
t.ros=zeros(m,n,3,3);
t.ros(:,:,1,1)=tt.f_ros11;
t.ros(:,:,1,2)=tt.f_ros12;
t.ros(:,:,1,3)=tt.f_ros13;
t.ros(:,:,2,1)=tt.f_ros21;
t.ros(:,:,2,3)=tt.f_ros23;
t.ros(:,:,3,1)=tt.f_ros31;
t.ros(:,:,3,2)=tt.f_ros32;
t.ros(:,:,3,3)=tt.f_ros33;
