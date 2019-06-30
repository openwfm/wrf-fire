disp('A simple block example: input wind speed is zero inside, uniform outside')
n=[500,500,50];
h=[2,2,1];
w=[1,1,1];
u0=grad3z(zeros(n),h);  % staggered mesh wind arrays
u0{3}(:,:,1)=0;       % zero vertical speed on the ground
u0{1}(:,:,:)=1;       % unit homogeneous speed in direction 1
u0{1}(200:300,200:300,1:20)=0; % except zero in a block
u0{2}(:,:,:)=0;
u = mass_cons_int(u0,h,w,'check');
plot_wind(u,h,5)
disp('horizontal velocity component about the middle of the leading edge of the block')
squeeze(u{1}(197:203,250,25:-1:1))'
disp('note small inside and speedup above the top of the block')
disp('vertical velocity component about the middle of the leading edge of the block')
squeeze(u{3}(197:203,250,25:-1:1))'
disp('note positive and zero at ground level')
disp('vertical velocity component about the middle of the trailing edge of the block')
squeeze(u{3}(297:303,250,25:-1:1))'
disp('note negative and zero at ground level')