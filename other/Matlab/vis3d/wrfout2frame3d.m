% function wrfout2frame3d(file,steps)
disp('1. file=''wrfout file''')
disp('2. readwrfout')
disp('3. steps=[1:20] % time steps to display')
disp('4. wrfout2frame3d')


% note the variables are read here without the WRF permutation of dimensions 

% hill
amin=[15,15,1];  % the atm grid part to show
amax=[30,30,2];
amin=[10,10,1];  % the atm grid part to show
amax=[30,30,1];
amin=[1,1,1]
amax=[19,31,1]

qstep=[20,20];        % quiver step for wind on the surface fire grid
astep=[2,2,1];        % quiver step for wind on the atmosphere grid
qs=1;                 % scaling for quiver arrows, 0=default

Harmanli
astep=[10,10,1];
qs=0.005;
amin=[1,1,1];
amax=[160,160,1];
swind=0;              % do not display surface wind

%-------------------------------------------------

a=avifile('fire.avi');
for k=steps,
    frame3d(swind,amin,amax,astep,qstep,qs,...
    fxlong(:,:,k),fxlat(:,:,k),xlong(:,:,k),xlat(:,:,k),...
    zsf(:,:,k),fgrnhfx(:,:,k),fuel_frac(:,:,k),uf(:,:,k),vf(:,:,k),...
    u(:,:,:,k),v(:,:,:,k),w(:,:,:,k),...
    ph(:,:,:,k),phb(:,:,:,k),hgt(:,:,k))
    title(t(k,:),'Interpreter','none')
    F=getframe(gcf);
    a=addframe(a,F); 
end
a=close(a);