% function wrfout2frame3d(file,steps)
disp('1. file=''wrfout file''')
disp('2. steps=[1:20] % time steps to display')
disp('3. wrfout2frame3d')

fxlong =ncread(file,'FXLONG');
fxlat  =ncread(file,'FXLAT');
xlong  =ncread(file,'XLONG');
xlat   =ncread(file,'XLAT');
zsf    =ncread(file,'ZSF');
fgrnhfx=ncread(file,'FGRNHFX');
uf     =ncread(file,'UF');
vf     =ncread(file,'VF');
u      =ncread(file,'U');
v      =ncread(file,'V');
w      =ncread(file,'W');
ph     =ncread(file,'PH');
phb    =ncread(file,'PHB');
hgt    =ncread(file,'HGT');


% note the variables are read here without the WRF permutation of dimensions 

amin=[15,15,1];  % the atm grid part to show
amax=[30,30,2];
amin=[1,1,1];  % the atm grid part to show
amax=[41,41,2];

qstep=[20,20];        % quiver step for wind on the surface fire grid
astep=[2,2,1];      % quiver step for wind on the atmosphere grid
qs=1e-3;             % scaling for quiver arrows
swind=0;              % do not display surface wind

%-------------------------------------------------

for k=steps,
    frame3d(swind,amin,amax,astep,qstep,qs,...
    fxlong(:,:,k),fxlat(:,:,k),xlong(:,:,k),xlat(:,:,k),...
    zsf(:,:,k),fgrnhfx(:,:,k),uf(:,:,k),vf(:,:,k),...
    u(:,:,:,k),v(:,:,:,k),w(:,:,:,k),...
    ph(:,:,:,k),phb(:,:,:,k),hgt(:,:,k))
end