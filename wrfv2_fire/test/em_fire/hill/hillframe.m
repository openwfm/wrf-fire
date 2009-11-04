disp('1. install mexnc under the same directory as wrf-fire, then')
disp('2. start matlab in wrf/WRFV3/test/em_fire to set up the paths properly')
disp('   or cd there and run startup while in the directory')
disp('3. read a wrfrst file into the workspace as in')
disp('   >> ncload wrfrst_d01_0001-01-01_00:01:00')

% note the variables are read here without the WRF permutation of dimensions 
ideal=1;
rst=1;

if rst,
    u=u_2;
    v=v_2;
    w=w_2;
    ph = ph_2;
end

clf
amin=[15,15,1];  % the atm grid part to show
amax=[30,30,2];
amin=[1,1,1];  % the atm grid part to show
amax=[41,41,2];

qstep=[5,5];        % quiver step for wind on the surface fire grid
astep=[1,1,1];      % quiver step for wind on the atmosphere grid
qs=1.5;             % scaling for quiver arrows
r=[10,10];          % refinement factor
swind=1;            % plot surface wind

%-------------------------------------------------

frame3d(swind,amin,amax,astep,qstep,qs,...
    fxlong,fxlat,xlong,xlat,zsf,fgrnhfx,uf,vf,u,v,w,ph,phb,hgt)