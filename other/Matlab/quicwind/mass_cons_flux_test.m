function err = mass_cons_flux_test
disp('mass_cons_flux_test - testing mass consistent flux approximation with terrain')
nx=5; ny=3; nz=2;

hh=rand(1,3);
[x,y,z]   = ndgrid(hh(1)*[0:nx],hh(2)*[0:ny],hh(3)*[0:nz]);
X={x,y,z};
X=add_terrain_to_mesh(X,'hill','shift')

disp('direct method'0

lambda0=zeros(n);
u0= grad3z(rand(n),'zero at bottom');
warning('not ready')
err=0
return
[u,err] = mass_cons_flux(u0,h,w,'check'); 
end