function err = mass_cons_flux_test
disp('mass_cons_flux_test - testing mass consistent flux approximation with terrain')
nx=7; ny=7; nz=4;

hh=rand(1,3);
[x,y,z]   = ndgrid(hh(1)*[0:nx],hh(2)*[0:ny],hh(3)*[0:nz]);
X={x,y,z};
X=add_terrain_to_mesh(X,'hill','shift')

U0= grad3z(ones(size(x)-1),[1 1 1]);
err=0;
[u,err] = mass_cons_flux(U0,X,'check'); 
end