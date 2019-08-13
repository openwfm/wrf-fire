function err = mass_cons_flux_test
disp('mass_cons_flux_test - testing mass consistent flux approximation with terrain')
n=[20,30,5];
h = rand(1,3);
w = rand(1,3);
lambda0=zeros(n);
u0= grad3z(rand(n),'zero at bottom');
warning('not ready')
err=0
return
[u,err] = mass_cons_flux(u0,h,w,'check'); 
end