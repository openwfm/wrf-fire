function errmax=wind2flux_test
nx=50; ny=30; nz=10;
h=[rand,rand,1];
hh=rand(1,3);
% corner nodes
[x,y,z]   =ndgrid(hh(1)*[0:nx],hh(2)*[0:ny],hh(3)*[0:nz]);
% midpoints
[xm,ym,zm]=ndgrid(h(1)*([1:nx]-0.5),h(2)*([1:ny]-0.5),h(3)*([1:nz]-0.5));
X = {x,y,z};
% test field
% gradient for sizing
Usize = grad3z(xm,h,1);

disp('constant wind')
c=[rand,rand,rand];
for i=1:3,
    Uconst{i} = 0*Usize{i}+c(i);
end

errmax=0
testing_wind(Uconst)
fprintf('mesh size %g %g %g max error %g\n',nx,ny,nz,errmax)

function testing_wind(U)
        
    fl=wind2flux(U,X);
    d=div3(fl);
    disp('divergence zero except at the bottom')
    err=big(d(:,:,2:end))

    disp('terrain slope in x direction')
    disp('divergence zero except at the bottom')
    thx = 0.1*hh(1)*[0:nx]'*ones(1,ny+1);
    test_terrain(thx)

    disp('terrain slope in y direction')
    disp('divergence zero except at the bottom')
    thy = 0.1*hh(2)*ones(nx+1,1)*[0:ny]; 
    test_terrain(thy)

    disp('terrain slope in random constant direction')
    thxy = rand*thx + rand*thy;
    test_terrain(thxy)

    disp('roof slope in x direction')
    half = floor(nx/2);
    xs=hh(1)*[0:half,half-1:-1:2*half-nx];
    ys=ones(1,ny+1);
    th = 0.1*xs'*ys;
    test_terrain(th)

    disp('roof slope in y direction')
    half = floor(ny/2);
    xs=ones(1,nx+1);
    ys=hh(2)*[0:half,half-1:-1:2*half-ny];
    th = 0.1*xs'*ys;
    test_terrain(th)

    disp('pyramid roof slope')
    half = floor(ny/2);
     xs=[0:nx]; ys=[0:ny];
    [ii,jj]=ndgrid(xs,ys);
    th = nx+ny-abs(ii-nx/2)-abs(jj-ny/2);
    test_terrain(th)

    disp('random terrain')
    th = min(hh)*0.1*rand(size(X{1}(:,:,1)));
    test_terrain(th)

    function test_terrain(t)
        XX=add_terrain_to_mesh(X, t, 'shift');
        test_divergence
        XX=add_terrain_to_mesh(X, t, 'compress');
        test_divergence
        
        function test_divergence
            fl=wind2flux(U,XX);
            d=div3(fl);
            disp('divergence zero except at the bottom')
            err=big(d(:,:,2:end));
            fprintf('err=%g\n',err)
            errmax=max(errmax,err)
        end
    end

end

end





