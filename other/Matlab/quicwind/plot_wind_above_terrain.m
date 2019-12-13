function plot_wind_above_terrain(U,X,d)
% plot_wind(U,X,d)
%   U  cell array size 3 with the wind components
%   X  cell array size 3 with the mesh coordinates
%   d  vector size 3, query grid spacing     

if ~exist('d','var')
    d = h3(3)*(size(u3{1},3)-1)/3;
end

check_mesh(X);

clf, hold off
mesh(X{3}(:,:,1)); % terrain

[nx1,ny1,nz1]=size(X{1}); % mesh size
nx=nx1-1; ny=ny1-1; nz=nz1-1;

[ix,jx,kx]=meshgrid(1:nx1,1:ny1,1:nz1);           % grid indices of corners xy z
[iu,ju,ku]=meshgrid(1:nx1,0.5+[1:ny],0.5+[1:nz]); % grid indices of u
[iv,jv,kv]=meshgrid(0.5+[1:nx],1:ny1,0.5+[1:nz]); % grid indices of v
[iw,jw,kw]=meshgrid(0.5+[1:nx],0.5+[1:ny],1:nz1);  % grid indices of w

[iq,jq,kq]=ndgrid([1+d:d:nx],[1+d:d:ny],[1+d:d:nz]);  % query in index space
iq=iq(:);
jq=jq(:);
kq=kq(:);


% interpolate coordinates and vectors to query points
p=[2 1 3];
xd = interp3(ix,jx,kx,permute(X{1},p),iq,jq,kq);
yd = interp3(ix,jx,kx,permute(X{2},p),iq,jq,kq);
zd = interp3(ix,jx,kx,permute(X{3},p),iq,jq,kq);
ud = interp3(iu,ju,ku,permute(U{1},p),iq,jq,kq);
vd = interp3(iv,jv,kv,permute(U{2},p),iq,jq,kq);
wd = interp3(iw,jw,kw,permute(U{3},p),iq,jq,kq);

hold on 
quiver3(xd,yd,zd,ud,vd,wd)
hold off

axis equal
xlabel('x'),ylabel('y'),zlabel('z')
title('Wind field')
a=gcf;fprintf('Figure %i: wind field\n',a.Number)

end

