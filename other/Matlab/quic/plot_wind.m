function plot_wind(u3,h3)
m=[20,20,10];  % points to display in each direction
% interpolate to center points
u = 0.5*(u3{1}(1:end-1,:,:)+u3{1}(2:end,:,:));
v = 0.5*(u3{2}(:,1:end-1,:)+u3{2}(:,2:end,:));
w = 0.5*(u3{3}(:,:,1:end-1)+u3{3}(:,:,2:end));
n=size(u);
for i=1:3
    pos{i}=h3(i)*([1:n(i)]-0.5); % vector of positions in dimension i
    qi{i}=1+[0:m(i)-1]*(n(i)-1)/(m(i)-1);
end
[x,y,z]=ndgrid(pos{1},pos{2},pos{3});
[ix,iy,iz]=ndgrid(qi{1},qi{2},qi{3});
xd = interp3(x,ix,iy,iz);
yd = interp3(y,ix,iy,iz);
zd = interp3(z,ix,iy,iz);
ud = interp3(u,ix,iy,iz);
vd = interp3(v,ix,iy,iz);
wd = interp3(w,ix,iy,iz);

quiver3(xd,yd,zd,ud,vd,wd)
end