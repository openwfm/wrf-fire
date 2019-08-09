function plot_wind(u3,h3,d)
% plot_wind(u3,h3,s)
%   u3  cell array size 3 with the wind components
%   h3  vector of mesh spacing (m)
%   d   (optional) distance between points to display, in all 3 directions     

if ~exist('d','var')
    d = h3(3)*(size(u3{1},3)-1)/3;
end

% interpolate to center points
u = 0.5*(u3{1}(1:end-1,:,:)+u3{1}(2:end,:,:));
v = 0.5*(u3{2}(:,1:end-1,:)+u3{2}(:,2:end,:));
w = 0.5*(u3{3}(:,:,1:end-1)+u3{3}(:,:,2:end));

% dimension of the center point array
n=size(u);

for i=1:3
    c{i}=h3(i)*(1/2 + [1:n(i)]);     % coordinates of s
    l(i) = h3(i)*(n(i)-1);           % length of the dimension
    p{i} = [d/2 : d : l(i)];         % display points in physical space [0, h3(i)*(n(i)-1)]
    q{i} = 1+p{i}/h3(i);             % display points in index space [1,n(i)]
end


[x, y ,z ] = ndgrid(c{1},c{2},c{3});
[xd,yd,zd] = ndgrid(p{1},p{2},p{3});
[iq,jq,kq] = ndgrid(q{1},q{2},q{3});

ud = interp3(u,iq,jq,kq);
vd = interp3(v,iq,jq,kq);
wd = interp3(w,iq,jq,kq);

quiver3(xd,yd,zd,ud,vd,wd,0.5)

disp('for some reason, streamlines do not work')
%hold on
%[startx,starty,startz]=ndgrid(p{1}(1),p{2}(1),p{3}(1));
%streamline(x,y,z,u,v,w,startx,starty,startz)
%hold off

axis([0,l(1),0,l(2),0,l(3)])
axis equal
xlabel('m'),ylabel('m'),zlabel('m')
title('Wind field')
a=gcf;fprintf('Figure %i: wind field\n',a.Number)
mid=round(size(xd,2)/2);
figure
quiver(xd(:,mid,:),zd(:,mid,:),ud(:,mid,:),vd(:,mid,:),0.5)
axis([0,l(1),0,l(3)])
axis equal
xlabel('m'),ylabel('m'),zlabel('m')
title('Wind field at middle crossection')
a=gcf;fprintf('Figure %i: wind field middle crossection\n',a.Number)


end