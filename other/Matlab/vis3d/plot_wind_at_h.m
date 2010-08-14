function plot_wind_at_h(p,ilevels,ilayers,alpha,timestep)
x=p.xlat;
y=p.xlong;
clf
if ~exist('timestep','var'),
    timestep=1;
end
% wind at given height
for i=ilevels(:)'
    z=p.levels(i)*ones(size(x));
    ws=sqrt(p.uch(:,:,i,timestep).^2 + p.vch(:,:,i,timestep).^2);
    surf(x,y,z,ws,'EdgeAlpha',alpha*0,'FaceAlpha',alpha)
    hold on, drawnow
end
for i=ilayers(:)'
    z=p.altitude(:,:,i,timestep);
    ws=sqrt(p.uch(:,:,i,timestep).^2 + p.vch(:,:,i,timestep).^2);
    surf(x,y,z,ws,'EdgeAlpha',alpha*0,'FaceAlpha',alpha)
    hold on, drawnow
end
colorbar
hold off
end
