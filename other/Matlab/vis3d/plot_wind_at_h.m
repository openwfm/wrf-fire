function plot_wind_at_h(p,iheights,levels,alpha,timestep,windscale,heightscale)
x=p.xlat;
y=p.xlong;
clf
if ~exist('timestep','var'),
    timestep=1;
end
if ~isscalar(timestep),
    error('timestep must be scalar')
end
hgt=p.hgt(:,:,timestep); % terrain height
% wind at given height
for i=iheights(:)'
    z=p.heights(i)*ones(size(x));
    ws=sqrt(p.uch(:,:,i,timestep).^2 + p.vch(:,:,i,timestep).^2)
    print_layer_msg(i,z,ws)
    surf(x,y,z+hgt,ws,'EdgeAlpha',alpha*0,'FaceAlpha',alpha)
    hold on
end
for i=levels(:)'
    z=p.altitude(:,:,i,timestep)-hgt;
    ws=sqrt(p.uc(:,:,i,timestep).^2 + p.vc(:,:,i,timestep).^2)
    print_layer_msg(i,z,ws)
    surf(x,y,z+hgt,ws,'EdgeAlpha',alpha*0,'FaceAlpha',alpha)
    hold on
end
caxis([0,windscale])
title(p.times,'Interpreter','none')
colorbar
axis('image')
a=[min(x(:)),max(x(:)),min(y(:)),max(y(:)),0,heightscale]; % var limits
axis(a);     % set var limits
xysize=max([a(4)-a(3),a(2)-a(1)]);
zsize=a(6)-a(5);
daspect([1,1,3*zsize/xysize]);
hold off, drawnow
end

function print_layer_msg(i,z,ws)
    fprintf('level %i height %g to %g median %g horizontal wind speed %g to %g median %g\n',...
       i,min(z(:)),max(z(:)),median(z(:)),min(ws(:)),max(ws(:)),median(ws(:)) )
end