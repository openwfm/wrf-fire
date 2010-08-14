function plot_wind_at_h(p,ilevels,ilayers,alpha,timestep)
x=p.xlat;
y=p.xlong;
clf
if ~exist('timestep','var'),
    timestep=1;
end
hgt=p.hgt(:,:,timestep); % terrain height
% wind at given height
for i=ilevels(:)'
    z=p.levels(i)*ones(size(x));
    ws=sqrt(p.uch(:,:,i,timestep).^2 + p.vch(:,:,i,timestep).^2);
    print_layer_msg(i,z,ws)
    surf(x,y,z+hgt,ws,'EdgeAlpha',alpha*0,'FaceAlpha',alpha)
    hold on, drawnow
end
for i=ilayers(:)'
    z=p.altitude(:,:,i,timestep)-hgt;
    ws=sqrt(p.uch(:,:,i,timestep).^2 + p.vch(:,:,i,timestep).^2);
    print_layer_msg(i,z,ws)
    surf(x,y,z+hgt,ws,'EdgeAlpha',alpha*0,'FaceAlpha',alpha)
    hold on, drawnow
end
colorbar
hold off
end

function print_layer_msg(i,z,ws)
    fprintf('level %i height %g to %g median %g horizontal wind speed %g to %g median %g\n',...
       i,min(z(:)),max(z(:)),median(z(:)),min(ws(:)),max(ws(:)),median(ws(:)) )
end