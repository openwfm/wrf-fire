function sf(f,u,v,l)
    [m,n]=size(f);
    %f(f==0)=NaN;
    dx=6;dy=6;
    clf,hold off
    s=20;
    ix=1:ceil(m/s):m;
    iy=1:ceil(n/s):n;
    x=([0:m-1])*dx;
    y=([0:n-1])*dy;
    xh=([0:m-1]+0.5)*dx;
    yh=([0:n-1]+0.5)*dy;
    [xx,yy]=ndgrid(x,y);
    sc=2*min(m,n)/s;
    hsc=1e5; % scale for flux labels
    quiver(xx(ix,iy),yy(ix,iy),sc*u(ix,iy),sc*v(ix,iy),0,'k')
    hold on
    %contour(y,x,l',[0 0],'y')
    h=pcolor(xh,yh,(1/hsc)*f');
    %h=surf(xh,yh,f');
    c=hot;
    cc=c;
    c(2:31,:)=cc(22:51,:);
    for i=32:64
        t=(i-32)/(64-32);
        c(i,:)=t*cc(64,:)+(1-t)*cc(52,:);
    end
    c(1,:)=0.45;
    colormap(c);
    if(0),
        hc=colorbar
        % title(hc,sprintf('%e J/m^2/s',hsc))
        title(hc,'10^5 J/m^2/s')
    end
    mx=4e5; % max color displayed
    caxis([0,mx/hsc]);
    hold on
    set(h,'edgecolor','none')
    %alpha(h,0.5)
    %colorbar
    xlabel('dist (m)')
    ylabel('dist (m)')
    zlabel('heat flux (J/m^2/s)')
    axis equal
    axis([0,m*dx,0,n*dy,0,mx])
    %axis tight
end
