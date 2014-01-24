function h=plotmap(fig,x,y,z,s)
if fig~=0,
    figure(fig);
end
h=pcolor(x,y,z);
set(h,'EdgeAlpha',0,'FaceAlpha',1);
colorbar;
title(s);
end
