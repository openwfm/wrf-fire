function plot_ros(long,lat,ros)
% plot_ros(long,lat,ros)
% in:
%   long    east-west coordinates (m), size (m,n)
%   lat     south-north coordinates (m) size (m,n)
%   ros     rate of spread in 8 directions,from read_ros_from_wrfout
%           size (m,n,3,3)

% resample
step=20;

long_r=long(1:step:end,1:step:end);
lat_r=lat(1:step:end,1:step:end);
ros_r=ros(1:step:end,1:step:end,:,:);
m_r=size(ros_r,1);
n_r=size(ros_r,2);

d=zeros(8,2);
u_r=zeros(m_r,n_r);
v_r=zeros(m_r,n_r);
for i=1:m_r
    for j=1:n_r
        k=0;
        for a=1:3
            for b=1:3
                u=a-2;v=b-2;s=norm([u,v]);
                if s>0,
                    k=k+1;
                    u=u/s;v=v/s;
                    d(k,:)=[ros_r(i,j,a,b)*u,ros_r(i,j,a,b)*v];
                end
            end
        end
    % now d has the ends of ros vectors as rows
    center=mean(d);
    C=cov(d);
    [V,D]=eig(C);
    % components of the vector of max spread
    [lambda1,i1]=max(diag(D));   % max eigenvalue
    s1=sqrt(lambda1);            % singular value
    ax=V(:,i1);                 % the eigenvector
    vertex=(center+s1*ax'*sign(center*ax));  % vertex of the ellipse away from the center
    u_r(i,j)=vertex(1);
    v_r(i,j)=vertex(2);
    end    
end

% scale
dx=long_r(2:end,:)-long_r(1:end-1,:);dx=mean(dx(:))
dy=lat_r (2:end,:)-lat_r (1:end-1,:);dy=mean(dy(:))
maxros=max(sqrt((u_r(:).^2+v_r(:).^2)))
scaleros=min(dx,dy)/6;

% plot
clf
quiver(long_r,lat_r,scaleros*u_r,scaleros*v_r,'k')
%hold on
%pcolor(long,lat,mr); shading flat
%hold off
% drawnow
%colorbar
end