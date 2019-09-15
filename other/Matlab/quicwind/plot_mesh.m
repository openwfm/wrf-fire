function plot_mesh(X,bbox)
check_mesh(X);
if exist('bbox','var')
    for i=1:3
        X{i}=X{i}(bbox(1):bbox(2),bbox(3):bbox(4),bbox(5):bbox(6));
    end
end
[nx,ny,nz]=size(X{1});
nx=nx-1; ny=ny-1; nz=nz-1;
x=X{1};
y=X{2};
z=X{3};
clf
hold on
properties={'FaceColor', 'b', 'FaceAlpha', 0.05, 'EdgeColor','k','EdgeAlpha',1};
for ivar=1:3
    ivar
    for i=1:nx+(ivar==1)
        i
        for j=1:ny+(ivar==2)
            j
            for k=1:nz+(ivar==3)
                xx = vpatch3(x,i,j,k,ivar);
                yy = vpatch3(y,i,j,k,ivar);
                zz = vpatch3(z,i,j,k,ivar);
                % i,j,k,xx,yy,zz
                patch('Xdata', xx, 'YData', yy, 'ZData', zz, properties{:});
            end
        end
        drawnow
    end
end
hold off
end

function v=vpatch3(a,i,j,k,ivar)
switch ivar
    case 1
        v=[a(i,j,k),a(i,j+1,k),a(i,j+1,k+1),a(i,j,k+1)];
    case 2
        v=[a(i,j,k),a(i+1,j,k),a(i+1,j,k+1),a(i,j,k+1)];
    case 3
        v=[a(i,j,k),a(i+1,j,k),a(i+1,j+1,k),a(i,j+1,k)];
end
end