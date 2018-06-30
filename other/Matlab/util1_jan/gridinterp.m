function vi=gridinterp(x,y,v,xi,yi)

if any(size(xi)~=size(yi)),
    error('xi and yi must be same size')
end
[mi,ni]=size(xi);

[mm,nn]=size(x);
[ii,jj]=ndgrid(1:mm,1:nn);

vi=zeros(mi,ni);
disp('interpolating')
for i=1:mi
    for j=1:ni
        xx=xi(i,j);
        yy=yi(i,j);
        if ~isnan(xx) & ~isnan(yy),
            % find the nearest point
            d2=(x-xx).*(x-xx)+(y-yy).*(y-yy);
            k=find(d2(:)==min(d2(:)));k=k(1);
            ix=ii(k);
            iy=jj(k);
            if ix>1 & ix < mm & iy >1 & iy < nn,
                x3=x(ix-1:ix+1,iy-1:iy+1);
                y3=y(ix-1:ix+1,iy-1:iy+1);
                v3=v(ix-1:ix+1,iy-1:iy+1);
                v_interp = scatteredInterpolant(x3(:),y3(:),v3(:));
                vv = v_interp(xx,yy);
                % disp(x3);disp(xx);disp(y3),disp(yy),disp(v3),disp(vv)
                % hold off; mesh(x3,y3,v3);hold on;plot3(xx,yy,vv,'*k');hold off;drawnow
            else
                vv = NaN;
            end
        else
            vv=NaN;
        end
        vi(i,j)=vv;
    end
end
end