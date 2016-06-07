function fire_pixels(obs,base_time,varargin)
% fire_pixels(obs,base_time)
% fire_pixels(obs,base_time,dim)
if length(varargin)>0,
    dim=varargin{1};
end
for i=1:length(obs)
    x=obs(i);
    kk=find(x.data(:)>=7);
    if ~isempty(kk),
        rlon=0.5*abs(x.lon(end)-x.lon(1))/(length(x.lon)-1);
        rlat=0.5*abs(x.lat(end)-x.lat(1))/(length(x.lat)-1);
        lon1=x.xlon(kk)-rlon;
        lon2=x.xlon(kk)+rlon;
        lat1=x.xlat(kk)-rlat;
        lat2=x.xlat(kk)+rlat;
        X=[lon1,lon2,lon2,lon1]';
        switch dim
        case 3 
            Y=[lat1,lat1,lat2,lat2]';
            Z=ones(size(X))*(x.time-base_time);
            cmap=cmapmod14;
            C=cmap(x.data(kk)'+1,:);
            C=reshape(C,length(kk),1,3);
            patch(X,Y,Z,C);
        otherwise
            error('unknown argument dim')
        end
        hold on
    end
end
hold off
end